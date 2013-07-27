{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE EmptyDataDecls    #-}
import           Control.Arrow
import           Control.Arrow.ArrowTree (getChildren, multi)
import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Control.Monad (filterM, foldM, liftM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Data.Tree.NTree.TypeDefs (NTree)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Network.HTTP (getResponseBody, simpleHTTP)
import           Network.HTTP.Base (mkRequest, RequestMethod( GET ))
import           Network.URI (parseURI) 
import           Text.Feed.Import
import           Text.Feed.Types (Feed)
import           Text.Feed.Query (getFeedItems, getItemLink)
import           Text.XML.HXT.Arrow.ReadDocument (readString)
import           Text.XML.HXT.Arrow.XmlArrow (ArrowXml, getText, hasName)
import           Text.XML.HXT.Arrow.XmlState (IOSArrow, no, runX, withParseHTML, withWarnings, yes)
import           Text.XML.HXT.DOM.TypeDefs (XmlTree, XNode)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Curator
  name String
  url String
  feed String
  deriving Show
Article
  url String
  title String
  content String
  curatorId CuratorId
  deriving Show
|]


--getFeeds :: MonadIO m => ConnectionPool -> m [Curator]
--getFeeds = \pool -> liftIO $ flip runSqlPersistMPool pool $ do
  ---- curatorIds :: [Entity val]
  --curatorIds <- selectList [] [Asc CuratorName] 
  --let curatorGenerics = map entityVal curatorIds
  --let curators = map (\x -> x :: Curator) curatorGenerics
  --return curators

getFeeds :: SqlPersistM [Curator]
getFeeds = do
  curatorIds <- selectList [] [Asc CuratorName] 
  let curatorGenerics = map entityVal curatorIds
  let curators = map (\x -> x :: Curator) curatorGenerics
  return curators

-- Stolen from http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html
openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u -> liftIO(getResponseBody =<< simpleHTTP (mkRequest GET u))

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

-- might want to update for xml feeds.
getUrl :: String -> IO (IOSArrow XmlTree (NTree XNode))
getUrl url = do 
    contents <- runMaybeT $ openUrl url
    return $ readString [withParseHTML no, withWarnings no] (fromMaybe "" contents)

getUrlAsString :: String -> IO String
getUrlAsString url = do
    contents <- runMaybeT $ openUrl url
    return $ fromMaybe "" contents

getFeedFromUrl :: String -> MaybeT IO Feed
getFeedFromUrl url = do 
    contents <- openUrl url
    MaybeT $ return $ parseFeedString contents

getLinksFromFeed :: String -> MaybeT IO [String]
getLinksFromFeed url =  do
    feed <- getFeedFromUrl url
    let items = getFeedItems feed
    return $ catMaybes $ map getItemLink items

getLinksForCurator :: Curator -> MaybeT IO [String]
getLinksForCurator (Curator _ _ feedUrl) = 
    getLinksFromFeed feedUrl

getLinksForCurators :: [Curator] -> MaybeT IO [String]
getLinksForCurators curators = foldM app [] curators
    where app :: [String] -> Curator -> MaybeT IO [String]
          app links curator = do
              curatorLinks <- getLinksForCurator curator
              return $ links ++ curatorLinks

connectionString = "host=localhost dbname=artrank user=weirdcanada password=weirdcanada port=5432"

main  :: IO ()
main = withPostgresqlPool connectionString 10 $ \pool -> do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll

    weirdCanadaId <- insert $ Curator "Weird Canada" "weirdcanada.com" "http://weirdcanada.com/feed"
    silentShoutId <- insert $ Curator "Silent Shout" "silentshout.ca" "http://silentshout.ca/feed"

    oneWeirdCurator <- selectList [CuratorName ==. "Weird Canada"] [LimitTo 1]
    
    -- links <- runMaybeT $ getLinksForCurator (oneWeirdCurator :: Curator)

    curators <- getFeeds

    liftIO $ print curators
    --liftIO $ print (oneWeirdCurator :: [Entity Curator])

