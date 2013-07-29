{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE EmptyDataDecls    #-}
import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Control.Monad (filterM, foldM, liftM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.Lazy (runWriterT, tell, WriterT, writer)
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
import           System.FilePath

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
openUrl :: String -> MaybeT (WriterT [String] IO) String
openUrl url = case parseURI url of
    Nothing -> lift $ writer (fail "", ["Could not parse URI: " ++ url])
    Just u -> lift $ liftIO(getResponseBody =<< simpleHTTP (mkRequest GET u))

getFeedFromUrl :: String -> MaybeT (WriterT [String] IO) Feed
getFeedFromUrl url = do 
    contents <- openUrl url
    lift $ tell ["Opened Url: " ++ url]
    let result = parseFeedString contents
    lift $ case result of
        Nothing -> tell ["Failed to parse URL: " ++ url]
        Just x -> tell ["Succesfully parsed URL: " ++ url]
    MaybeT $ return $ parseFeedString contents

getLinksFromFeed :: String -> MaybeT (WriterT [String] IO) [String]
getLinksFromFeed url =  do
    feed <- getFeedFromUrl url
    let items = getFeedItems feed
    return $ catMaybes $ map getItemLink items

getLinksForCurator :: Curator -> MaybeT (WriterT [String] IO) [String]
getLinksForCurator (Curator _ _ feedUrl) = 
    getLinksFromFeed feedUrl


getLinksForCurators :: [Curator] -> MaybeT (WriterT [String] IO) [String]
getLinksForCurators curators = foldM app [] curators
    where app :: [String] -> Curator -> MaybeT (WriterT [String] IO) [String]
          app links curator = lift $ do
              maybeNewLinks <- runMaybeT $ getLinksForCurator curator
              case maybeNewLinks of
                  Nothing -> writer (links, ["Unable to add new links for curator: " ++ (show curator)])
                  Just newLinks -> writer (links ++ newLinks, [])
          
              --return $ links ++ curatorLinks

connectionString = "host=localhost dbname=artrank user=weirdcanada password=weirdcanada port=5432"

main  :: IO ()
main = withPostgresqlPool connectionString 10 $ \pool -> do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll

    --weirdCanadaId <- insert $ Curator "Weird Canada" "weirdcanada.com" "http://weirdcanada.com/feed"
    --silentShoutId <- insert $ Curator "Silent Shout" "silentshout.ca" "http://silentshout.ca/feed"

    oneWeirdCurator <- selectList [CuratorName ==. "Weird Canada"] [LimitTo 1]
    
    -- links <- runMaybeT $ getLinksForCurator (oneWeirdCurator :: Curator)

    --curators <- getFeeds
    let curators = [Curator "Weird Canada" "weirdcanada.com" "http://pitchfork.com/rss/reviews/albums/"]


    (links,logs) <- liftIO $ runWriterT $ runMaybeT $ getLinksForCurators curators
    liftIO $ print links
    liftIO $ print logs

    liftIO $ print curators
    --liftIO $ print (oneWeirdCurator :: [Entity Curator])
