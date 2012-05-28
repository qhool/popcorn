module Popcorn (PopcornInterface (..),
                PluginInfo (..),
                loadPlugins,
                loadPluginsWithOpts,
                pluginStatusStr) where

import Control.Monad
import Data.List
import System.Plugins.Make
import System.Plugins.Load
import System.Directory
import System.FilePath

data PopcornInterface = PopcornInterface { title :: String,
                                           about :: String }

data PluginInfo = PluginInfo { name :: String, 
                               source :: FilePath, 
                               makeStat :: MakeStatus,
                               loadStat :: LoadStatus PopcornInterface }
                  
spName :: PluginInfo -> String -> PluginInfo
spName (PluginInfo n s m l) n' = PluginInfo n' s m l

spSource :: PluginInfo -> FilePath -> PluginInfo
spSource (PluginInfo n s m l) s' = PluginInfo n s' m l

spMakeStat :: PluginInfo -> MakeStatus -> PluginInfo
spMakeStat (PluginInfo n s m l) m' = PluginInfo n s m' l

spLoadStat :: PluginInfo -> LoadStatus PopcornInterface -> PluginInfo
spLoadStat (PluginInfo n s m l) l' = PluginInfo n s m l'


getDirectoriesContents :: [FilePath] -> IO [FilePath]
getDirectoriesContents dirs = fmap concat $ 
                              (filterM doesDirectoryExist dirs) >>= (sequence.(map conts_d)) where 
  add_d :: FilePath -> [FilePath] -> [FilePath] 
  add_d d fs = map (combine d) fs
  conts_d :: FilePath -> IO [FilePath]
  conts_d d = fmap (add_d d) $ getDirectoryContents d 
           
filesAndDirs :: [FilePath] -> IO ([FilePath],[FilePath])
filesAndDirs ps = do
    tups <- sequence $ map fileordir ps
    --now we have [([FilePath],[FilePath])]
    return $ foldr (\(fa,da) (fs,ds)->(fa++fs,da++ds)) ([],[]) tups
  where
  fileordir :: FilePath -> IO ([FilePath],[FilePath])
  fileordir p = do
    filep <- doesFileExist p
    dirp <- doesDirectoryExist p
    return ( if filep then [p] else [],
             if dirp then [p] else [] )
      
      
           
getPlugins :: [FilePath] -> IO [PluginInfo]
getPlugins dirs = do
  (files,dirs) <- getDirectoriesContents dirs >>= filesAndDirs
  dirPlugs <- filterM doesFileExist $ map (`combine` "plugin.hs") dirs
  return $ map (\(n,f) -> PluginInfo {name=n, source=f, makeStat=MakeFailure [], loadStat=LoadFailure [] }) $
    ( map (\f -> (takeBaseName f,f)) $ filter ((==".hs").snd.splitExtension) files ) ++
      ( map (\f -> (takeBaseName $ takeDirectory f, f) ) dirPlugs )
  
buildPlugins :: [PluginInfo] -> [String] -> IO [PluginInfo]
buildPlugins plugs args = sequence $ map bp plugs
  where
    bp :: PluginInfo -> IO PluginInfo
    bp p = do
      stat <- makeAll (source p) args
      return (spMakeStat p stat)
      
loadPlugin :: PluginInfo -> [FilePath] -> IO PluginInfo
loadPlugin p paths = lp (makeStat p) p where
  lp (MakeFailure errs) p = return p
  lp (MakeSuccess code obj) p = do
    stat <- load obj paths [] "getInterface"
    return (spLoadStat p stat)
  
loadPluginsWithOpts :: [FilePath] -> [String] -> [FilePath] -> IO [PluginInfo]
loadPluginsWithOpts extra_dirs args paths = getPlugins ( extra_dirs ++ [ "~/.popcorn/plugins", 
                                                                         "/etc/popcorn/plugins",
                                                                         "/usr/lib/popcorn/plugins",
                                                                         "/usr/local/lib/popcorn/plugins",
                                                                         "/usr/share/popcorn/plugins" ] )
                         >>= (`buildPlugins` args) >>= (sequence.(map (`loadPlugin` paths)))
                                            
loadPlugins :: IO [PluginInfo]
loadPlugins = loadPluginsWithOpts [] [] []

pluginStatusStr :: PluginInfo -> String
pluginStatusStr p = pss (makeStat p) (loadStat p) where 
  pss (MakeFailure errs) _ = concat $ intersperse "\n" $ ((name p)++": Make failed:"):errs
  pss _ (LoadFailure errs) = concat $ intersperse "\n" $ ((name p)++": Load failed:"):errs
  pss _ (LoadSuccess _ _) = (name p)++": Loaded."
  --pss _ _ = (name p)++": Unknown."
  
--showPluginsStatus = do
--  plugs <- getPlugins [ "./plugins" ] >>= buildPlugins >>= (sequence.(map loadPlugin))
--  sequence $ map (putStrLn.pluginStatusStr) $ plugs