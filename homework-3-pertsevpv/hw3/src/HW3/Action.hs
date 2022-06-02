module HW3.Action where

import Data.Set.Internal (Set, member)

import HW3.Base(HiMonad(..), HiAction(..), HiValue(..))

import Control.Exception.Base (Exception, throwIO)

import Control.Monad.IO.Class
import Data.Sequence.Internal(fromList)
import System.Directory (getCurrentDirectory, createDirectory, setCurrentDirectory, doesFileExist, doesDirectoryExist, listDirectory)
import Data.Text (pack, unpack)
import Data.Time.Clock (getCurrentTime)
import Data.ByteString(readFile, writeFile)
import Data.Text.Encoding(decodeUtf8')
import System.Random

-- | permissions for actions
data HiPermission 
  = AllowRead   -- ^ permission to read
  | AllowWrite  -- ^ permission to write
  | AllowTime   -- ^ permission to get time
  deriving (Show, Eq,  Ord)

-- | exceptions for actions
data PermissionException 
  = PermissionRequired HiPermission -- ^ absence of necessary permission
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

-- | Monad instance for HIO
instance Monad HIO where
  m >>= k = HIO $ \r -> do
    a <- runHIO m r
    runHIO (k a) r

-- | Functor instance for HIO
instance Functor HIO where
  fmap f m = HIO $ fmap f . runHIO m
    
-- | Applicative instance for HIO
instance Applicative HIO where
  pure = liftHIO . pure
  f <*> v = HIO $ \ r -> runHIO f r <*> runHIO v r
      
-- | HiMonad instance for HIO
instance HiMonad HIO where
  runAction HiActionCwd = do
    perm <- ask
    if member AllowRead perm then do
      dir <- liftIO getCurrentDirectory
      return $ HiValueString (pack dir)
    else liftIO $ throwIO (PermissionRequired AllowRead)
    
  runAction (HiActionRead path) = do
    perm <- ask
    if member AllowRead perm then do
      isFile <- liftIO $ doesFileExist path
      isDir <- liftIO $ doesDirectoryExist path
      if isFile then do
        bytes <- liftIO $ Data.ByteString.readFile path
        let decoded = decodeUtf8' bytes
        case decoded of 
          (Left _) -> return $ HiValueBytes bytes
          (Right utf) -> return $ HiValueString utf
      else if isDir then do
        list <- liftIO $ listDirectory path
        return $ HiValueList $ fromList (map (HiValueString . pack) list)
      else return $ HiValueString mempty      
    else liftIO $ throwIO (PermissionRequired AllowRead)

  runAction (HiActionWrite path bytes) = do
    perm <- ask
    if member AllowWrite perm then do
      liftIO $ Data.ByteString.writeFile path bytes
      return HiValueNull
    else liftIO $ throwIO (PermissionRequired AllowWrite)
  
  runAction (HiActionMkDir dir) = do
    perm <- ask
    if member AllowWrite perm then do
      liftIO $ createDirectory dir
      return HiValueNull
    else liftIO $ throwIO (PermissionRequired AllowWrite)
  
  runAction (HiActionChDir dir) = do
    perm <- ask
    if member AllowRead perm then do
     liftIO $ setCurrentDirectory dir
     return HiValueNull
    else liftIO $ throwIO (PermissionRequired AllowRead)

  runAction HiActionNow = do
    perm <- ask
    if member AllowTime perm then do
     utc <- liftIO getCurrentTime
     return $ HiValueTime utc
    else liftIO $ throwIO (PermissionRequired AllowTime)
  
  runAction (HiActionRand l r) = do
    g <- newStdGen
    let (ran, _) = uniformR (l, r) g
    return $ HiValueNumber (fromIntegral ran)
    
  runAction (HiActionEcho text) = do
    perm <- ask
    if member AllowWrite perm then do
      liftIO $ putStrLn $ Data.Text.unpack text
      return HiValueNull
    else liftIO $ throwIO (PermissionRequired AllowWrite)
    
instance MonadIO HIO where
  liftIO = liftHIO . liftIO
  
ask :: HIO (Set HiPermission)
ask = HIO return

liftHIO :: IO a -> HIO a
liftHIO m = HIO (const m)