{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import qualified Data.ByteString.Char8 as Byte
import Data.Function ((&))
import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix

import System.Fuse (FuseOperations(..))
import qualified System.Fuse as Fuse

type Handler a = IO (Either Fuse.Errno a)

helloString :: Byte.ByteString
helloString = Byte.pack "Hello Fuse!\n"

rootPath :: FilePath
rootPath = "/"

helloPath :: FilePath
helloPath = "/hello"

dirStat :: Fuse.FuseContext -> Fuse.FileStat
dirStat ctx =
    let
        statEntryType = Fuse.Directory
        statFileMode =
            foldr1 Posix.unionFileModes
                [ Posix.ownerReadMode
                , Posix.ownerExecuteMode
                , Posix.groupReadMode
                , Posix.groupExecuteMode
                , Posix.otherReadMode
                , Posix.otherExecuteMode
                ]
        statLinkCount = 2
        statFileOwner = Fuse.fuseCtxUserID ctx
        statFileGroup = Fuse.fuseCtxGroupID ctx
        statSpecialDeviceID = 0
        statFileSize = 4096
        statBlocks = 1
        statAccessTime = 0
        statModificationTime = 0
        statStatusChangeTime = 0
    in
    Fuse.FileStat { .. }

fileStat :: Fuse.FuseContext -> Fuse.FileStat
fileStat ctx =
    let
        statEntryType = Fuse.RegularFile
        statFileMode =
            foldr1 Posix.unionFileModes
                [ Posix.ownerReadMode
                , Posix.groupReadMode
                , Posix.otherReadMode
                ]
        statLinkCount = 1
        statFileOwner = Fuse.fuseCtxUserID ctx
        statFileGroup = Fuse.fuseCtxGroupID ctx
        statSpecialDeviceID = 0
        statFileSize = fromIntegral $ Byte.length helloString
        statBlocks = 1
        statAccessTime = 0
        statModificationTime = 0
        statStatusChangeTime = 0
    in
    Fuse.FileStat { .. }

helloGetFileStat :: FilePath -> Handler Fuse.FileStat
helloGetFileStat path
    | path == rootPath =
        Right . dirStat <$> Fuse.getFuseContext

    | path == helloPath =
        Right . fileStat <$> Fuse.getFuseContext

    | otherwise =
        return $ Left Fuse.eNOENT

helloOpen :: FilePath -> Fuse.OpenMode -> Fuse.OpenFileFlags -> Handler ()
helloOpen path mode _flags
    | path == helloPath =
        case mode of
            Fuse.ReadOnly ->
                return $ Right ()

            _ ->
                return $ Left Fuse.eACCES

    | otherwise =
        return $ Left Fuse.eNOENT

helloRead :: FilePath -> () -> Posix.ByteCount -> Posix.FileOffset -> Handler Byte.ByteString
helloRead path _ byteCount offset
    | path == helloPath =
        helloString
            & Byte.drop (fromIntegral offset)
            & Byte.take (fromIntegral byteCount)
            & Right
            & return

    | otherwise =
        return $ Left Fuse.eNOENT

helloOpenDirectory :: Monad m => String -> m Fuse.Errno
helloOpenDirectory path
    | path == rootPath =
        return Fuse.eOK

    | otherwise =
        return Fuse.eNOENT

helloReadDirectory :: FilePath -> Handler [(FilePath, Fuse.FileStat)]
helloReadDirectory path
    | path == rootPath = do
        let (_ : helloName) = helloPath
        ctx <- Fuse.getFuseContext
        return $ Right
            [ (".", dirStat ctx)
            , ("..", dirStat ctx)
            , (helloName, fileStat ctx)
            ]

    | otherwise =
        return $ Left Fuse.eNOENT

helloGetFileSystemStats :: String -> Handler Fuse.FileSystemStats
helloGetFileSystemStats _ =
    return $ Right $ Fuse.FileSystemStats
        { fsStatBlockSize = 512
        , fsStatBlockCount = 1
        , fsStatBlocksFree = 1
        , fsStatBlocksAvailable = 1
        , fsStatFileCount = 5
        , fsStatFilesFree = 10
        , fsStatMaxNameLength = 255
        }

helloFSOps :: Fuse.FuseOperations ()
helloFSOps =
    Fuse.defaultFuseOps
        { fuseGetFileStat = helloGetFileStat
        , fuseOpen = helloOpen
        , fuseRead = helloRead
        , fuseOpenDirectory = helloOpenDirectory
        , fuseReadDirectory = helloReadDirectory
        , fuseGetFileSystemStats = helloGetFileSystemStats
        }

main :: IO ()
main = Fuse.fuseMain helloFSOps Fuse.defaultExceptionHandler
