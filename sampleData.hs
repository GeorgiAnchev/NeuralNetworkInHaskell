module SampleData where

import ImageLoader;
import Control.Monad;
import System.Directory;


tinyTrainDirectory = "C:/Users/Owner/Desktop/uni/FP/2020/haskell/project/CIFARsmallFlattened/train/" 
tinyTestDirectory = "C:/Users/Owner/Desktop/uni/FP/2020/haskell/project/CIFARsmallFlattened/test/"

--getAllFiles :: FilePath -> IO [FilePath]
getAllFiles path numFiles = do
    allItems <- getDirectoryContents path
    let a = take numFiles allItems
    let fullStrings = map (path++) a
    b <- mapM pixels fullStrings
    return b

    --filterM doesFileExist allItems
    --files
    --return $ map (path ++ ) allItems

trainData :: IO [[Double]]
trainData = getAllFiles tinyTrainDirectory 100


testData :: IO [[Double]]
testData = getAllFiles tinyTestDirectory 10

trainDataPrint :: IO ()
trainDataPrint = do 
    allFiles <- getAllFiles tinyTrainDirectory 100
    print $ allFiles !! 0

getAllFolders path = do
    contents <- getDirectoryContents path
    filterM doesDirectoryExist contents