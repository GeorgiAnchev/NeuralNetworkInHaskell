import ImageLoader;
import System.Directory;
import Control.Monad;

currentDirectory = "C:/Users/Owner/Desktop/uni/FP/2020/haskell/project"

classesCount :: Integer
classesCount = 10

classes :: [[Char]]
classes = ["airplane", "automobile", "bird", "cat", "deer", "dog", "frog", "horse", "ship", "truck"];

trainFolder :: [Char]
trainFolder = "train"

testFolder :: [Char]
testFolder = "test"

dataFolder = "CIFARsmall"

imagesInClassSmall :: Integer
imagesInClassSmall = 10

trainPath = currentDirectory ++ "/" ++ dataFolder ++ "/" ++ trainFolder ++ "/"

smallTrainSetFolders :: [String]
smallTrainSetFolders = map (trainPath ++) classes

smallTrainSetFiles = do
    filesList <- getAllFilesIn smallTrainSetFolders
    --allFiles <- concat filesList
    let listOfList = sequence filesList
    smth <- listOfList
    let listOfPath = concat smth
    return listOfPath;

getAllFilesIn:: [String] -> [String]
getAllFilesIn folderPaths = do 
    filePaths <- map getAllFiles folderPaths
    return filePaths

--a = pixels "C:/Users/Owner/Desktop/uni/FP/2020/haskell/project/0001.png";

getAllFolders :: FilePath -> IO [FilePath]
getAllFolders path = do
    allItems <- getDirectoryContents path
    filterM doesDirectoryExist allItems

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles path = do
    allItems <- getDirectoryContents path
    files <- filterM doesFileExist allItems
    let a = map (path ++) files
    print "asda"

