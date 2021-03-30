module ImageLoader where

import Codec.Picture

-- dynWidth :: DynamicImage -> Int
-- dynWidth img = dynamicMap imageWidth img

--a = fromDynamicImage img 
-- ImageRGB8 -- the picture type
pixels :: FilePath -> IO ([Double])
pixels path = do
    imageLoad <- readPng path --"C:/Users/Owner/Desktop/uni/FP/2020/haskell/project/0001.png"
    case imageLoad of
        Right image -> do
            let rgb8Image = fromDynamicImage image
            let pixelList = getPixelList rgb8Image
            let listOfDoubles = getNumberList pixelList
            let normalizedDoubles = map (\number -> (number / 128.0) - 1) listOfDoubles
            return normalizedDoubles;

-- pxl row col = do
--     imageLoad <- readPng "C:/Users/Owner/Desktop/uni/FP/2020/haskell/project/0001.png"
--     case imageLoad of
--         Left error  -> putStrLn error
--         Right image -> do
--             let content = fromDynamicImage image
--             print $ pixelAt content row col


getPixelList :: Image PixelRGB8 -> [PixelRGB8]
getPixelList img = [ pixelAt img i j | i <- [0..31], j <- [0..31] ]

getNumberList :: [PixelRGB8] -> [Double]
getNumberList pixelList = map fromIntegral $ concat $ map (\(PixelRGB8 r g b) -> [r, g, b]) pixelList

class ToPixelRGB8 a where
    toRGB8 :: a -> PixelRGB8

instance ToPixelRGB8 PixelRGB8 where
    toRGB8 (PixelRGB8 r g b) = PixelRGB8 r g b

fromDynamicImage :: DynamicImage -> Image PixelRGB8
fromDynamicImage (ImageRGB8 img) = pixelMap toRGB8 img


