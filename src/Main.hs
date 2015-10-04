{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Trees.KdTree as KD
import Data.List (group)
import qualified Data.Vector as V
import System.Environment (getArgs)

type Pixel = Integer
type Pixels = V.Vector Pixel

data Image = Image { label :: Integer, pixels :: Pixels }
    deriving (Show, Eq)

instance FromRecord Image where
  parseRecord v =
    Image <$>
    parseField (V.head v) <*>
    V.mapM parseField (V.tail v)

instance KD.Point Image where
  dimension (Image _ pixels) = V.length pixels

  coord n (Image _ pixels) = fromIntegral $ pixels V.! n

  dist2 (Image _ a) (Image _ b) = fromIntegral . V.sum . V.map (^2) $ V.zipWith (-) a b


parseRecords :: BS.ByteString -> Either String (V.Vector Image)
parseRecords = decode HasHeader

buildTree :: (V.Vector Image) -> KD.KdTree Image
buildTree images = KD.fromList $ V.toList images

-- most present integer in a list
mostPresent :: [Integer] -> Integer
mostPresent xs = fst $ foldr f (0,0) [ (head l, length l) | l <- group xs ]
  where f (x,xcnt) (y,ycnt)
          | xcnt > ycnt = (x,xcnt)
          | otherwise = (y,ycnt)

validate :: KD.KdTree Image -> Int -> Image -> Int
validate kdt k image
  | nearest == label image = 1
  | otherwise = 0
  where label (Image l _) = l
        nearest = mostPresent $ map label (KD.kNearestNeighbors kdt k image)

run :: Int -> String -> String -> IO ()
run k trainingFile validationFile = do
  trainingCsv <- BS.readFile trainingFile
  validationCsv <- BS.readFile validationFile

  let Right !training = parseRecords trainingCsv
  let Right !validation = parseRecords validationCsv

  let !kdt = buildTree training

  print $ V.sum (V.map (validate kdt k) validation)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (k:training:validation:_) -> run (read k) training validation
    _ -> putStrLn "Usage: ./digit-recognition <k> <training.csv> <validation.csv>"
