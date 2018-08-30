module Main where

import Data.Complex

import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Display

a :: Display
a = FullScreen


blackColor :: Color
blackColor = makeColor 0.0 0.0 0.0 1.0


whiteColor :: Color
whiteColor = makeColor 1.0 1.0 1.0 1.0


block :: Picture
block = rectangleSolid 1.0 1.0


blockf :: ((Float, Float), Bool) -> Picture
blockf ((x,y),t) = let b = if t
                            then blackColor
                            else whiteColor
                   in color b (translate x y block)


fractalRadius :: Float
fractalRadius = 100.0 


fractal :: (Complex Float, Complex Float) -> ((Float,Float),Bool)
fractal n = let a1 = iterate (\(start,c) -> (start*start + c,c)) n :: [(Complex Float, Complex Float)]
                a = length . filter (\(x,_) -> (realPart . abs $ x) <= fractalRadius) . take 200 $ a1 :: Int
                b =  if (a >= 99)
                       then False
                       else True
                c = snd n
                d = (realPart c, imagPart c) :: (Float, Float)
            in (d, b) 


complexBlock :: [(Complex Float, Complex Float)]
complexBlock = let x = [-100.0..100.0]
                   c = 0 :+ 0
               in [(c,(a/200.0) :+ (b/200.0)) | a <- x, b <- x]


main :: IO ()
main = display a whiteColor (mconcat . map (blockf . fractal) $ complexBlock)
