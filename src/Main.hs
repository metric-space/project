module Main where

import Data.Complex
import qualified Data.Vector as V

import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Display

windowSpec :: Display
windowSpec = InWindow "hello" (800,800) (0,0)


blackColor :: Color
blackColor = makeColor 0.0 0.0 0.0 1.0


whiteColor :: Color
whiteColor = makeColor 1.0 1.0 1.0 1.0


pseudoPixel :: Picture
pseudoPixel = rectangleSolid 1.0 1.0


blockf :: ((Float, Float), Bool) -> Picture
blockf ((x,y),t) = let b = if t
                            then blackColor
                            else whiteColor
                   in color b (translate x y pseudoPixel)


fractalRadius :: Float
fractalRadius = 2.0 


fractalCalc :: (Complex Float, Complex Float) -> ((Float,Float),Bool)
fractalCalc n = let a1 = V.iterateN 50 (\(start,c) -> (start*start + c,c)) (fmap (*0.005) n) :: V.Vector (Complex Float, Complex Float)
                    a = V.length . V.filter (\(x,_) -> (realPart . abs $ x) <= fractalRadius) $  a1 :: Int 
                    b =  if (a >= 49)
                       then False
                       else True
                    c = snd n
                    d = (realPart c, imagPart c) :: (Float, Float)
                in (d, b) 


complexBlock :: [(Complex Float, Complex Float)]
complexBlock = let x = [-400.0..400.0]
                   c = 0 :+ 0
               in [(c,a :+ b) | a <- x, b <- x]


fractal :: Picture
fractal =  mconcat . map (blockf . fractalCalc) $ complexBlock


main :: IO ()
main = display windowSpec whiteColor fractal
