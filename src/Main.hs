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
                    b = (realPart . abs . fst .V.last $ a1) <= fractalRadius :: Bool
                    c = snd n
                    d = (realPart c, imagPart c) :: (Float, Float)
                in (d, b) 


complexBlock :: V.Vector (Complex Float, Complex Float)
complexBlock = let x = [-400.0..400.0]
                   xv = V.fromList x
                   c = 0 :+ 0
               in V.concatMap (\x -> V.map (\z -> (c, x :+ z)) xv) xv


fractal :: Picture
fractal =  mconcat . V.toList . V.map (blockf . fractalCalc) $ complexBlock


main :: IO ()
main = display windowSpec whiteColor fractal
