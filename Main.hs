-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Some Javascript stuff
import GHCJS.Types
--import Javascript.Web.Window
import qualified JavaScript.Web.Canvas as Canvas

-- | Miso framework import
import qualified Miso
import Miso ((<#))
import Miso.Html
import Miso.String

import qualified Control.Lens as L
import Control.Lens.Operators
import Linear

import Control.Monad


type Point = V2 Double
type Force = V2 Double

data Body = Body { _position :: Point
                 , _velocity :: Point
                 , _acceleration :: Point
                 } deriving (Eq, Show)

L.makeLenses ''Body

type Model = [Body]


data Action
  = MoveBody
  | DrawBody
  | AddBody !(V2 Double)
  | NoOp


gravity :: Point -> Point -> Force
gravity p1 p2 = direction ^* strength
  where
    g = 1500
    direction = p2 - p1
    len = norm direction
    strength = g / (len^2)

updateModel
  :: Action
  -> Model
  -> Miso.Effect Action Model
updateModel (DrawBody) body = body <# do
  ctx <- getCtx
  Canvas.clearRect 0 0 400 400 ctx
  --Canvas.fillStyle 255 204 0 0.6 ctx
  --Canvas.beginPath ctx
  --Canvas.arc 150 150 10.0 0 (pi*2) False ctx
  --Canvas.fill ctx
  Canvas.fillStyle 0 0 0 0.6 ctx
  forM_ body $ \b@Body{..} -> do
    Canvas.beginPath ctx
    Canvas.arc (_position ^. _x) (_position ^. _y) 3.0 0 (pi*2) False ctx
    Canvas.fill ctx
  pure MoveBody

updateModel (MoveBody) body =
  body' <# pure DrawBody
  where
    body' = stepAll . stepAll . stepAll . stepAll $ body
    stepAll b = step <$> b <*> b
    step (Body l _ _) b@Body{..}
      | l == _position = b
      | otherwise = let
                  dt            = 0.004166666666666666667
                  position'     = _position
                                + (_velocity ^* dt * 0.5)
                                + (0.5 *^ _acceleration ^* dt^2)

                  acceleration' = gravity _position l
                  velocity'     = _velocity + (0.5 *^
                                                    (_acceleration
                                                      + acceleration')
                                                    ^* dt)
                  in Body position' velocity' acceleration'

main = Miso.startApp Miso.App {
    initialAction = MoveBody
  , update = updateModel
  , ..
  }
  where
    view _ = canvas_ [ id_ "canvas"
                   , width_ "400"
                   , height_ "400"
                   ] []
    model = [ Body (V2 50 50) (V2 30 (-24)) (V2 0 0)
            , Body (V2 250 250) (V2 (-30) 24) (V2 0 0)
            ]
    subs  = []
    events = defaultEvents
    mountPoint = Nothing

foreign import javascript unsafe "$r = document.getElementById('canvas').getContext('2d');"
  getCtx :: IO Canvas.Context
