{-# language TypeApplications #-}
module Main where

import Reflex.Brick (runReflexBrickApp)

import Dunjy (dunjy)

main :: IO ()
main = runReflexBrickApp @() (pure ()) Nothing dunjy
