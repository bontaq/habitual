{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import qualified Brick.Main           as M
import qualified Brick.Widgets.Edit   as E
import qualified Brick.AttrMap        as A
import qualified Graphics.Vty         as V
import qualified Brick.Types          as T
import qualified Brick.Widgets.List   as L

import Lib
import Types

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

drawUI :: AppState -> [T.Widget ()]
drawUI = undefined

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent = undefined

globalDefault :: V.Attr
globalDefault = V.green `on` V.black

attrMap' :: A.AttrMap
attrMap' = A.attrMap globalDefault
  [ (E.editAttr,          V.white `on` V.blue)
  , (E.editFocusedAttr,   V.black `on` V.yellow)
  , (customAttr,          fg V.green)
  ]

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const attrMap'
          }

main :: IO ()
main = someFunc
