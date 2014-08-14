{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit
import qualified Data.Text as T
import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput

-- mkIntroDialog requires the handle monad from mkGame
-- It sets up an edit field (within a dialog widget) and when the
-- dialog widget is accepted (and thus automatically closed) it will
-- pass the edit widget's contents into the handle monad
mkIntroDialog handle = do
  prompt <- plainText $ T.pack "Type something"
  e <- editWidget
  efg <- newFocusGroup
  efg `addToFocusGroup` e

  dbox <- (return prompt) <--> (return e)
  (d, dfg) <- newDialog dbox ""

  fg <- mergeFocusGroups efg dfg

  c <- centered =<< withPadding (padLeftRight 10) (dialogWidget d)
  d `onDialogAccept` \this -> getEditText e >>= handle
  d `onDialogCancel` const shutdownUi
  return (c, fg)

-- mkGame returns:
--     a text widget
--     a focus group
--     a handle monad that will set the text when called
mkGame = do
  tw <- plainText $ T.pack "Placeholder Text"
  fg <- newFocusGroup
  fg `addToFocusGroup` tw
  let setGameText txt = tw `setText` ("You entered: " `T.append` txt)
  return (tw, fg, setGameText)

main = do
  c <- newCollection

  (ui_game, fg_game, setGameText) <- mkGame
  switch_game <- addToCollection c ui_game fg_game

  -- handle is a monad that sets the game text and switches focus to it
  let handle txt = do
      setGameText txt
      switch_game

  (ui_intro, fg_intro) <- mkIntroDialog handle
  switch_intro <- addToCollection c ui_intro fg_intro
  switch_intro

  fg_game `onKeyPressed` \_ k _ ->
        case k of
            KEsc -> exitSuccess
            _ -> return False

  runUi c defaultContext
