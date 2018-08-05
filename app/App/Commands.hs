module App.Commands where

import App.Commands.Cat
import App.Commands.CmpEq8s
import App.Commands.Wc
import Data.Semigroup       ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdCmpEq8s
  <>  cmdCat
  <>  cmdWc
