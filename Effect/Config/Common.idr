-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Effect.Config.Common

import Text.Lexer
import Text.Parser

import Effects
import Effect.File

import public Config.Common.Lexer
import public Config.Common.Parser
import public Config.Error

%default total
%access export

readConfigFile : (extensions     : TokenMap Token)
              -> (tokens_to_keep : Maybe (TokenData Token -> Bool))
              -> (parser         : Rule a)
              -> (fname          : String)
              -> Eff (Either ConfigError a) [FILE ()]
readConfigFile extra keep p fname = do
    Result str <- readFile fname | FError err => pure (Left (FileNotFound fname err))
    case parse extra keep str p of
      Left err  => pure $ Left err
      Right val => pure $ Right val


-- --------------------------------------------------------------------- [ EOF ]
