-- ---------------------------------------------------------------- [ File.idr ]
-- Module    : File.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Common.File

import Text.Lexer
import Text.Parser

import Config.Common.Lexer
import Config.Common.Parser
import Config.Error

%default total
%access export

readConfigFile : (extensions     : TokenMap Token)
              -> (tokens_to_keep : Maybe (TokenData Token -> Bool))
              -> (parser         : Rule a)
              -> (fname          : String)
              -> IO (Either ConfigError a)
readConfigFile extra keep p fname = do
    Right str <- readFile fname | Left err => pure (Left (FileNotFound fname err))
    case parse extra keep str p of
      Left err  => pure $ Left err
      Right val => pure $ Right val

-- --------------------------------------------------------------------- [ EOF ]
