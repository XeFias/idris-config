-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Error

import Text.Parser
import Config.Common.Lexer

%access public export

data ConfigError : Type where
  LexingError    : (Int,Int) -> String -> ConfigError
  ParsingError   : ParseError (TokenData Token) -> ConfigError
  FileLexError   : String -> (Int,Int) -> String -> ConfigError
  FileParseError : String -> ParseError (TokenData Token) -> ConfigError
  FileNotFound   : String -> FileError -> ConfigError

Show ConfigError where
  show (LexingError pos rest) = unlines ["Lexing Error:", show pos, rest]
  show (ParsingError (Error str toks)) = unlines ["Parse Error:", str, show $ map (tok) toks]
  show (FileParseError fn (Error str toks))  =
    unlines [ unwords ["Parse Error:", fn, "error was:"]
            , str
            , "Remaining tokens:"
            , show $ map (tok) toks
            ]
  show (FileLexError fn pos rest)  =
    unlines [ unwords ["Lexing Error:", fn, " at ", show pos, "error was:"]
            , "Remaining tokens:"
            , show rest
            ]
  show (FileNotFound fname err) =
    unlines [ unwords ["File", show fname, "caused error:"]
            , show err
            ]


-- --------------------------------------------------------------------- [ EOF ]
