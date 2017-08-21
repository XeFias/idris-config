-- --------------------------------------------------------------- [ Lexer.idr ]
-- Module    : Lexer.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Common.Lexer

import public Text.Lexer

%default total
%access export

public export
data Token = Reserved String
           | Value String
           | NumberI Integer
           | NumberD Double
           | Symbol String
           | Quoted String
           | WS String
           | Unknown String
           | Comment String

Show Token where
  show (Comment  x) = unwords ["Comment", show x]
  show (NumberI  x) = unwords ["NumberI", show x]
  show (NumberD  x) = unwords ["NumberD", show x]
  show (Value    x) = unwords ["Value", show x]
  show (Symbol   x) = unwords ["Symbol", show x]
  show (Quoted   x) = unwords ["Quoted", show x]
  show (Reserved x) = unwords ["Reserved", show x]
  show (WS       x) = unwords ["WS",     show x]
  show (Unknown  x) = unwords ["BAD TOKEN", show x]

doubleLit : Lexer
doubleLit = (is '-' <|> empty)
        <+> digits
        <+> (is '.' <+> (digits <|> empty) <|> empty)

export
reserved : String -> Lexer
reserved str = (space <+> exact str<+> space)
           <|> (some (exact str) <+> space)

strLit' : Lexer
strLit' = is '"' <+> manyTill (any) (is '"')

-- Reserved symbols
coreSymbols : List String
coreSymbols = ["(", ")", "{", "}", "[", "]", ";", ",", "=", ".", ":"]

validSymbol : Lexer
validSymbol = some (oneOf ":!#$%&*+./<=>?@\\^|-~")

coreTokens : TokenMap Token -> TokenMap Token
coreTokens ext =
     map (\x => (exact x, Symbol)) coreSymbols
  ++ [ (doubleLit, NumberD . cast)
     , (intLit, NumberI . cast)
     , (strLit', Quoted)
     , (space, WS)
     ]
  ++ ext
  ++ [ (any, Unknown) ]

export
stripWhiteSpace : TokenData Token -> Bool
stripWhiteSpace t =
  case tok t of
           WS _      => False
           otherwise => True


export
lex : TokenMap Token -> String-> Either ((Int,Int), String) (List (TokenData Token))
lex tmap str
    = case Text.Lexer.lex (coreTokens tmap) str of
           (tok, (_, _, "")) => Right tok
           (_, (c,l,msg)) => Left ((c,l), msg)

-- --------------------------------------------------------------------- [ EOF ]
