-- -------------------------------------------------------------- [ Parser.idr ]
-- Module    : Parser.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.Common.Parser

import Data.List.Views

import Text.Lexer
import Text.Parser

import Config.Error
import Config.Common.Lexer

%default total

public export
Rule : Type -> Type
Rule ty = Grammar (TokenData Token) True ty

public export
EmptyRule : Type -> Type
EmptyRule ty = Grammar (TokenData Token) False ty


-- Some basic parsers used by all the intermediate forms

export
location : EmptyRule (Int, Int)
location
    = do tok <- peek
         pure $ MkPair (line tok) (col tok)

export
symbol : String -> Rule ()
symbol req
    = terminal (\x => case tok x of
                           Symbol s => if s == req then Just ()
                                                   else Nothing
                           _ => Nothing)

export
value : Rule String
value = terminal (\x => case tok x of
                           Value y => Just y
                           _       => Nothing)

export
double : Rule Double
double = terminal (\x => case tok x of
                           NumberD y => Just y
                           _         => Nothing)

export
integer : Rule Integer
integer = terminal (\x => case tok x of
                           NumberI y => Just y
                           _         => Nothing)

export
quoted : Rule String
quoted = terminal (\x => case tok x of
                           Quoted q => Just (stripQ q)
                           _ => Nothing)
  where
    stripQ : String -> String
    stripQ x with (unpack x)
      stripQ x | [] = ""
      stripQ x | (y :: xs) with (snocList xs)
        stripQ x | (y :: []) | Empty = ""
        stripQ x | (y :: (ys ++ [z])) | (Snoc rec) = pack ys


export
key : String -> Rule ()
key k =
    terminal (\x => case tok x of
                           Quoted q => if (stripQ q == k)
                                       then (Just ())
                                       else Nothing
                           _ => Nothing)
  where
    stripQ : String -> String
    stripQ x with (unpack x)
      stripQ x | [] = ""
      stripQ x | (y :: xs) with (snocList xs)
        stripQ x | (y :: []) | Empty = ""
        stripQ x | (y :: (ys ++ [z])) | (Snoc rec) = pack ys

export
reserved : String -> Rule ()
reserved res =
  terminal (\x => case tok x of
                    Reserved x => if trim x == res
                                    then Just ()
                                    else Nothing
                    _ => Nothing)


export
lbrace : Rule ()
lbrace = symbol "{"

export
rbrace : Rule ()
rbrace = symbol "}"

export
lbracket : Rule ()
lbracket = symbol "["

export
rbracket : Rule ()
rbracket = symbol "]"

export
lparen : Rule ()
lparen = symbol "("

export
rparen : Rule ()
rparen = symbol ")"

export
semi : Rule ()
semi = symbol ";"

export
colon : Rule ()
colon = symbol ":"

export
dot : Rule ()
dot = symbol "."

export
comma : Rule ()
comma = symbol ","

export
unit : Rule ()
unit = do
  lparen
  rparen

export
emptyArray : Rule ()
emptyArray = do
  lbracket
  rbracket

export
emptyMap : Rule ()
emptyMap = do
  lbrace
  rbrace

export
braces : Rule ty -> Rule ty
braces p = between lbrace rbrace p

export
brackets : Rule ty -> Rule ty
brackets p = between lbracket rbracket p

export
parens : Rule ty -> Rule ty
parens p = between lparen rparen p

export
commaSep : Rule ty -> EmptyRule (List ty)
commaSep p = sepBy comma p

export
commaSep1 : Rule ty -> Rule (List ty)
commaSep1 p = sepBy1 comma p

export
parse : (extensions     : TokenMap Token)
     -> (tokens_to_keep : Maybe (TokenData Token -> Bool))
     -> (input          : String )
     -> (parser         : Rule ty)
     -> Either ConfigError ty
parse extra keep str p =
  case lex extra str of
    Left (pos, err) => Left $ LexingError pos err
    Right toks =>
      let kept_tokens = filter (\x => (fromMaybe (const True) keep) x) toks
       in case Text.Parser.parse kept_tokens p of
               Left err  => Left (ParsingError err)
               Right (val,_) => Right val
-- --------------------------------------------------------------------- [ EOF ]
