-- ---------------------------------------------------------------- [ JSON.idr ]
-- Description : Parse JSON files.
--               This code was borrowed and improved from lightyear examples.
--
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.JSON

import Text.Lexer
import Text.Parser

import Config.Common.Lexer
import Config.Common.Parser
import Config.Common.File

import public Data.AVL.Dict

import public Config.Error

%default partial
%access private

-- ------------------------------------------------------------------- [ Model ]

public export
data JsonValue = JsonString String
               | JsonNumber Double
               | JsonBool Bool
               | JsonNull
               | JsonArray (List JsonValue)
               | JsonObject (Dict String JsonValue)

public export
Show JsonValue where
  show (JsonString s)   = show s
  show (JsonNumber x)   = show x
  show (JsonBool True ) = "true"
  show (JsonBool False) = "false"
  show  JsonNull        = "null"
  show (JsonArray  xs)  = show xs
  show (JsonObject xs)  =
      "{" ++ unwords (intersperse "," (map fmtItem $ Dict.toList xs)) ++ "}"
    where
      fmtItem (k, v) = show k ++ " : " ++ show v

-- ------------------------------------------------------------------ [ Parser ]

export
jsonTokMap : TokenMap Token
jsonTokMap = map (\x => (reserved x, Reserved)) ["true", "false", "null"]

jsonString : Rule JsonValue
jsonString = do
  str <- quoted
  pure (JsonString str)

jsonNumber : Rule JsonValue
jsonNumber = do
  dbl <- double
  pure (JsonNumber dbl)

bool : Rule Bool
bool =  (do reserved "true";  pure True)
    <|> (do reserved "false"; pure False)

null : Rule ()
null = do reserved "null"; pure ()

jsonBool : Rule JsonValue
jsonBool  =  (do reserved "true";  pure $ JsonBool True)
         <|> (do reserved "false"; pure $ JsonBool False)

jsonNull : Rule JsonValue
jsonNull = do
  reserved "null"
  pure JsonNull

mutual
  jsonArray : Rule (List JsonValue)
  jsonArray = brackets (commaSep1 jsonValue)
          <|> (do emptyArray; pure Nil)

  keyValuePair : Rule (String, JsonValue)
  keyValuePair = do
      key <- quoted
      colon
      value <- jsonValue
      pure (key, value)

  jsonObject : Rule (Dict String JsonValue)
  jsonObject = (do kvs <- braces (commaSep1 keyValuePair)
                   pure $ fromList kvs)
           <|> (do emptyMap; pure empty)

  jsonValue : Rule JsonValue
  jsonValue =  jsonString
           <|> jsonNumber
           <|> jsonBool
           <|> jsonNull
           <|> (do arr <- jsonArray;  pure $ JsonArray arr)
           <|> (do obj <- jsonObject; pure $ JsonObject obj)


export
json : Rule JsonValue
json = (do arr <- jsonArray;  pure $ JsonArray arr)
   <|> (do obj <- jsonObject; pure $ JsonObject obj)

export
parseJSON : String -> Rule ty -> Either ConfigError ty
parseJSON str p =
  case parse jsonTokMap (Just $ stripWhiteSpace) str p of
    Left err => Left err
    Right doc => Right doc

export
toString : JsonValue -> String
toString doc = show doc

export
fromString : String -> Either ConfigError JsonValue
fromString str =
    case parse jsonTokMap (Just $ stripWhiteSpace) str (assert_total json) of
      Left err  => Left err
      Right doc => Right doc

export
fromFile : String -> IO $ Either ConfigError JsonValue
fromFile fname = do
    Right doc <- readConfigFile jsonTokMap (Just $ stripWhiteSpace) (assert_total json) fname
                  | Left err => pure (Left err)
    pure (Right doc)


-- --------------------------------------------------------------------- [ EOF ]
