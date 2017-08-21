module Config.JSON.API

import Data.AVL.Dict

import Config.JSON

{-
public export
data JsonValue = JsonString String
               | JsonNumber Double
               | JsonBool Bool
               | JsonNull
               | JsonArray (List JsonValue)
               | JsonObject (Dict String JsonValue)
-}


export
data FromJSON : Type -> Type where
  Object : String -> FromJSON ty -> FromJSON ty
  VStr   : String -> FromJSON String
  VSci   : String -> FromJSON Double
  VBool  : String -> FromJSON Bool

  VMaybe : FromJSON a
        -> FromJSON (Maybe a)

  VOpt : ty
      -> FromJSON ty
      -> FromJSON ty

  SeqParse : FromJSON tyA
          -> (tyA -> FromJSON tyB)
          -> FromJSON tyB
  SeqEmpty : FromJSON tyA
          -> (tyA -> FromJSON tyB)
          -> FromJSON tyB

  Fail : String     -> FromJSON ty
  Pure : (val : ty) -> FromJSON ty

  Alt : FromJSON ty
     -> FromJSON ty
     -> FromJSON ty

public export
inf : Bool -> Type -> Type
inf True  x = Inf x
inf False x = x

export %inline
(>>=) : FromJSON tyA
     -> (tyA -> FromJSON tyB)
     -> FromJSON  tyB
(>>=)   = SeqParse

export
pure : (val : ty) -> FromJSON ty
pure = pure

export
(<|>) : FromJSON ty
     -> FromJSON ty
     -> FromJSON ty
(<|>) = Alt


covering
bob : FromJSON ty -> JsonValue -> Maybe ty
bob (Object x rest) (JsonObject o) = do
  obj <- lookup x o
  res <- bob rest obj
  pure res

bob (VStr x) (JsonObject o) = do
  obj <- lookup x o
  case obj of
    (JsonString val) => Just val
    otherwise        => Nothing

bob (VSci x) (JsonObject o) = do
  obj <- lookup x o
  case obj of
    (JsonNumber val) => Just val
    otherwise        => Nothing

bob (VBool x) (JsonObject o) = do
  obj <- lookup x o
  case obj of
    (JsonBool val) => Just val
    otherwise      => Nothing

bob (VMaybe rest) o =
  pure $  bob rest o

bob (VOpt def rest) o = do
  case bob rest o of
    Just val  => Just val
    otherwise => Just def

bob (SeqParse x f) o = do
  res <- bob x o
  bob (f res) o

bob (SeqEmpty x f) o = do
  res <- bob x o
  bob (f res) o

bob (Fail x) o = Nothing

bob (Alt l r) o =
  case bob l o of
    Nothing  => bob r o
    Just val => Just val

bob (Pure val) _ = Just val
bob expr obj = Nothing

data Person = MkPerson String Double

Show Person where
  show (MkPerson n a) = unwords ["MkPerson", show n, show a]


buildPerson : FromJSON Person
buildPerson = Object "person" $ do
  name <- VStr "name"
  age  <- VSci "age"
  pure (MkPerson name age)

joe : String
joe = """{ "person" : { "name": "Joe", "age": "as" } }"""

fromJSON : FromJSON ty -> String -> Maybe ty
fromJSON schema str =
  case fromString str of
    Left err  => Nothing -- Left err
    Right doc =>
      case bob schema doc of
        Nothing  => Nothing
        Just obj => Just obj

namespace Main
  main : IO ()
  main =
    case fromString joe of
      Left err => printLn "Nout"
      Right v => printLn v
