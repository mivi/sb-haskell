import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int
import Control.Monad

trybytes = L8.readInt bytes
  where bytes = L8.pack "1234567890"

createSample :: String -> String -> L8.ByteString
createSample n1 n2 = L8.concat [cstr n1, L8.pack n1, cstr n2, L8.pack n2]
          where cstr = (L8.pack).show.length

data State = State {
  string :: L.ByteString
  ,  offset :: Int64
  ,  nextsize :: Int64
  } deriving Show

-- a = L8.readInt
simpleLengthParse :: State -> Either String (Int, State)
simpleLengthParse ps = case L8.readInt (snd (L8.splitAt (offset ps) (string ps))) of
  Nothing -> Left "not a number"
  Just (i,s) ->Right (i, ps {string=s, offset=0, nextsize=(fromIntegral i)})


simpleStringParse :: State -> Either String  (String, State)
simpleStringParse ps = if (nextsize ps) < 0
                         then  Left "Invalid index"
                         else  let a = L8.splitAt (nextsize ps) (string ps) in
                         Right (L8.unpack (fst a),ps {string=snd a, offset=0,nextsize=0})

newtype Parser a = Parser {
  fetchparser ::  State -> Either String (a, State)
  }

instance Monad Parser where
  m >>= f = Parser newparser
    where newparser ps =
            case (fetchparser m) ps of
              Left msg -> Left msg
              Right (result, newps) -> (fetchparser (f result)) newps

  return a = Parser ( \s ->Right (a, s))

  fail a = Parser ( \_ -> Left a)

lengthbasedStringParser = Parser (simpleLengthParse) >>=
  (\_ ->Parser (simpleStringParse))

execMyParser ps =(fetchparser jointparser) ps
  where jointparser = Parser(simpleLengthParse) >>=
  (\_ -> Parser(simpleStringParse)) >>=
  (\x ->Parser(simpleLengthParse)) >>=
  (\_ -> Parser(simpleStringParse)) >>=
  (\y -> return [x,y])

