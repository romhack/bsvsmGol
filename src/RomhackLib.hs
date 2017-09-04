module RomhackLib ( readW16LePtrTable, PtrTableVars(..), showListHex
                  , BinaryBlockVars(..), readW8Block, takeWhileInclusive
                  , readW8Tail, CharCode, CharTables(..), DecodeTable(..)
                  ,EncodeTable(..), readTable, decodeByTable, encodeByTable
                  ,writeBlock)
                  where
import           Data.Binary.Get hiding (lookAhead) --collision with Parsec
import           Data.Word
import qualified Data.ByteString.Lazy as Bs
import qualified Data.ByteString as Bss
import           Data.Int
import           Control.Monad
import           Text.Printf
import qualified Data.Map.Lazy        as M
import  Text.Parsec hiding (count)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number
import Data.Tuple
import Data.Maybe
import Text.Parsec.Combinator


type CharCode = Word8

data PtrTableVars = PtrTableVars { ptrTableOffset :: Int64
                                  ,msgCount :: Int
                                  ,ptrBase  :: Int64
                                } deriving (Show)
data BinaryBlockVars = BinaryBlockVars {  blockName :: String
                                        , blockOffset :: Int64
                                        , blockSize :: Int64}

data CharTables = CharTables { decodeTable :: DecodeTable
                              ,encodeTable :: EncodeTable
                              } deriving (Show)

data DecodeTable = DecodeTable { dCharEntries :: M.Map CharCode Char
                               , dEndOfLines:: [CharCode]
                               , dEndOfStrings :: [CharCode]
                                } deriving (Show)

data EncodeTable = EncodeTable { eCharEntries :: M.Map Char CharCode
                               , eEndOfLines:: [CharCode]
                               , eEndOfStrings :: [CharCode]
                                } deriving (Show)


data TableData= TableData { charEntries :: [(CharCode, Char)]
                               ,endOfLines :: [CharCode]
                               ,endOfStrings  :: [CharCode]
                               } deriving (Show)

--read offsets in ROM for each of pointer entry
readW16LePtrTable :: Bs.ByteString -> PtrTableVars -> [Int64]
readW16LePtrTable input (PtrTableVars offset cnt base) =
    map ((+ base) . fromIntegral) pointers
  where
    pointers :: [Word16]
    pointers = runGet (replicateM cnt getWord16le) (Bs.drop offset input)

readW8Block :: Bs.ByteString -> BinaryBlockVars -> [Word8]
readW8Block input (BinaryBlockVars _ offset size) = Bs.unpack binaryBlock
  where
    binaryBlock =  Bs.take size $ Bs.drop offset input

readW8Tail :: Bs.ByteString -> Int64 -> [Word8]
readW8Tail input offset = Bs.unpack $ Bs.drop offset input

--print hex values for each element of list
showListHex :: (PrintfArg a) => [a] -> String
showListHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]"

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

writeBlock :: BinaryBlockVars -> Bs.ByteString -> String -> IO()
writeBlock  (BinaryBlockVars patchName offset size) patchBlock dstName = do
  dstFile <- Bss.readFile dstName
  let
    dummyLen = size - Bs.length patchBlock
    dummy = Bss.replicate (fromIntegral dummyLen) 0
    insertBlock :: Bss.ByteString
    insertBlock = Bs.toStrict patchBlock `Bss.append` dummy
    patchLen = Bs.length patchBlock
    start = Bss.take (fromIntegral offset) dstFile
    end = Bss.drop  (fromIntegral (offset + size)) dstFile
  if patchLen > size
    then error $ printf "%s size 0x%X > specified 0x%X. ABORTED!" patchName patchLen size
    else do
      Bss.writeFile dstName $ Bss.concat [start, insertBlock, end]
      putStrLn $ printf "%s successfully written at 0x%X" patchName offset
      putStrLn $ printf "0x%X bytes are free" dummyLen






----------------------------PARSE TABLE-----------------------------------------
{- Table format is XX=C...YY..ends..ZZ
where XX - hex code of char, C - actual char, YYs - list of end of line codes,
ends - marker for end of strings, ZZ - list of end of string codes.
All of them are separated by newlines
-}
readTable :: String -> CharTables
readTable tblFileString = CharTables DecodeTable {dCharEntries = dChars,
                            dEndOfLines =  eols, dEndOfStrings = eoss}
                            EncodeTable {eCharEntries = eChars,
                            eEndOfLines =  eols, eEndOfStrings = eoss}
  where
    TableData c eols eoss = td
    dChars = M.fromList c
    eChars = M.fromList (map swap c)
    td = case parseTable tblFileString of
      Left err -> error $ show err
      Right tblData -> tblData



parseTable :: String -> Either ParseError TableData
parseTable input = parse tableParser "Table parse failed" filtered
  where --preprocess text for parsing
    filtered :: String
    filtered = unlines $ filter (/="") $ lines input --skip empty lines


tableParser :: Parser TableData
tableParser = do --main parser for table file
  ents <- charEntry `manyTill` lookAhead (try eolEntry) --first 41=A
  eols <- eolEntry `manyTill` string "ends\n" --then line breaks
  eoss <- hexnum `sepEndBy1` newline --after "ends" go string terminators
  return TableData {charEntries = ents, endOfLines = eols, endOfStrings = eoss}

charEntry :: Parser (Word8, Char)
charEntry = do --parse for ex. "01 = A"
    p <- hexnum
    _ <- char '='
    v <- anyChar
    _ <- newline
    return (p, v)

eolEntry :: Parser Word8
eolEntry = do --parse hexvalues separated by newline
  p <- hexnum
  _ <- newline
  return p
----------------------PARSE SCRIPT----------------------------------------------
{- Script is a plain text file, which can have raw hex bytes values: \XX
c-style multiline comments are also allowed: /*this is comment*/
-}
encodeByTable :: EncodeTable -> String -> [CharCode]
encodeByTable encodeTbl script = case parseScript  of
    Left err -> error $ show err
    Right encoded -> encoded
  where
    parseScript = parse (scriptParser encodeTbl) "Script parse failed" script


scriptParser :: EncodeTable -> Parser [CharCode] --main parser for script file
scriptParser encodeTbl = many (newLines <|> comments) >> --first occurence of skipped data
                            many1 (try rawCode <|> plainChar encodeTbl)

rawCode :: Parser CharCode
rawCode = do --format of raw hexcode: \XX
  _ <- char '\\'
  hexStr <- count 2 hexDigit --read exactly 2 hexdigits
  _ <- many (newLines <|> comments) --skip comments
  return $ read $ "0x"++hexStr --convert to Word8

plainChar :: EncodeTable -> Parser CharCode
plainChar (EncodeTable chrs _ _)  = do --find code of simple char by table
  plainChr <- anyChar
  _ <- many (newLines <|> comments) --skip comments
  case M.lookup plainChr chrs of
    Just foundCode -> return foundCode
    Nothing -> fail $ printf "Char '%c' not found in encoding table" plainChr

comments :: Parser () --skip c-style multiline comments
comments = void $ string "/*" >> manyTill anyChar (string "*/")

newLines :: Parser ()
newLines = void newline
--------------------------------------------------------------------------------
decodeByTable :: DecodeTable -> [CharCode] -> String --decode message from binary by table
decodeByTable (DecodeTable chrs eols eoss) = go
  where
    go [] = []
    go (x:xs)
      | x `elem` eoss = printf "\\%02X\n" x --end of string, just print hexcode
      | x `elem` eols= printf "\\%02X\n" x ++ go xs
      | otherwise = foundChar : go xs
          where foundChar = fromMaybe (error (printf "Char with code %d not found in table" x))
                                   (M.lookup x chrs)
