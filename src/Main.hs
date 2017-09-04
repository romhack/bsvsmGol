module Main where

import           Data.Bits
import qualified Data.ByteString.Lazy as Bs
import           Data.Word
import           RomhackLib           (BinaryBlockVars (..), CharCode,
                                       CharTables (..), EncodeTable (..),
                                       PtrTableVars (..), decodeByTable,
                                       encodeByTable, readTable,
                                       readW16LePtrTable, readW8Block,
                                       readW8Tail, showListHex, writeBlock)


import           Data.Binary.Put
import           Data.List
import           Data.List.Split
import qualified Data.Map.Lazy        as M
import           Data.Maybe
import           System.Environment

type Nybble =  Word8


----------------------------CONSTANTS-------------------------------------------
ptrTblVars :: PtrTableVars
ptrTblVars = PtrTableVars { ptrTableOffset = 0x1D8BC, msgCount = 0x40
                          , ptrBase = 0x2f010} --game's pointer table

charMapVars :: BinaryBlockVars
charMapVars = BinaryBlockVars {blockName = "charMap"
                              , blockOffset = 0x1D837, blockSize = 0x2B}

scriptVars :: BinaryBlockVars
scriptVars = BinaryBlockVars {blockName = "script"
                              , blockOffset = 0x2F010, blockSize = 0x76E}


decompress :: String -> IO ()
decompress srcFileName = do
  rom <- Bs.readFile srcFileName
  decodeTblStr <- readFile "decode.tbl"
  let
    CharTables decodeTbl _ = readTable decodeTblStr --parse table file
    offsets = readW16LePtrTable rom ptrTblVars
    charMap = map (subtract 0x20) $ readW8Block rom charMapVars
      -- 0x20 convert from ASCII to ROM encoding
    messages = map (decompressMsg charMap . readW8Tail rom) offsets
      -- read chunks of bytes starting from pointer points, and decompress them
    decodedMessages = map (decodeByTable decodeTbl) messages
    commentedMessages = map (\m -> "\n/*\n"++ m ++"*/\n" ++ m) decodedMessages
  writeFile "script.txt" $ concat commentedMessages

decompressMsg :: [CharCode] -> [Word8] -> [CharCode] --uncompress whole input
decompressMsg charMap input = go inputNybbles
  where
    go :: [Nybble] -> [CharCode] --decompress full input stream
    go [] = []
    go (0xE:idx:xs) = lookUpCharMap (idx + 0xE) : go xs
    go (0xF:idx:xs) = lookUpCharMap (idx + 0x1E) : go xs
    go (idx:xs) = lookUpCharMap idx : go xs
    lookUpCharMap :: Nybble -> CharCode
    lookUpCharMap index = charMap !! fromIntegral index
    inputNybbles :: [Nybble] --split full input stream on nybbles
    inputNybbles = concatMap (\byte -> [byte `shiftR` 4, byte .&. 0xF]) input

compress :: String -> IO ()
compress dstFileName = do
  script <- readFile "script.txt"
  encodeTblStr <- readFile "encode.tbl"
  let
    CharTables _ encodeTbl = readTable encodeTblStr --parse table file
    scriptEncoded = encodeByTable encodeTbl script
    charHistogram = histogram scriptEncoded
    charMap = map snd charHistogram
    charMapSerialized = map (+ 0x20) charMap --charmap stored in ASCII

    EncodeTable _ _ eoss = encodeTbl
    messages = (split.dropFinalBlank.keepDelimsR.oneOf) eoss scriptEncoded
    compressedMsgs = map (compressMsg charMap) messages
    compressedEosMsg = compressMsg charMap [head eoss] --single eos entries in ptr tbl
    offsets = calcOffsets compressedEosMsg compressedMsgs
    ptrBlock = Bs.concat $ map (runPut . putWord16le) offsets
    PtrTableVars ptrTableOffs msgCnt _ = ptrTblVars
    ptrInsertVars = BinaryBlockVars {blockName = "pointers" --form vars for insertion
                                    , blockOffset = ptrTableOffs
                                    , blockSize = 2* fromIntegral msgCnt}
    compressedBlock = compressedEosMsg ++  --start with fixed eosMsg
                        concat (filter (/=compressedEosMsg) compressedMsgs)

  writeBlock charMapVars (Bs.pack charMapSerialized) dstFileName
  writeBlock ptrInsertVars ptrBlock dstFileName
  writeBlock scriptVars (Bs.pack compressedBlock) dstFileName


calcOffsets :: [Word8] -> [[Word8]] -> [Word16]
calcOffsets eosMsg = go 1 --start from 1, as 0 is eos entry
  where
    go _ [] = [] --last message doesn't generate  pointer
    go accum (m:ms)--single eos, just emit zero pointer, this entry not stored in script
      | m == eosMsg = 0 : go accum ms
      | otherwise = accum : go (accum + fromIntegral (length m)) ms

histogram :: Ord a => [a] -> [(Int,a)] --sorted counts of each char occurence in list
histogram xs = sortBy (flip compare) countEntries
  where
    countEntries = swap . M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]
    swap = map (\(a,b)->(b,a))

compressMsg :: [CharCode] -> [Word8] -> [Word8]
compressMsg charMap input = mergeInputnybbles $ concatMap compIndex indexes
  where
    indexes = map lookupIdx input
    lookupIdx :: Word8 -> Word8
    lookupIdx x = fromIntegral $ fromMaybe (error "Code not found in charMap")
                  $ elemIndex x charMap
    compIndex :: Word8 -> [Word8]
    compIndex i
      | i >= 0x1E = [0xF, i - 0x1E]
      | i >= 0xE = [0xE, i - 0xE]
      | otherwise = [i]
    mergeInputnybbles :: [Word8] -> [Word8]
    mergeInputnybbles [] = []
    mergeInputnybbles [lo] = [lo*0x10]
    mergeInputnybbles (hi:lo:xs) = hi * 0x10 + lo : mergeInputnybbles xs


main :: IO()
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "bvsmGolv0.1\nGolob codes compression tool for 'Bart Vs. the Space Mutants' game."
    parse ["-d", fileName] = decompress fileName
    parse ["-c", fileName] = compress fileName
    parse _ = putStrLn "Usage:\n\
      \  bvsmGol -d <file>  Decompress script from given ROM file.\n\
      \  bvsmGol -c <file>  Compress script and insert in given ROM file.\n\
      \Options:\n\
      \  -h     Show this screen.\n\
      \  -v     Show version."
