import qualified Data.ByteString.Lazy as BSL
import qualified Data.Word as Word
import qualified System.IO as I

main :: IO ()
main = prompt "FilePass?: " >>= BSL.readFile >>= (putStrLn . ceChecker . BSL.unpack)

prompt :: String -> IO String
prompt message = putStr message >> I.hFlush I.stdout >> getLine

ceChecker :: [Word.Word8] -> String
ceChecker (byte : bytes)
  | byte == 0x1B = "ISO-2022-JP" -- this is escape sequence
  | between 0x21 byte 0x7E = ceChecker bytes -- one byte character
  | between 0x81 byte 0x9F = "Shift-JIS"
  | between 0xA1 byte 0xDF = "EUC-JP"
  | otherwise = ceChecker' bytes -- checking second byte
  where ceChecker' (byte' : bytes')
          | between 0x40 byte' 0x7E || between 0x80 byte' 0xA0 = "Shift-JIS"
          | byte' == 0xFD || byte' == 0xFE = "EUC-JP"
          | otherwise = ceChecker bytes'
        ceChecker' [] = error "Impossible!!"
        between x y z = x <= y && y <= z
ceChecker [] = "Unknown"
