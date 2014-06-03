import qualified Data.ByteString.Lazy.Char8 as BS
import VcfToPlink
import System.Environment
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = do
         filepath <- getArgs 
         file <- BS.readFile $ head filepath
         let myFile = getFile file 
             myContent = map splitLine $ content myFile
             pedFile = head.tail $ filepath
             outFileRoot = head.tail.tail $ filepath
             tpedFile = outFileRoot ++ ".tped"
             tfamFile = outFileRoot ++ ".tfam"
         pedFileContent <- BS.readFile pedFile
         writeTpedFile tpedFile myContent
         writeTfamFile pedFileContent (header myFile) tfamFile

writeTpedFile :: String -> [ SplitLine ] -> IO()
writeTpedFile f c = BS.writeFile  f. BS.unlines  $map toOutputString c

writeTfamFile :: BS.ByteString -> Line -> String -> IO()
writeTfamFile p l t = BS.writeFile t . BS.unlines $ outLines 
                       where
                          ids = drop 9 $  BS.split '\t' l 
                          idsToPedLines = pedFileToMap p
                          outLines = map (fromJust . flip  M.lookup  idsToPedLines) ids
                             


pedFileToMap :: BS.ByteString -> M.Map BS.ByteString BS.ByteString
pedFileToMap s = M.fromList pairs     
                 where 
                      curLines = tail $ BS.lines s
                      getPairs x = ( BS.takeWhile (/=','). BS.tail $ BS.dropWhile ( /= ',') x, BS.map  commaToTab x)
                      pairs = map getPairs curLines

commaToTab :: Char -> Char 
commaToTab x 
                     | x == ',' = '\t'
                     | otherwise = x
