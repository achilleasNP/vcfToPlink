import qualified Data.ByteString.Lazy.Char8 as BS
import VcfToPlink
import System.Environment

main :: IO ()
main = do
         filepath <- getArgs 
         file <- BS.readFile $ head filepath
         let myFile = getFile file 
             myContent = map splitLine $ content myFile
--         BS.writeFile  (head $ tail filepath)  $ BS.pack "achilleas" 
         BS.writeFile  (head $ tail filepath) . BS.unlines $  map toOutputString  myContent 

lightMap:: [BS.ByteString] -> BS.ByteString
lightMap = BS.unwords 
