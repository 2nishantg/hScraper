module HScraper.Tidy (
  tidy
  ) where
import Data.Text as T
import Data.Text.IO as TIO
import System.Process
import System.Directory(getCurrentDirectory)

tidy ::T.Text -> IO T.Text
tidy t  = do
  pwd <- System.Directory.getCurrentDirectory
  let tempFile = pwd ++ "/hscraper_temp.html"
  TIO.writeFile tempFile  t
  (_,Just hout,_,_) <- createProcess (proc "tidy" ["-q","-f", "/home/nis/hscraper_webpages.logs", tempFile]){ std_out = CreatePipe }
  TIO.hGetContents hout