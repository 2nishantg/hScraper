module HScraper.Tidy (
  tidy
  ) where
import Data.Text as T
import Data.Text.IO as TIO
import System.Process

tidy ::T.Text -> IO T.Text
tidy t  = do
  TIO.writeFile "/home/nis/hscraper_temp.html" t
  (_,Just hout,_,_) <- createProcess (proc "tidy" ["-q","-f", "/home/nis/hscraper_webpages.logs", "/home/nis/hscraper_temp.html"]){ std_out = CreatePipe }
  TIO.hGetContents hout
