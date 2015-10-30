import System.IO


main = putStrLn "Hi"

getStringFromFile :: String -> IO String
getStringFromFile str = unwords . words  <$> ( openFile str ReadMode >>= hGetContents)

applyOnIO :: (a -> b) -> IO a -> IO b
applyOnIO f a = f <$> a

