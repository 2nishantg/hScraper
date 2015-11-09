import HScraper

main :: IO ()
main = runTests testUrlsList

testUrlsList :: [String]
testUrlsList = ["http://home.iitk.ac.in/~nishgu/"
               ,"http://www.cse.iitk.ac.in/users/mainakc/"
               ]

runTests :: [String] -> IO ()
runTests xs  = testPrint $ foldl ga [] xs
  where ga acc el = (el, urlTest el) : acc

urlTest :: String -> IO Bool
urlTest url = (NullTree /=) <$> getParsedHTML <$> (tidy =<< fetchResponse url)


testPrint :: [(String,IO Bool)] -> IO ()
testPrint  = mapM_  printResult

printResult :: (String,IO Bool) -> IO ()
printResult (x, y) = putStr x >> y >>= g
  where g True = putStrLn " Passed"
        g False = putStrLn " Failed"

testHtml = Prelude.unlines["<html>",
                          "<head> yolo </head>",
                          "<body> <p> test </p> </body>",
                          "</html>"]
