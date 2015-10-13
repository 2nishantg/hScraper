import HScraper.HTMLparser
import qualified Data.Text as T

test_html =  Prelude.unlines["<html>",
                  "<head>",
                      " <metadata> Test </metadata>",
                  " </head>",
                  " <body>",
                    "<p class=\"testing\">",
                    "testing class",
                    "</p>",
                    "<p class=\"anotherClass\">",
                       " Second class tested ",
                    "</p>",
                  " </body>",
                  " </html>"]

main = print $parseHtml (T.pack test_html)
