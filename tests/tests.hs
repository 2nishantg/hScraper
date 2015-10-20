import HScraper.HTMLparser
import HScraper.Query
import HScraper.Types
import qualified Data.Text as T

simpleComment = print $parseHtml (T.pack "<!-- hey you -->")

test_html =  Prelude.unlines["<!Doctype html>",
                  "<html>",
                  "<test tag/>",
                  "<!-- hey you -->",
                  "<head>",
                      " <metadata> Test </metadata>",
                  "<!-- hey you -->",
                      "<my name is khan/>",
                  " </head>",
                  " <body>",
                    "<p class=\"testing\">",
                    "<!-- another comment -->",
                    "testing class",
                  "<!-- hey you -->",
                  "<again one liner/>",
                    "more text",
                    "</p>",
                    "<p class=\"anotherClass\">",
                       " Second class tested ",
                    "</p>",
                  " </body>",
                  " </html>"]

complicated = Prelude.unlines["<html>",
              "<head>",
              "<style tags />",
              "<STYLE type=\"text/css\">",
              "<!--",
              "A { text-decoration:none }",
              "-->",
              "</STYLE>",
              " <title>Mainak Chaudhuri</title>",
              "</head>",
              "<body>",
              "<TABLE BORDER='0'>",
              "<TR>",
              "<TD VALIGN='TOP' width='400'>",
              "<font size='2'><b>Mainak <!-- comments can be any where -->Chaudhuri</b></font>",
              "Member of faculty",
              " <a href=\"http://www.cse.iitk.ac.in\">Computer Science and Engineering</a>",
              " <a href=\"http://www.iitk.ac.in\">Indian Institute of Technology, Kanpur</a> 208016",
              "  India",
              " </TD>",
              "</TR>",
              "</TABLE>",
              "</body>",
              "</html>"]





testHtmlWithComment =  Prelude.unlines[ "<html>",
                  "<!-- my name is ayush-->",
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



main = print $ parseHtml (T.pack test_html)

testComment = print $ parseHtml (T.pack testHtmlWithComment)

bigPage = print $ parseHtml (T.pack complicated)
