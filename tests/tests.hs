import HScraper.HTMLparser
import qualified Data.Text as T

test_html =  Prelude.unlines["<!Doctype html>",
                  "<html>",
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

complicated = Prelude.unlines["<html>",
              "<head>",
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
              "<font size='2'><b>Mainak Chaudhuri</b></font>",
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



main = print $parseHtml (T.pack test_html)

testComment = print $parseHtml (T.pack testHtmlWithComment)

bigPage = print $parseHtml (T.pack complicated)
