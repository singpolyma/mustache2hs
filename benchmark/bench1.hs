module Main where

import Control.Monad
import Blaze.ByteString.Builder
import qualified Data.ByteString as BS

import Records
import MustacheTemplates

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

main :: IO ()
main = void $ sequence $ replicate 10000 $ toByteStringIO BS.putStr $
	toplevel htmlEscape (TopLevel {
		thing = 12,
		subs = [
			SubLevel {
				thing2 = False,
				other = Just "w00t!"
			},
			SubLevel {
				thing2 = True,
				other = Nothing
			}
		]
	})
