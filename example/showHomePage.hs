module Main where

import Text.Blaze.Internal
import Text.Blaze.Html5
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL

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
main = TL.putStr $ TL.toLazyText $
	homePage htmlEscape (Blog {
		postCount = 2,
		posts = [
			Post {
				entryTitle = "This is a post!",
				entryContent = HTML $ text $ T.pack "And the contents of that post!"
			},
			Post {
				entryTitle = "Earlier post...",
				entryContent = HTML $ b $ text $ T.pack "Text all bold"
			}
		]
	})
