module Main where

import Text.Blaze.Internal
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL

import Records
import MustacheTemplates

htmlEscape :: TL.Text -> TL.Text
htmlEscape = renderHtml . lazyText

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
