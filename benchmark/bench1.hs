module Main where

import Control.Monad
import Text.Blaze.Internal
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL

import Records
import MustacheTemplates

htmlEscape :: TL.Text -> TL.Text
htmlEscape = renderHtml . lazyText

main :: IO ()
main = void $ sequence $ replicate 10000 $ TL.putStr $ TL.toLazyText $
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
