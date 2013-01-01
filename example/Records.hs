module Records where

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Data.Text.Buildable

newtype HTML = HTML Html

data Blog = Blog {
	postCount :: Int,
	posts :: [Post]
}

data Post = Post {
	entryTitle :: String,
	entryContent :: HTML
}

instance Buildable HTML where
	build (HTML html) = renderHtmlBuilder html
