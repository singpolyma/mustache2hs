module Records where

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Text.PrettyPrint.Leijen.Text

newtype HTML = HTML Html

data Blog = Blog {
	postCount :: Int,
	posts :: [Post]
}

data Post = Post {
	entryTitle :: String,
	entryContent :: HTML
}

instance Pretty HTML where
	pretty (HTML html) = string $ renderHtml html
