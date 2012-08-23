module Main where

import Control.Monad
import Blaze.ByteString.Builder
import qualified Data.ByteString as BS

import Records
import Text.Hastache
import Text.Hastache.Context 

main :: IO ()
main = void $ sequence $ replicate 10000 $ toByteStringIO BS.putStr =<<
	hastacheFileBuilder defaultConfig "toplevel.mustache"
	(mkGenericContext (TopLevel {
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
	}))
