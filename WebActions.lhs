--------------------------------------------------------------------------
This file packages up some simple IO Actions downloading and manipulating
pages and files from the web.  It is an extension of the code in the
IOActions module, and is intended primarily for learning about and
demonstrating the construction of IO programs in Haskell.  If you wanted
to use functions like this in a real application, it would likely make
more sense to import the items that you need directly from the libraries
where they are defined.

This file relies on libraries that I think are only available for ghc/ghci.
Sorry, you won't be able to use it with Hugs.  The code also relies on an
extension to support flexible instance declarations, which can be enabled
in GHC using the following pragma:
    
> {-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------

> module WebActions(

>     URL(..),
>     getText,         -- :: URL -> IO String
>     getByteString,   -- :: URL -> IO ByteString
>     writeByteString, -- :: String -> ByteString -> IO ()
>     downloadTo,      -- :: FilePath -> URL -> IO ()
>     getTags,         -- :: URL -> IO [Tag String]
>     getLinkURLs,     -- :: URL -> IO [URL]
>     getHTML,         -- :: URL -> IO [TagTree String]
>     getXML,          -- :: URL -> IO [Content]

>   ) where

> import IOActions
> import Network.Curl.Download
> import qualified Data.ByteString as BS
> import Text.HTML.TagSoup
> import Text.HTML.TagSoup.Tree
> import Text.XML.Light
> import Text.URI(mergeURIStrings, unescapeString)
> import Treedot
> import Data.Char

We'll take a naive approach and view URLs as plain strings:

> type URL = String

-- Text: --------------------------------------------------------

Download the contents of specified URL as text string:

> getText           :: URL -> IO String
> getText url        = openURIString url >>= \r -> case r of
>                        Right s -> return s
>                        Left m  -> do putStrLn ("getText error for " ++ url ++ ": " ++ m)
>                                      return ""

-- ByteString: --------------------------------------------------

Download the contents of specified URL as ByteString; this is a
compact representation for chunks of binary data that can be
downloaded without any encoding/decoding work:

> getByteString     :: URL -> IO BS.ByteString
> getByteString url  = openURI url >>= \r -> case r of
>                        Right bs -> return bs
>                        Left m   -> do putStrLn ("getByteString error for " ++ url ++ ": " ++ m)
>                                       return BS.empty

An IOAction that writes a byte string out to a specified file:

> writeByteString   :: String -> BS.ByteString -> IO ()
> writeByteString    = BS.writeFile

Combining the two operations above, we can download the contents
of a file by reading it in as a ByteString and then dumping it out
to a given file:

> downloadTo        :: FilePath -> URL -> IO ()
> downloadTo dir url = getByteString url
>                      >>= writeByteString (dir </> urlName url)

In this particular example, we construct the name of the file
that is created by taking the last part of the specified URL
as the filename in the given directory, dir.  The url name is
just the rightmost portion of the URL upto but not including
the rightmost slash.

> urlName           :: String -> String
> urlName            = reverse . takeWhile ('/'/=) . reverse

-- Tags: --------------------------------------------------------

Download the page at a given URL as a list of tags:

> getTags           :: URL -> IO [Tag String]
> getTags url        = openAsTags url >>= \r -> case r of
>                        Right ts -> return ts
>                        _        -> do putStrLn ("getTags error for " ++ url)
>                                       return []

Extract a list of linked items from the page at a given URL:

> getLinkURLs       :: URL -> IO [URL]
> getLinkURLs url    = getTags url >>= \ts ->
>                       return [ unescapeString (mergeURIStrings url link)
>                              | (TagOpen "a" attrs) <- ts,
>                                ("href", link) <- attrs ]

Download the page at a given URL as an HTML tag tree:

> getHTML    :: URL -> IO [TagTree String]
> getHTML url = getTags url >>= inIO (tagTree . filter clean)
>               where clean (TagText s) = any (not . isSpace) s
>                     clean _           = True
>               -- uses tagTree :: [Tag] -> [TagTree]

Some instance declarations that allow us to convert the structure
of a tag tree into dot format.

> instance Tree (TagTree s) where
>   subtrees (TagBranch s as ts) = ts
>   subtrees (TagLeaf l)         = []

> instance LabeledTree (TagTree String) where
>   label (TagBranch s as ts)   = s
>   label (TagLeaf (TagText s)) = filter isPrint s
>   label other                 = ""

-- XML: ---------------------------------------------------------

Download the page at a given URL in XML format:

> getXML    :: URL -> IO [Content]
> getXML url = openAsXML url >>= \x ->
>              case x of
>                Right c -> return c
>                _       -> do putStrLn ("getXML error for " ++ url)
>                              return []

The following instance declarations provide the tree structure for
XML content, which will allow us to vizualize the data as a dot
tree.  (We do some truncation of text elements in an attempt to
keep the output to a reasonable size; of course, this won't help
with large pages!)

> instance Tree Content where
>   subtrees (Elem e) = elContent e
>   subtrees other    = []

> instance LabeledTree Content where
>   label (Elem e) = qName (elName e)
>   label (CRef s) = s
>   label (Text t) = show (trunc (cdData t))
>     where trunc t = case splitAt 12 t of
>                       (as, []) -> as
>                       (as, bs) -> as ++ "..."

-----------------------------------------------------------------
