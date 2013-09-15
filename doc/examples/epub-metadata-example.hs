import Codec.Epub
import Codec.Epub.Data.Package
import Control.Monad.Error


main :: IO ()
main = do
   -- epub-metadata actions are in MonadIO and MonadError, so we're
   -- using ErrorT here

   result <- runErrorT $ do

      -- Use one of the getPkgXmlFrom* actions to extract the
      -- Package Document as an XML string
      --
      -- See Codec.Epub.IO

      (_pathToXmlInEpub, xmlString) <- getPkgXmlFromZip
         "/path/to/book.epub"

      -- Now the sections of meta-information about the book can
      -- be extracted from that XML using functions like getPackage,
      -- getMetadata, etc.
      --
      -- See Codec.Epub.Parse

      pkg <- getPackage xmlString  -- :: Codec.Epub.Data.Package
      meta <- getMetadata xmlString  -- :: Codec.Epub.Data.Metadata

      -- Parts of these data structures can be used from here
      -- as needed
      --
      -- See Codec.Epub.Data.Package for pkgVersion below
      -- and the others in Codec.Epub.Data.*

      liftIO $ putStrLn $ pkgVersion pkg

      -- There is also pretty-print formatting of these data types
      -- in Codec.Epub.Data through the Formattable typeclass
      --
      -- See Codec.Epub.Format

      liftIO $ putStr $ format meta

   either putStrLn return result
