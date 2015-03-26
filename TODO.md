- May need to add a special function that will try to locate modified
  even in the metaDate field (which is what an epub2 might look like)
- Need more API docs. Detail the differences between epub2 and
  epub3 in the data structures, where the fields come from and which
  are only one or the other.
- Separate some of the Metadata sub-types into their own modules,
  starting with Title. We've got some utility functions now for this,
  will help to clean it up some more.
- Better errors on parse failure, this will emerge along with more unit testing
- More unit testing
- Clean up README.md to no longer refer to ui3.info so much. And
  remove that source tarball link stuff. This is handled neatly by
  cabal, isn't it?
