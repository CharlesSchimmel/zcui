zcui: zip convert update import
===============================

zcui is a tool to manage my music library. When I add new FLAC to my library, I
run it through [beets](https://beets.io/) to fix metadata and fetch cover art,
archive the FLACs on external storage, and convert to opus to sync to my
phone, then update beets with the new files.

The whole process looks like:

* Rip cd's and gather bandcamp zips into a staging folder.
* Run [`beet import`](https://beets.io/) on that folder to add cover art,
  standardize tags, fetch additional metadata, etc and move files to music
  library. This requires some manual intervention if beets can't automatically
  resolve the metadata so can't be scripted.
* Run zcui to Zip (or copy) albums to archive, Convert to opus, Update
  beets (to remove now deleted flacs), and re-Import converted albums.

This project has turned into a scratchpad for exploring project organization and
Haskell patterns like ReaderMonad/ReaderT/RIO to abstract out various
capabilities. The various steps of my library management stand in for
"real-life" use-cases, like CRUD operations.

## Installation?

zcui is pretty tailored to my specific use case but if you also use beets and
need a way to automate archiving and conversion it should be as easy as cloning
the repo and running `stack install`.
