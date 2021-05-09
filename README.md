zcui: zip convert update import
===============================

A scratchpad for various Haskell design patterns

## Purpose 

This project has turned into a scratchpad for working with various
Haskell infrastructure patterns like using the ReaderMonad/ReaderT/RIO pattern
to abstract out various capabilities. The various steps of my library management
stand in for "real-life" use-cases, like CRUD operations.

My music library management process generally goes like:

* Rip cd's and gather bandcamp zips into a staging folder.
* Run [`beet import`](https://beets.io/) on that folder to add cover art,
  standardize tags, fetch additional metadata, etc and move files to music
  library. This requires some manual intervention so can't be scripted.
* Run zcui to Zip (actually copy) albums to archive, Convert to opus, Update
  beets (to remove now deleted flacs), and re-Import converted albums.
