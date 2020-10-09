zcui: zip convert update import
===============================

_Turning a 100 line bash script into a 450 line Haskell script: Adventures in
[the Turtle library](https://hackage.haskell.org/package/turtle-1.5.19), better
Haskell coding practices, and MTL monad stacks_

## Purpose
My music library management process generally goes like:

* Rip cd's and gather bandcamp zips into a staging folder.
* Run [`beet import`](https://beets.io/) on that folder to add cover art,
  standardize tags, fetch additional metadata, etc and move files to music
  library. This requires some manual intervention so can't be scripted.
* Run zcui to Zip (actually copy) albums to archive, Convert to opus, Update
  beets (to remove now deleted flacs), and re-Import converted albums.

## Thoughts
* Turtle is a pretty robust library, but had a slight learning curve. I'm not
  sure I agree with the inherent listy-ness of 'Shell', and there doesn't seem
  to be any easy way to just flush the shell where I needed the result in IO
  (other than reduce, although there's probably a Fold for that.)
* The basic ReaderT, ExceptT stack was easier to set up and required less
  boilerplate than I thought. I don't know why I'm only using it now.

## Is this Turtle implementation better than my shell script?

Maybe. It's probably more robust in some places, and adding options was not only
easy but made it more configurable. It is telling that for a barely non-trivial
script, I pulled in a Reader, Except stack and did not do everything in Shell.

Would I use Turtle for scripts in the future? Definitely, although my "this
script has become too long and should be adapted in a higher-level language"
threshold might be higher.
