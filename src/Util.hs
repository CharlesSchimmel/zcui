module Util where

import           Data.Text                     as T
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Control.Foldl                  ( Fold(..) )
import           Data.Either                    ( fromRight )
import           Turtle                         ( toText )

_toText path = fromRight (T.pack $ show path) $ toText path

maybeToErr :: MonadError Text m => Text -> Maybe a -> m a
maybeToErr msg = maybe (throwError msg) pure

collectEithers :: Fold (Either a b) (Either [a] [b])
collectEithers = Fold collect (Right []) id
 where
  collect (Right bs) (Right b) = Right (b : bs)
  collect (Left  as) (Left  a) = Left (a : as)
  collect _          (Left  a) = Left [a]
  collect (Left as)  _         = Left as

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft fn = either (Left . fn) Right

biMap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
biMap lFn rFn = either (Left . lFn) (Right . rFn)

