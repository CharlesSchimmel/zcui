{-# LANGUAGE TupleSections #-}

module Zcui.Util where

import           Control.Foldl                  ( Fold(..) )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Except           ( MonadError(catchError)
                                                , liftEither
                                                , throwError
                                                )
import           Control.Monad.Trans.Maybe
import           Data.Either                    ( fromRight )
import           Data.Functor                   ( ($>) )
import           Data.Text                     as T
import           Prelude                       as P
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

($$>) :: (Functor g, Functor f) => g (f a) -> b -> g (f b)
($$>) g_f b = fmap ($> b) g_f

mapMToSnd :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t (a, b))
mapMToSnd f = P.mapM (\a -> (a, ) <$> f a)

asMaybeT_ :: Monad m => [a] -> ([a] -> m b) -> MaybeT m b
asMaybeT_ = flip asMaybeT

asMaybeT :: Monad m => ([a] -> m b) -> [a] -> MaybeT m b
asMaybeT fn as =
    if P.null as then MaybeT $ pure Nothing else MaybeT (Just <$> fn as)
