\section{Types}

Custom data types and structures used throughout.

TODO: can these be split into two seperate modules?

\begin{code}
module Types where

import Data.Bifoldable

import Data.List.NonEmpty  (NonEmpty (..), (<|), nonEmpty)
import Data.Map            (Map, elemAt, foldMapWithKey, mapKeysMonotonic, singleton, unionWith)
\end{code}

\subsection{Key actions}

Data type representing actions so that they can be lifted to Template Haskell
experessions later.

\begin{code}
data KeyAction  =  ActionSpawn String
                   deriving (Eq, Show)
\end{code}

\subsection{Submaps}

A nice feature of |XMonad.Util.EZConfig.additionalKeysP| is that it allows for
the easy binding of sequences of keys, via some ``sugar'' on top of
|XMonad.Actions.Submap.submap|. I want to recreate this feature here.

\textbf{The current solution is way too complicated and needs to be simplified.}

A key may be bound directly to an action or be bound to a submap. This choice is
represented by the |ESM| type. |SM| wraps a plain |Map|'s values with |ESM|.

\subsubsection{|ESM|}

Either a submap (|LSM|) or just a value (|RSM|).

TODO: could a recursive type be used instead of this \emph{and} a |newtype|?

\begin{code}
data ESM k v = LSM (SM k v) | RSM v deriving (Eq, Show)
\end{code}

\subsubsection{|SM|}

The |SM| type wraps the keys of a |Map| with |ESM|.

\begin{code}
newtype SM k v = SM { unSM :: Map k (ESM k v) } deriving (Eq, Show)
\end{code}

\subsubsection{Instances}

Semigroup and monoid instances inherit an |Ord| constraint from |Map|.

\begin{code}
instance Ord k => Semigroup (ESM k v) where
  LSM x <> LSM y = LSM (x <> y)

instance Ord k => Semigroup (SM k v) where
  SM m <> SM n = SM $ unionWith (<>) m n

instance Ord k => Monoid (SM k v) where
  mempty = SM mempty
\end{code}

Folding, mapping, and traversing |ESM|s recurses on left values and uses rights
directly. |SM| instances just have to apply functions recursively to their
internal |Map|s and |ESM|s.

\begin{code}
instance Foldable     (ESM k) where  foldMap   f (RSM x)   =           f x
                                     foldMap   f (LSM sm)  =           foldMap f sm
instance Functor      (ESM k) where  fmap      f (RSM x)   = RSM  $    f x
                                     fmap      f (LSM sm)  = LSM  $    fmap f sm
instance Traversable  (ESM k) where  traverse  f (RSM x)   = RSM  <$>  f x
                                     traverse  f (LSM sm)  = LSM  <$>  traverse f sm

instance Foldable     (SM k) where foldMap   f (SM m) =          foldMap   (foldMap   f) m
instance Functor      (SM k) where fmap      f (SM m) = SM  $    fmap      (fmap      f) m
instance Traversable  (SM k) where traverse  f (SM m) = SM  <$>  traverse  (traverse  f) m
\end{code}

\subsubsection{Probably superfluous instances}

I don't think any of these are used, will be ever used, or should ever be used,
but they are defined here anyway for\ldots{} fun.

|ESM| can be lifted to an applicative. When combining two maps, each of the
right values of the ``argument'' map are passed to each of the right values of
the ``function'' map. A similar method is used to lift |ESM| to a monad.

\begin{code}
instance Applicative (ESM k) where
  pure = RSM
  RSM f   <*> RSM x       = RSM (f x)
  RSM f   <*> LSM sm      = LSM (fmap f sm)
  LSM sm  <*> RSM x       = LSM (fmap ($ x) sm)
  fs      <*> LSM (SM m)  = LSM (SM (fmap (fs <*>) m))

instance Monad (ESM k) where
  RSM x       >>= f = f x
  LSM (SM m)  >>= f = LSM (SM (fmap (>>= f) m))
\end{code}

|Data.Map.foldMapWithKeys| can easily be reworked into |bifoldMap| and a
|Bifoldable| instance.

\begin{code}
instance Bifoldable SM where
  bifoldMap f g (SM m) = foldMapWithKey (\k v -> f k <> bifoldMap f g v) m

instance Bifoldable ESM where
  bifoldMap _ g (RSM x)   = g x
  bifoldMap f g (LSM sm)  = bifoldMap f g sm
\end{code}

In theory, a combination of |mapKeys| and plain |fmap| theoretically can be
composed into |bimap|. However, |mapKeys| has an |Ord| constraint and will not
work with |Bifunctor|. 

Bitraversing is more insteresting.
There is no function provided by |Data.Map| that allows for traversing over keys.
Defining one, however, would not be too complicated. Two possibilities are:

\begin{description}
  \item[Reconstructing the |Map| within the applicative.] A function declared
    like this would work similarly to |mapKeysMonotonic|---and have the same
    restriction that function being applied must be monotonic, so its no good.

\begin{spec}
import Data.Map.Internal (Map (..))

bitraverseMonotonic :: Applicative f => (k -> f g) -> (v -> f w)  ->  Map k v -> f (Map g w)
bitraverseMonotonic _ _ Tip              = pure Tip
bitraverseMonotonic f g (Bin s k v l r)  = Bin  <$> pure s <*> f k <*> g v
                                                <*> bitraverseMonotonic f g l
                                                <*> bitraverseMonotonic f g r
\end{spec}

  \item[Mapping each element into a singleton and folding results.] Using
    |foldMapWithKey| and the |Ap| data type (from |Data.Monoid|), this task is
    trivial. Sequentially applying |singleton| to the new lifted keys, then
    wrapping this new |Map| in |Ap| produces a monoid that is easily folded into
    a single |Map|, as long as |Map| itself is a monoid---which it only is if
    its keys are |Ord|erable, thus having the same constraint as the previous
    method and being equally useless for this purpose.

\begin{spec}
import Data.Monoid (Ap (..))

traverseKeysMonoid :: (Applicative f, Ord g) => (k -> f g)  -> Map k (ESM k a) -> f (Map g (ESM g a))
traverseKeysMonoid f = getAp . foldMapWithKey go
  where  go k v = Ap (singleton <$> f k <*> recurse v)
         recurse (RSM x)   = RSM <$> x
         recurse (LSM sm)  = LSM <$> traverseKeysMonoid f sm
\end{spec}
\end{description}

\subsubsection{Other |Map| functions for |SM|}

|mapKeysMonotonic|; an unsafe function that applies a given function over the
keys of a submap. It does not check if the ordering of the keys has changed,
so if they do change, the |Map| will be broken, hence it being unsafe.

\begin{code}
smMapKeysMonotonic :: (k -> g) -> SM k a -> SM g a
smMapKeysMonotonic f (SM m) = SM $ fmap recurse $ mapKeysMonotonic f m
  where  recurse (RSM x) = RSM x
         recurse (LSM x) = LSM $ smMapKeysMonotonic f x
\end{code}

\subsubsection{Creating and eliminating submaps}

A ``singleton'' submap can be created by providing a ``path'' and its value, the
path being a non-empty list of keys to the value.

\begin{code}
toSM :: NonEmpty k -> t -> SM k t
toSM (k:|ks) = SM . singleton k . maybe pure (\p -> LSM . toSM p) (nonEmpty ks)
\end{code}

The inverse of the above, a path and its value can be extracted from a non-empty
submap.

\begin{code}
fromSM :: SM k t -> Maybe (NonEmpty k, t)
fromSM (SM m)  |  null m                      = Nothing
               |  (k, RSM x)    <- head' m    = Just (k:|[], x)
               |  (k, LSM sm)   <- head' m,
                  Just (ks, x)  <- fromSM sm  = Just (k<|ks, x)
               |  otherwise                   = Nothing
               where head' = elemAt 0
\end{code}
