module Stuff
  ( module Prelude
  , module Export
  , type (\/)
  , log
  , logError
  , logInfo
  , logWarning
  , dlmap
  , drmap
  , (\)
  , compose2, (\\)
  , compose3, (\\\)
  , compose4, (\\\\)
  , compose5, (\\\\\)
  ) where

import Prelude

import Control.Alt (class Alt, (<|>)) as Export
import Control.Alternative (class Alternative) as Export
import Control.Apply (lift2, lift3, lift4, lift5) as Export
import Control.Biapplicative (class Biapplicative, bipure) as Export
import Control.Biapply (class Biapply, (<<$>>), (<<*>>), (*>>), (<<*), bilift2, bilift3) as Export
import Control.Comonad (class Comonad, extract) as Export
import Control.Extend (class Extend, extend, duplicate, (<<=), (=>>), (=>=), (=<=)) as Export
import Control.Monad.Aff.Class (liftAff) as Export
import Control.Monad.Eff.Class (liftEff) as Export
import Control.Monad.Eff.Exception (Error) as Export
import Control.Monad.IO (IO, launchIO, runIO, runIO') as Export
import Control.Monad.IO.Class (class MonadIO, liftIO) as Export
import Control.Monad.IOSync (IOSync, runIOSync, runIOSync') as Export
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync) as Export
import Control.Monad.Rec.Class (Step(..), class MonadRec, tailRecM, tailRecM2, tailRecM3, tailRec, forever) as Export
import Control.MonadPlus (class MonadPlus) as Export
import Control.MonadZero (class MonadZero, guard) as Export
import Control.Plus (class Plus,empty) as Export
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr, bifold, bitraverse_, bifor_, bisequence_, biany, biall) as Export
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Export
import Data.Bifunctor.Clown (Clown(..)) as Export
import Data.Bifunctor.Joker (Joker(..)) as Export
import Data.Bitraversable (class Bitraversable, bisequence, bitraverse, ltraverse, rtraverse, bifor, lfor, rfor) as Export
import Data.Const (Const(..)) as Export
import Data.Decidable (class Decidable, lose, lost) as Export
import Data.Decide (class Decide, choose, chosen) as Export
import Data.Divide (class Divide, divide, divided) as Export
import Data.Divisible (class Divisible, conquer) as Export
import Data.Either (Either(..), either, isLeft, isRight, fromLeft, fromRight) as Export
import Data.Eq (class Eq1, eq1, notEq1) as Export
import Data.Exists (Exists, mkExists, runExists) as Export
import Data.Foldable (class Foldable, foldMap, foldl, foldr, fold, traverse_, for_, sequence_, oneOf, intercalate, and, or, all, any, sum, product, elem, notElem, find, findMap, maximum, maximumBy, minimum, minimumBy) as Export
import Data.Function (on) as Export
import Data.Functor.Compose (Compose(..)) as Export
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<), (>#<), coerce) as Export
import Data.Functor.Coproduct (Coproduct(..), left, right, coproduct) as Export
import Data.Functor.Coproduct.Nested (type (<\/>)) as Export
import Data.Functor.Invariant (class Invariant, imap) as Export
import Data.Functor.Mu (Mu(..), transMu, roll, unroll) as Export
import Data.Functor.Nu (Nu) as Export
import Data.Functor.Product (Product(..)) as Export
import Data.Functor.Product.Nested (type (</\>)) as Export
import Data.Group (class Group, ginverse, class CommutativeSemigroup, Abelian) as Export
import Data.Identity (Identity(..)) as Export
import Data.Int (even, odd) as Export
import Data.Int.Bits ((.&.), (.|.), (.^.), shl, shr, zshr) as Export
import Data.Lazy (Lazy, defer, force) as Export
import Data.Leibniz (Leibniz(..), type (~)) as Export
import Data.List (List(Nil), (:)) as Export
import Data.Maybe (Maybe(..), maybe, maybe', fromMaybe, fromMaybe', isJust, isNothing, fromJust) as Export
import Data.Maybe.First (First(..)) as Export
import Data.Maybe.Last (Last(..)) as Export
import Data.Module (class LeftModule, (^+), (^-), (^*), mnegateL, class RightModule, (+^), (-^), (*^), mnegateR) as Export
import Data.Monoid (class Monoid, mempty) as Export
import Data.Monoid.Additive (Additive(..)) as Export
import Data.Monoid.Alternate (Alternate(..)) as Export
import Data.Monoid.Conj (Conj(..)) as Export
import Data.Monoid.Disj (Disj(..)) as Export
import Data.Monoid.Dual (Dual(..)) as Export
import Data.Monoid.Endo (Endo(..)) as Export
import Data.Monoid.Multiplicative (Multiplicative(..)) as Export
import Data.Newtype (class Newtype, unwrap, wrap, ala, over, overF, under, underF, over2, overF2, under2, underF2) as Export
import Data.Op (Op(..)) as Export
import Data.Ord (abs, signum, class Ord1, compare1) as Export
import Data.Ord.Max (Max(..)) as Export
import Data.Ord.Min (Min(..)) as Export
import Data.Profunctor (class Profunctor, dimap, arr) as Export
import Data.Profunctor.Choice (class Choice, (+++), (|||)) as Export
import Data.Profunctor.Closed (class Closed, closed) as Export
import Data.Profunctor.Costar (Costar(..)) as Export
import Data.Profunctor.Costrong (class Costrong, unsecond, unfirst) as Export
import Data.Profunctor.Star (Star(..)) as Export
import Data.Profunctor.Strong (class Strong, (***), (&&&), second, first) as Export
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol, reifySymbol) as Export
import Data.Traversable (class Traversable, sequence, traverse, for, scanl, scanr) as Export
import Data.Tuple (Tuple(..), fst, snd, curry, uncurry, swap) as Export
import Data.Tuple.Nested (type (/\), (/\)) as Export
import Debug.Trace (trace, traceShow, traceAny, spy, traceAnyA, traceA, traceShowA, traceAnyM, traceShowM) as Export
import Partial (crash, crashWith) as Export
import Partial.Unsafe (unsafePartial, unsafeCrashWith) as Export
import Type.Proxy (Proxy(..), Proxy2(..), Proxy3(..)) as Export
import Unsafe.Coerce (unsafeCoerce) as Export

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.IOSync (IOSync)
import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Profunctor as Profunctor

infixr 6 type Either as \/

log :: String -> IOSync Unit
log = liftEff \ Console.log

logError :: String -> IOSync Unit
logError = liftEff \ Console.error

logInfo :: String -> IOSync Unit
logInfo = liftEff \ Console.info

logWarning :: String -> IOSync Unit
logWarning = liftEff \ Console.warn

dlmap :: ∀ a b c p. Profunctor p => (a -> b) -> p b c -> p a c
dlmap = Profunctor.lmap

drmap :: ∀ a b c p. Profunctor p => (b -> c) -> p a b -> p a c
drmap = Profunctor.rmap

infixr 9 compose as \
infixr 9 compose2 as \\
infixr 9 compose3 as \\\
infixr 9 compose4 as \\\\
infixr 9 compose5 as \\\\\

compose2 :: forall t100 t101 t102 t98 t99. Semigroupoid t99 => t99 t101 t100 -> (t98 -> t99 t102 t101) -> t98 -> t99 t102 t100
compose2 f g = (f \ _) \ g

compose3 :: forall t86 t90 t91 t92 t93 t94. Semigroupoid t91 => t91 t93 t92 -> (t86 -> t90 -> t91 t94 t93) -> t86 -> t90 -> t91 t94 t92
compose3 f g = ((f \ _) \ _) \ g

compose4 :: forall t70 t74 t78 t79 t80 t81 t82. Semigroupoid t79 => t79 t81 t80 -> (t70 -> t74 -> t78 -> t79 t82 t81) -> t70 -> t74 -> t78 -> t79 t82 t80
compose4 f g = (((f \ _) \ _) \ _) \ g

compose5 :: forall t50 t54 t58 t62 t63 t64 t65 t66. Semigroupoid t63 => t63 t65 t64 -> (t50 -> t54 -> t58 -> t62 -> t63 t66 t65) -> t50 -> t54 -> t58 -> t62 -> t63 t66 t64
compose5 f g = ((((f \ _) \ _) \ _) \ _) \ g
