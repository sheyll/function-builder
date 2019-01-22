-- | This library allows you to build function builder libraries.
--
-- This library is made to be useful especially for library authors, who want to provide
-- building blocks for users to build functions of varying parameters in a type safe way.
--
-- Imagine a library that let's user create nicely formatted strings, with the ability to
-- compose strings into larger strings and to allow the rendering of
-- all sorts of values, for example 'Double's, 'Bool's, or lists to strings.
--
-- __This library__ allows the author of such a library to easily add the
-- building blocks, allowing users to build __poly variadic functions__, i.e. with parameters
-- depending on the order and composition of these building blocks.
--
-- Several 'FunctionBuilder' values sharing a common monoidal output type can be composed
-- to a big 'FunctionBuilder' value, in order to build an __output function__ that
-- has a flexible number and types of parameters depending, on the individual
-- 'FunctionBuilder's used. This output function can be obtained by 'toFunction'.
--
-- 'FunctionBuilder's can also be composed via standard type classes.
--
-- This module gives you ready-made  like 'Functor', 'Applicative', 'Semigroup', 'Monoid' or 'Category' instances;
--
-- The basic building blocks are 'toFunction', 'immediate' and 'addParameter'.
--
-- For example, you could use this library to build a string formatting
-- library, that allows users to compose arbitrary, /printf-style/ render __/functions/__
-- from reusable building blocks, such that they can be re-combined in order to make
-- get functions, that can be applied to parameters that fill place holders, like e.g.:
--
-- @
--     renderCpuTemp :: Int -> Float -> String
--     renderCpuTemp =
--       toFunction (render "CPU " . renderInt . render " Temperature: " . renderFloat)
-- @
--
module Data.FunctionBuilder where

import           Control.Category
import           Data.Monoid
import           Data.Semigroup
import           Prelude                 hiding ( id
                                                , (.)
                                                )
import           Data.Tagged

-- | A function, that takes an accumulation function as paramater,
-- and returns a function that will have zero or more parameters and returns
-- an accumulated result: @(acc -> next)
--
-- A @FunctionBuilder acc next f@ is a function @(acc -> next) -> f@.
--
-- Type parameter:
--
-- [@acc@] The final output value that gets build up by the
-- applying the resulting function build by the composed @FunctionBuilder@s.
-- If you were building a @printf@ style library, then @acc@ would
-- probably be 'String'.
--
-- [@next@] The @next@ parameter allows composing @FunctionBuilder@s, and the final output
-- will be a function @f@ with zero or more parameters of different type
-- resulting in an @acc@ value. Most 'FunctionBuilder's are parameteric in @next@ and
-- also have @next@ in a in @f_make_next@.
-- Also note that in @(acc -> next) -> f_make_next@ the @next@ is
-- the output of the continuation @acc -> next@ passed to the @FunctionBuilder@ function,
-- hence this /output/ is actually in /input/ from the perspective of the @FunctionBuilder@,
-- which makes a @FunctionBuilder@ 'Contravariant' in @next@.
--
-- [@f_make_next@] This is usually a function that returns @next@ or is
-- directly @next@, this is the resulting - seemingly /poly variadic/ -
-- __outout function__ composed through the composition of @FunctionBuilder@s, and
-- obtained by 'toFunction'.
--
-- It is required for the type-class instances allowing the
-- composition as 'Semigroup's or 'Monoid's or even 'Category'.
--
-- It is totaly valid to apply it to 'id', to get @f@, and behind @f@
-- typically lies a function of some parameters to @next@.
--
-- At the end of /the chain/ @next@ will be @acc@ and before that
-- the function that takes the next parameters and then returns out.
--
-- See `toFunction`.
--
-- Composition comes in two flavours:
--
--     (1) By using `(.)` to add to the accumulator a value passed to an additional argument
--     of the resulting output function.
--
--     (2) By using `(<>)` to append a fixed value to the accumulator directly.
--
-- For example:
--
-- > import Data.Monoid (Sum(..))
-- >
-- > add :: FunctionBuilder (Sum Int) next (Int -> next)
-- > add = FB $ \k -> \x -> k (Sum x)
--
-- Here the @next@ parameter in @add@ is just passed through and
-- is the __key__ to be able to __compose__ @FunctionBuilder@s. @add@ is
-- parametric in @next@.
-- .
-- And when we are done composing, we pass `id` to the @FunctionBuilder@, which
-- forces the the @next@ parameter to match the @acc@ type, and which
-- would the make @add@ function look like this:
--
-- > addToZero :: FunctionBuilder (Sum Int) (Sum Int) (Int -> Sum Int)
-- > addToZero = add
newtype FunctionBuilder acc next f_make_next = FB {runFunctionBuilder :: (acc -> next) -> f_make_next }

-- | Compose 'FunctionBuilder's such that the output function first takes all parameters
-- from the first 'FunctionBuilder' and then all parameters from the second 'FunctionBuilder' and then
-- appends the results of both functions, which is why we need the 'Monoid' constraint.
instance Monoid m => Category (FunctionBuilder m) where
  (.) (FB f) (FB g) = FB (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))
  id = FB ($ mempty)

-- | Allow appending a 'FunctionBuilder' to another without changing the resulting output function.
-- For example, 'FunctionBuilder's that have @FunctionBuilder m r r@ can append something to @m@.
-- It is not possible to add new parameters to the output function, this can only be done by
-- the 'Category' instance.
instance Semigroup m => Semigroup (FunctionBuilder m r r) where
  (<>) (FB f) (FB g) = FB (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))

-- | Allow appending a 'FunctionBuilder' to another without changing the resulting output function.
-- For example, 'FunctionBuilder's that have @FunctionBuilder m r r@ can append something to @m@.
-- It is not possible to add new parameters to the output function, this can only be done by
-- the 'Category' instance.
instance Monoid m => Monoid (FunctionBuilder m r r) where
  mappend = (<>)
  mempty  = id

instance Functor (FunctionBuilder m r) where
  fmap f (FB !h) = FB $ \k -> f (h k)

instance Applicative (FunctionBuilder m r) where
  pure = FB . const
  (FB f) <*> (FB x) = FB (\k -> f k (x k))

instance Monad (FunctionBuilder m r) where
  (FB m) >>= f = FB (\k -> runFunctionBuilder (f (m k)) k)

-- | Turn a 'FunctionBuilder' into the __output function__ that consumes
-- zero or more of parameter and then always return @outout@.
--
-- If passed a 'FunctionBuilder' value of type @FunctionBuilder String String (Int -> Double -> Int -> String)@
--
-- For example:
--
-- > example :: Int -> Double -> Int -> String
-- > example = toFunction (i . d . i)
-- >
-- > s :: String -> FunctionBuilder String a a
-- > s x = FB (\k -> k x)
-- >
-- > i :: FunctionBuilder String next (Int -> next)
-- > i = FB (\k x -> k $ show x)
-- >
-- > d :: FunctionBuilder String next (Double -> next)
-- > d = FB (\k x -> k $ show x)
toFunction :: FunctionBuilder output output make_output -> make_output
toFunction = ($ id) . runFunctionBuilder

-- ** Building 'FunctionBuilder's

-- | Create a 'FunctionBuilder' that /appends/ something to the (monoidal-) output value.
--
-- This is a smart constructor for a 'FunctionBuilder'.
-- This functions is probably equal to:
--
-- > immediate x = FB (\k -> k x)
--
-- Example:
--
-- When building a 'String' formatting 'FunctionBuilder'
-- the function to append a literal string could be:
--
-- > s :: String -> FunctionBuilder String a a
-- > s = immediate
--
-- > c :: Char -> FunctionBuilder String a a
-- > c = immediate . (:[])
--
-- > example :: String
-- > example = toFunction (s "hello" . c ' ' . s "world")
--
-- >>> example
-- "hello world"
--
-- See the example in `toFunction`.
immediate :: m -> FunctionBuilder m r r
immediate m = FB { runFunctionBuilder = ($ m) }

-- | Create a 'FunctionBuilder' that adds an argument to the output function,
-- and converts that argument to a value that can be accumulated in the
-- resulting monoidal value.
--
-- This is a smart constructor for a 'FunctionBuilder'.
-- This functions is probably equal to:
--
-- > addParameter f = FB (\k x -> k (f x))
--
-- Example:
--
-- When building a 'String' formatting 'FunctionBuilder'
-- the function to append a parameter that has a show instance could be:
--
-- > showing :: Show a => FunctionBuilder String r (a -> r)
-- > showing = addParameter show
--
-- > example :: (Show a, Show b) => a -> b -> String
-- > example = toFunction (showing . showing)
--
-- >>> example True 0.33214
-- "True0.33214"
--
-- See the example in `toFunction`.
addParameter :: (a -> m) -> FunctionBuilder m r (a -> r)
addParameter f = FB { runFunctionBuilder = (. f) }

-- ** Modifying Parameters of 'FunctionBuilder's

-- | Take away a function parameter added with 'addParameter' by /pre -/ applying it to some
-- value.
-- This is equivalent to:
--
-- @
--     fillParameter f x = f <*> pure x
-- @
--
fillParameter :: FunctionBuilder m r (a -> b) -> a -> FunctionBuilder m r b
fillParameter (FB !f) x = FB $ \k -> f k x

-- | Convert a 'FunctionBuilder' for a function @(a -> b)@ to @(Tagged tag a -> b)@.
tagParameter
  :: forall tag m r a b
   . FunctionBuilder m r (a -> b)
  -> FunctionBuilder m r (Tagged tag a -> b)
tagParameter = fmap (\f -> f . untag)

-- ** 'FunctionBuilder' Transformations

-- | Compose to 'FunctionBuilder's such that the second 'FunctionBuilder' may depend on the intermediate result
-- of the first. If you skwirm hard enough you __almost__ see '(>>=)' with @m ~ n@.
bind
  :: FunctionBuilder m b c
  -> (m -> FunctionBuilder n a b)
  -> FunctionBuilder n a c
bind mbc fm =
  FB $ \kna -> runFunctionBuilder mbc (($ kna) . runFunctionBuilder . fm)

-- | Convert the accumulated (usually monoidal-) value,
-- this allows to change the underlying accumlator type.
mapAccumulator :: (m -> n) -> FunctionBuilder m a b -> FunctionBuilder n a b
mapAccumulator into (FB f) = FB (\k -> f (k . into))

-- | Convert the output of a 'FunctionBuilder' value; since most
-- 'FunctionBuilder's are parameteric in @r@ they also have @r@ in a
-- in @a@, such that @a@ always either is @r@ or is a
-- function returning @r@ eventually.
--
-- In order to get from a 'FunctionBuilder' that can accept a continuation returning it an @r@
-- to a 'FunctionBuilder' that accepts continuations returning an @s@ instead, we need to
-- apply a function @s -> r@ to the return value of the continuation.
--
-- Note that a 'mapNext' will not only change the @r@ to an @s@ but
-- probably also the the @a@, when it is parametric, as in this contrived example:
--
-- > example :: Int -> x -> Sum Int
-- > example = toFunction (ign add)
-- >
-- > add :: FunctionBuilder (Sum Int) next (Int -> next)
-- > add = FB (\k x -> k $ Sum x)
-- >
-- > ign :: FunctionBuilder m (x -> r) a -> FunctionBuilder m r a
-- > ign = mapNext const
--
-- Here the extra parameter @x@ is /pushed down/ into the @a@ of the @add@ 'FunctionBuilder'.
mapNext :: (s -> r) -> FunctionBuilder m r a -> FunctionBuilder m s a
mapNext outof (FB f) = FB (\k -> f (outof . k))
