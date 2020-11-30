{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Contravariant

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
    -- This is basically the same as
    -- fmap f (Compose g) Compose (fmap (fmap f) g)
    (<$>) f (Compose g) = Compose ((f <$>) <$> g)

instance (Applicative f, Applicative g) =>
    Applicative (Compose f g) where
    -- Implement the pure function for an Applicative instance for Compose
    pure x = Compose (pure (pure x))
    -- Implement the (<*>) function for an Applicative instance for Compose
    (<*>) (Compose f) (Compose g) = Compose (lift2 (<*>) f g)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    -- Values encapsulated by Monads are not composable like Functors and Applicatives
    -- A good discussion can be found at https://news.ycombinator.com/item?id=8986235#:~:text=Is%20Your%20Function%3F-,Monads%20themselves%20are%20composable%20via%20bind%2C%20but%20the%20values,by%20the%20monad%20are%20not.&text=That%20can%20be%20rather%20annoying,leads%20to%20highly%20idiosyncratic%20code.
    error "todo: Course.Compose (=<<)#instance (Compose f g)"

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
instance (Functor f, Contravariant g) =>
  Contravariant (Compose f g) where
-- Implement the (>$<) function for a Contravariant instance for Compose
  (>$<) =
    error "todo: Course.Compose (>$<)#instance (Compose f g)"
