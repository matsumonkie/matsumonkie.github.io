---
layout: post
title:  "Covariant, Contravariant, Bi, Pro Functor"
date:   2016-08-25
categories: haskell
---

## Covariant Functor

A covariant is just a regular functor. Nothing more :-)

## Contravariant Functor

A covariant functor is just a normal functor with an fmap function defined as `fmap :: (a -> b) -> f a -> f b`
We can see it a producer. If we have `a = Just 1` then we can `fmap (+ 1) a` or `fmap show a` or any function that takes an Int as input.
**Covariant** functors takes a defined input and produce any output type they want from it whereas **Contravariant** can be seen more as Consumers. They have one output defined type yet they can consume any input.
A covariant functor will take an defined ouput and modify its input.
It needs to implement a similar typeclass:

**class**
{% highlight haskell %}
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

{% endhighlight %}

**Example**
{% highlight haskell %}
newtype CT b a = CT { runCT :: a -> b }

instance Contravariant (CT String) where
  -- f    : CV String
  -- f a' : CV String a' : CV (a' -> String)
  -- f b' : CV String b' : CV (b' -> String)

  -- (b' -> a') -> (CV String) a' -> (CV String) b'

  -- (b' -> a') -> CV(a' -> String) -> CV(b' -> String)

  contramap f (CT i) = CT(i . f)
{% endhighlight %}

**Usage**
{% highlight haskell %}
toS :: CT String Int
toS = CT show

contra =
  let
    operations = contramap (+ 1) $ contramap (+ 1) $ contramap (+ 1) toS
    -- here we can see that the (+1) operations will be all applied before toS is called
    -- we are consuming operations to produce something for toS
  in putStrLn $ runCT operations 1

{- now if we try to do something similar via covariant functors: -}

instance Functor (CV Int) where
  fmap f (CV i) = CV (f . i)

same :: CV Int Int
same = CV id

cova =
  let
    operations = fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) same
    -- you'll notice that `same` is applied before all the fmap
    -- we are producing something out of same
  in putStrLn $ show $ runCV operations 1

{% endhighlight %}

## Bi Functor

A bi functor is a functor of two covariant arguments.

**class**
{% highlight haskell %}
class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

data Pair a b = Pair a b deriving (Show)

instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)

pair = Pair "" 0 :: Pair String Int
inc = (+ 1)
add = (++ "x")

main = putStrLn $ show $ bimap add inc $ bimap add inc pair -- Pair "xx" 2
{% endhighlight %}

Either, (,) have bifunctor instances for example.

**Example with Either**
{% highlight haskell %}
λ> :m Data.Bifunctor
λ> bimap (+1) (+2) $ (Left 0 :: Either Int Int)
Left 1
λ> bimap (+1) (+2) $ (Right 0 :: Either Int Int)
Right 2
{% endhighlight %}
