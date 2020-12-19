---
layout: post
title:  "Semigroup & Monoid"
date:   2017-10-13
categories: haskell
---

# Semigroup & Monoid

Both semigroup & monoid purposes are to combine two similar type.
Monoid is just a subset of semigroup so any monoid is a semigroup.

## Semigroup

Semigroups are sets that can be **combined** together. The combine operation must be associative operation.


---
<br>

> law1 : a semigroup must have an associated

**class**
{% highlight haskell %}
class Semigroup a where
  (<>) :: a -> a -> a
{% endhighlight %}


**Maybe instance**
{% highlight haskell %}
instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b       = b
  a       <> Nothing = a
  Just a  <> Just b  = Just (a <> b)
{% endhighlight %}

**Exemple**
{% highlight haskell %}
  fmap (+3) (Just 1) -- (+3) <$> Just 1
{% endhighlight %}

I've written this 'not so good example'
{% highlight haskell %}
import Data.Semigroup as S

{- so we want to be able to manipulate wagons
   meaning we want to combine 2 wagons into 1
-}
data Wagon a = Wagon { seats :: Int
                     , klass :: Klass
                     , passengers :: [a]
                     } deriving (Show)

data Klass = Klass1 | Klass2 deriving (Eq, Ord, Show)

instance Semigroup (Wagon a) where
  (Wagon { seats = c1
         , klass = k1
         , passengers = p1 })
    <> (Wagon { seats = c2
              , klass = k2
              , passengers = p2 }) =
    Wagon { seats = c1 + c2
          , klass = min k1 k2
          , passengers = (p1 S.<> p2)
          }

main :: IO ()
main = do
  let wagon1 = Wagon { seats = 10,
                       klass = Klass2
                     , passengers = [1, 2]
                     } :: Wagon Int
  let wagon2 = Wagon { seats = 10,
                       klass = Klass1
                     , passengers = [3, 4]
                     } :: Wagon Int
  putStrLn $ show $ wagon1 S.<> wagon2
  -- will print : Wagon {seats = 20, klass = Klass1, passengers = [1,2,3,4]}

{% endhighlight %}

---
<br>


## Monoid

So monoid is just a subset of semigroup. it respect the same constraints plus has an identity
---
<br>

> law1 : a monoid must have an associated
> law2 : a monoid must have an identity

**class**
{% highlight haskell %}
class Monoid a where
        mempty  :: a
        -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
{% endhighlight %}

**Maybe instance**
{% highlight haskell %}
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
{% endhighlight %}

** custom instance**
{% highlight haskell %}
data Wagon a = Wagon { seats :: Int
                     , klass :: Klass
                     , passengers :: [a]
                     } deriving (Show)

data Klass = Klass1 | Klass2 deriving (Eq, Ord, Show)

instance Monoid (Wagon a) where
  mempty = Wagon { seats = 0, klass = Klass2, passengers = [] }
  (Wagon { seats = c1
         , klass = k1
         , passengers = p1 })
    `mappend` (Wagon { seats = c2
              , klass = k2
              , passengers = p2 }) =
    Wagon { seats = c1 + c2
          , klass = min k1 k2
          , passengers = (p1 <> p2)
          }

main :: IO ()
main = do
  let wagon1 = Wagon { seats = 10,
                       klass = Klass2
                     , passengers = [1,2]
                     } :: Wagon Int
  putStrLn $ show $
    wagon1 <> wagonFor 10
    -- will return :  Wagon {seats = 20, klass = Klass2, passengers = [1,2]}
  putStrLn $ show $
    wagon1 <> wagonFor 200
    -- will return : Wagon {seats = 10, klass = Klass2, passengers = [1,2]}
  where
    -- sometimes we can't return a real wagon so we need a default value
    wagonFor :: Int -> Wagon Int
    wagonFor x =
      if x > 100 then -- no wagon this size available
        mempty
      else
        Wagon { seats = 10
              , klass = Klass2
              , passengers = []
              } :: Wagon Int

{% endhighlight %}
