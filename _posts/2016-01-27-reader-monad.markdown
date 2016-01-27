---
layout: post
title:  "Reader Monad"
date:   2015-11-09 16:26:35 +0100
categories: haskell reader monad
---

# Reader

---
<br>

> Useful to implicitely pass an environement through multiple functions

Before going straight to the reader, we need to understand (-> r) instance of Monad

**(→ r)**

{% highlight haskell %}
-- bind is a bit different for functions, remember its signature was:
>>= :: m a -> (a -> m b) -> m b
-- now say m is an (-> r) so m = (->) r
>>= :: (->) r a -> (a -> (->) r b) -> (->) r b
-- wich we can simplify with
>>= :: (r -> a) -> (a -> r -> b) -> r -> b

instance Monad ((->) r) where
  return x = \_ -> x
  h >>= f = \w -> f (h w) w
  -- h :: (r -> a)
  -- f :: (a -> r -> b)
{% endhighlight %}

{% highlight haskell %}
let h = (*4)
let f = (+)
let g = (h >>= f) 3 -- ((*4) >>= (+)) 3 = 15
-- g w = f (h w) w = (w * 4) + w

-- So why is this interesting ? well see how the parameter w is passed for each functions, If you are in the context of a Monad then you can pass the environment later on without specifying the argument again and again
-- for example, look the function below
multiplyAndAdd3 :: Int -> Int
multiplyAndAdd3 x = let
  a = x * 4
  b = x + 5
  in a + b
  -- here we have to specify x in each functions, it doesnt seem bad but if you have a lot of operations then you might want to pass x implicitely

-- like this for example
multiplyAndAdd :: Int -> Int
multiplyAndAdd =
-- observe the absence of x anywhere
  (*4) >>= \a ->
  (+5) >>= \b ->
  return (a + b)

-- and we can also use the do notation
multiplyAndAdd :: Int -> Int
multiplyAndAdd = do
  a <- (*4)
  b <- (+5)
  return (a + b)

multiplyAndAdd 3 -- 20
{% endhighlight %}

This brings us to the Reader Monad which does exactly the same thing with some aditional useful functions

**Monad Reader typeclass**

{% highlight haskell %}
newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
  return a = Reader $ \_ -> a
  m >>= k = Reader $ \r -> runReader (k $ runReader m r) r

reader :: Reader r m => (r -> a) -> m a

asks :: (r -> a) -> Reader r a
asks f = Reader f

ask :: Reader a a
ask = Reader id
{% endhighlight %}

**Simple example**

{% highlight haskell %}
import Control.Monad.Reader
import Data.Maybe
import Data.Map as Map

type Config = Map String String

serverConf = Map.fromList [
  ("domain", "localhost"),
  ("port", "8888"),
  ("protocol", "http")
  ] :: Config

url :: Reader Config String
url = do
  domain <- asks (fetch "domain")
  port <- asks (fetch "port")
  -- here you call buildProtocol but you don't pass it the environment it is done implicitely ! :-)
  protocol <- buildProtocol
  return (protocol ++ domain ++ ":" ++ port)

buildProtocol :: Reader Config String
buildProtocol = do
  protocol <- asks (fetch "protocol")
  return (protocol ++ "://")

fetch :: String -> Config -> String
fetch key conf =
  fromJust $ Map.lookup key conf

main = do
  putStrLn $ runReader url serverConf -- http://localhost:8888

{% endhighlight %}
