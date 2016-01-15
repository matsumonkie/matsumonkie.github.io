---
layout: post
title:  "Writer/Reader "
date:   2015-11-09 16:26:35 +0100
categories: haskell
---

# Writer

---
<br>

> Useful to return a tuple formed of a value and an accumulation

**Monad Writer instance**

{% highlight haskell %}
instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x, v)) >>= f =
    let (Writer (y, v')) = f x
    in Writer (y, v `mappend` v')
{% endhighlight %}

**Example with Strings**
{% highlight haskell %}
type MyWriter = Writer [String] Int

createWriter :: Int -> String -> MyWriter
createWriter speed log = writer (speed, [log])

setSpeed :: Int -> MyWriter
setSpeed speed =
  createWriter speed $ "set speed at: " ++ show speed

accelerate :: Int -> Int -> MyWriter
accelerate coef currentSpeed =
  createWriter newSpeed $ "accelerating at: " ++ show (newSpeed)
  where newSpeed = coef * currentSpeed

engineOn :: MyWriter
engineOn = createWriter 1 "engine on"

engineOff :: MyWriter
engineOff = createWriter 0 "engine off"

run :: MyWriter
run =
  engineOn >>
  setSpeed 10 >>=
  accelerate 10 >>
  engineOff >>=
  \x -> writer ((x+1) * 10000, ["car bug?"])
  -- WriterT (Identity (10000,["engine on","set speed at: 10","accelerating at: 100","engine off","car bug?"]))

-- with do notation it, prettier but less understanble IMO
run2 :: MyWriter
run2 = do
  a <- engineOn
  b <- setSpeed 10
  c <- accelerate 10 b
  d <- engineOff
  tell ["what?"]
  return (b)
  -- WriterT (Identity (0,["engine on","set speed at: 10","accelerating at: 100","engine off","what?"]))

{% endhighlight %}

# Reader

---
<br>

> Useful to implicitely pass an environement through multiple functions

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
  -- here you call buildProtocol but you don't pass
  -- it the environment it is done implicitely ! :-)
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
