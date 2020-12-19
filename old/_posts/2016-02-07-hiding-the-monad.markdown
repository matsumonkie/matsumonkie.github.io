---
layout: post
title:  "Hiding the monad"
date:   2016-02-06
categories: monad newtype
---

# Hiding the monad

---
<br>

In the Reader Monad post, we eventually create a really simple configuration from a reader.
The way it was done ended up with functions like this:

{% highlight haskell %}
buildProtocol :: Reader Config String
buildProtocol = do
  protocol <- asks (fetch "protocol")
  return (protocol ++ "://")
{% endhighlight %}

It's seems nice but the fact is that we manipulate a Reader. This means that we can call
whatever functions the Reader supports on it. So if we want to prevent someone using our
code to call asks or local, we can't.

Wrapping a reader with `newtype` can prevents this from happening. If we wrap our Reader with
a newtype and if we export only our newtype then there's no way to call functions directly to
the reader.

Let's build our Context first (the `a` in `Reader a b`)
{% highlight haskell %}
-- file: Context.hs
import Data.Map as Map

module Context
( Context
, context
) where

type Context = Map String String

-- should read context from a file, let's just stubbed the data instead
context = Map.fromList [
  ("domain", "localhost"),
  ("port", "8888"),
  ("protocol", "http")
  ] :: Context
{% endhighlight %}

Now we need to make our Reader wrapper which we'll call Config

{% highlight haskell %}
-- file: Config.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config
( Config -- the type, not the constructor
, runConfig
, fromEnv
) where
-- we exported a type and 2 functions, users cannot see/call anything else

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Context

newtype Config a = C (Reader Context a)
  deriving (Functor, Applicative, Monad)

runConfig :: Config a -> Context -> a
runConfig (C reader) context =
  runReader reader context

fromEnv :: String -> Config String
fromEnv key = C $ do -- by doing that, we set a Reader context inside the do
  env <- ask         -- that's why we can call ask here
  return (fetch key env)

fetch :: String -> Context -> String
fetch key conf =
  fromJust $ Map.lookup key conf
{% endhighlight %}

And finally, in our **Main.hs** we can only call `runConfig`, `fromEnv` and
have no idea how is the underlying Config running.

{% highlight haskell %}
-- file: Main.hs
import Context
import Config

url :: Config String
url = do
  domain <- fromEnv "domain"
  port <- fromEnv "port"
  return (domain ++ ":" ++ port)

main =
  putStrLn $ runConfig url serverContext -- localhost:8888

{% endhighlight %}
