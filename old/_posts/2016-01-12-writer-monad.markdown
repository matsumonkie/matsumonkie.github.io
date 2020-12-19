---
layout: post
title:  "Writer Monad"
date:   2015-11-04
categories: haskell writer monad
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
import Control.Monad.Writer

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
{% endhighlight %}

Which can be called like this:

{% highlight haskell %}
run :: MyWriter
run =
  engineOn >>
  setSpeed 10 >>= \currentSpeed ->
  tell ["test"] >>
  accelerate 10 currentSpeed >>
  engineOff >>=
  \x -> writer ((x+1) * 10000, ["car bug?"])
  -- WriterT (Identity (10000,["engine on","set speed at: 10","test","accelerating at: 100","engine off","car bug?"]))

-- with do notation it's prettier but less understandable IMO
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
