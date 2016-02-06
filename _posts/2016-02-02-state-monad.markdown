---
layout: post
title:  "State Monad"
date:   2015-11-09
categories: haskell state monad
---

# State

---
<br>

> Useful to implicitely pass a read/write environement through multiple functions

Let's try to implement it first

**env → (value, env)**

{% highlight haskell %}
newtype State s a = State { runState :: s -> (a, s) }

state :: (env -> (val, env)) -> State env val
state = State

get :: State env env
get = state $ \env -> (env, env)

put :: env -> State env ()
put s = state (\_ -> ((), s))

instance Monad (State s) where
  -- Monad m => a -> m a
  -- a -> State s a
  return value = state $ \env -> (value, env)

  -- m a -> (a -> m b) -> m b
  -- State s a -> (a -> State s b) -> State s b
  m >>= k = state $ \s ->
    let (newValue, newEnv) = (runState m) s
    in (runState (k newValue)) newEnv
{% endhighlight %}

**Simple usage**

{% highlight haskell %}
foo :: State [Int] Int
foo = do
  a <- get       -- a = [0]
  put (1 : a)    -- s = [1, 0]
  b <- get       -- b = [1, 0]
  let c = head b -- c = 1
  return (c + 4)

main = do
  putStrLn $ show $ runState foo [0] -- (5,[1,0])
{% endhighlight %}

Let's have a more realistic example.
Say you have a Car model that has a tank and a current speed.
You want to be able to move it with the accelerate function. Accelerate should set the
speed to currentSpeed + acceleration and should also decrease the gaz if the acceleration is
positive.

Say you have gaz = 100 and speed = 0

Accelerate by 10 should give you gaz = 90 and speed = 10

Accelerate by -5 shouldnt modify the gaz but set the speed to 5

You could modelize it like that:
{% highlight haskell %}
run :: (Int, Int) -> (Int, Int)
run (initialGaz, initialSpeed) =
  let
    (newGaz, newSpeed) = acc 10 (initialGaz, initialSpeed)
    (newGaz2, newSpeed2) = acc 10 (newGaz, newSpeed)
    (newGaz3, newSpeed3) = acc (-5) (newGaz2, newSpeed2)
  in
    (newGaz3, newSpeed3)

acc :: Int -> (Int, Int) -> (Int, Int)
acc acc (prevGaz, prevSpeed) =
  (prevGaz - gazConsumed, newSpeed)
  where gazConsumed = if acc < 0 then 0 else acc
  newSpeed = prevSpeed + acc

main = do
  let
    initialGaz = 100
    initialSpeed = 0
  putStrLn $ show $ run (initialGaz, initialSpeed)
{% endhighlight %}

This is really tedious because with have to keep the new state and pass it around.
Like the reader monad, we will pass an implicit environment around but with the faculty of modify it.
Here comes a first implementation of the State with our example:

{% highlight haskell %}
run :: Int -> State Int Int
run initialSpeed =
  let initState = return (initialSpeed)
  in
    initState >>=
    acc 10 >>=
    acc 10 >>=
    acc (-2)

-- with do notation we can see that >>= is different than a <- ...
-- cause a <- someState will assign the value to a without the state
run2 :: Int -> State Int Int
run2 initialSpeed = do
  return (initialSpeed)
  newSpeed <- acc 10 initialSpeed
  newSpeed' <- acc 10 newSpeed
  newSpeed'' <- acc (-2) newSpeed'
  return newSpeed''

acc :: Int -> Int -> State Int Int
acc acc prevSpeed =
  state $ \env -> (newSpeed, env - gazConsumed)
  where gazConsumed = if acc < 0 then 0 else acc
        newSpeed = prevSpeed + acc

main = do
  let
    initialGaz = 100
    initialSpeed = 0
  putStrLn $ show $ runState (run initialSpeed) initialGaz
{% endhighlight %}
