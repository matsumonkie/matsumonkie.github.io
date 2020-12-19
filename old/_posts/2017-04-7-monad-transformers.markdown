---
layout: post
title:  "Monad Transformers"
date:   2017-04-07
categories: haskell monad
---

When working with monads, we often find the need to use multiple of them at the same time. For example, you might want to modify a state and write to a log inside a function.
To be able to access multiple monads we need to stack them. This is possible thanks to Monad Transformers

Our example program will be a translator. It prompts the user a word, find its translation and display it. We also want to log all actions done by the program and display possible errors. So we will need to work with the Except, IO, Writer monads.
Because we need to stack them, we will work with their transformers equivalent (ExceptT, WriterT).

Let's start by defining the necessary setup

**setup**
{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Reader


translations :: M.Map T.Text T.Text
translations = M.fromList [ ("butler", "majordome")
                          , ("potato", "patate")
                          ]

data UserError = TranslationNotFound
               | NoInput
{% endhighlight %}

Now this is the program

**program**
{% highlight haskell %}
main :: IO ()
main = do
  logs <- execWriterT $ runExceptT translateDialog
  TIO.putStrLn $ T.append "logs: " $ T.intercalate " -> " logs
  return ()

{-
translate dialog stacks up ExceptT WriterT and IO monads
by default, we can only work on the top layer (ExceptT) which
is the one who will return the correct type signature. If we
want to work with an inner stack, we will have to lift it so
that it returns the correct type signature.
-}
translateDialog :: ExceptT UserError (WriterT [T.Text] IO) ()
translateDialog = do
  translated <- (promptWord >>= translate) `catchError` errorHandler
  liftIO $ TIO.putStrLn $ wrapText translated
  return ()
  where
    wrapText text = "[" <> text <> "]" -- Monoid mappend (<>) to concat T.Text

promptWord :: ExceptT UserError (WriterT [T.Text] IO) T.Text
promptWord = do
  {-
    (tell [""]) :: WriterT [T.Text] m0 ()
    so we need to lift it to the ExceptT monad like:
    lift $ tell ["/prompting"]
    now because we have only one Writer monad in our stack
    lifting is optional here
  -}
  tell ["prompting"]

  {-
    With the IO monad, we can also lift it the same way we did
    with the writer but this time its even deeper in the stack
    so we need to lift it twice (lift $ lift $ TIO.putStr.....).
    However, the IO monad is a bit different and we are given
    the possibility to use liftIO wich automatically does the job :-)
  -}
  liftIO $ TIO.putStr $ "which word to translate: "
  line <- liftIO $ TIO.getLine
  if T.null line then
    throwError NoInput
  else
    return line

translate :: T.Text -> ExceptT UserError (WriterT [T.Text] IO) T.Text
translate word = do
  tell ["translating"]
  -- maybe :: (default :: b) -> (function :: a -> b) -> (value :: Maybe a) -> b
  maybe (throwError TranslationNotFound) return translation
  where
    translation = M.lookup word translations

errorHandler :: UserError -> ExceptT UserError (WriterT [T.Text] IO) T.Text
errorHandler error = do
  liftIO $ TIO.putStrLn $ case error of
    NoInput -> "no input :("
    TranslationNotFound -> "translation not found :("
  throwError error
{% endhighlight %}
