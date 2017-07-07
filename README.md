[![Build Status](https://travis-ci.org/chpatrick/servant-generic.svg?branch=master)](https://travis-ci.org/chpatrick/servant-generic) [![Hackage](https://img.shields.io/hackage/v/servant-generic.svg)](http://hackage.haskell.org/package/servant-generic)

tl;dr
-----
Specify Servant APIs with simple records instead of `:<|>` trees.

```haskell
data Site route = Site
  { about :: route :-
      "about" :> Get '[PlainText] Text
  , faq :: route :-
      "faq" :> Get '[PlainText] Text
  } deriving Generic

siteServer :: Site AsServer
siteServer = Site
  { about = return "about"
  , faq = return "faq"
  }

type Api = ToServant (Site AsApi)

main :: IO ()
main = run 31337 $ serve (Proxy :: Proxy Api) (toServant siteServer)
```

So long and thanks for all the `:<|>`
-------------------------------------
Servant is great for building typesafe APIs. However, its biggest weakness is that you need to specify APIs as a tree of `:<|>` operators:

```haskell
type SiteOld =
       "about" :> Get '[PlainText] Text
  :<|> "faq" :> Get '[PlainText] Text
  :<|> "subsite" :> SubSiteOld

type SubSiteOld =
       "echo" :> Capture "x" Text :> Get '[PlainText] Text
  :<|> "timestwo" :> Capture "x" Int :> Get '[PlainText] Text

siteOldServer :: Server SiteOld
siteOldServer =
       return "about"
  :<|> subSiteOldServer
  :<|> return "faq"

subSiteOldServer :: Server SubSiteOld
subSiteOldServer =
       return
  :<|> (\x -> return $ T.pack $ show (x * 2))
```

This is cumbersome because you need to remember the exact order of routes when you are implementing the API. Not only that, but if you get anything wrong, you are treated with an incomprehensible error message. Let's see what happens if we mix up the implementations of `/faq` and `/subsite`:

```
    • Couldn't match type ‘(Text
                            -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                 ServantErr IO Text)
                           :<|> (Int
                                 -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                      ServantErr IO Text)’
                     with ‘transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                             ServantErr IO Text’
      Expected type: Server SiteOld
        Actual type: transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                       ServantErr IO Text
                     :<|> (Server SubSiteOld
                           :<|> ((Text
                                  -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                       ServantErr IO Text)
                                 :<|> (Int
                                       -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                            ServantErr IO Text)))
    • In the expression:
        return "about" :<|> subSiteOldServer :<|> return "faq"
      In an equation for ‘siteOldServer’:
          siteOldServer
            = return "about" :<|> subSiteOldServer :<|> return "faq"
```

Can you figure out what the problem is from that?

What this package lets you do is to specify routes as regular Haskell records:

```haskell
data Site route = Site
  { about :: route :-
      "about" :> Get '[PlainText] Text
  , faq :: route :-
      "faq" :> Get '[PlainText] Text
  , subSite :: route :-
      "subsite" :> ToServant (SubSite AsApi) -- record APIs can be nested easily
  } deriving Generic

data SubSite route = SubSite
  { echo :: route :-
      "echo" :> Capture "x" Text :> Get '[PlainText] Text
  , timesTwo :: route :-
      "timestwo" :> Capture "x" Int :> Get '[PlainText] Text
  } deriving Generic

type Api = ToServant (Site AsApi)

subSiteServer :: SubSite AsServer
subSiteServer = SubSite
  { echo = return
  , timesTwo = \x -> return $ T.pack $ show (x * 2)
  }

siteServer :: Site AsServer
siteServer = Site
  { about = return "about"
  , faq = return "faq"
  , subSite = toServant subSiteServer
  }
```

Now everything is named so we don't need to remember which route is which.
These records can be converted to Servant-compatible types like this:
```haskell
ToServant (MyApiRecord AsApi)
```

and to Servant-compatible implementations like this:

```haskell
toServant myApiRecordServer
```

These functions work with any library based on Servant, not just `servant-server`.

What happens if we make the same mistake as above?

```
    • Couldn't match type ‘(Text
                            -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                 ServantErr IO Text)
                           :<|> (Int
                                 -> transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                                      ServantErr IO Text)’
                     with ‘transformers-0.5.2.0:Control.Monad.Trans.Except.ExceptT
                             ServantErr IO Text’
      Expected type: AsServer :- ("faq" :> Get '[PlainText] Text)
        Actual type: ToServant (SubSite AsServer)
    • In the ‘faq’ field of a record
      In the expression:
        Site
          {about = return "about", faq = toServant subSiteServer,
           subSite = return "faq",
           home = return "So long and thanks for all the :<|>"}
      In an equation for ‘siteServer’:
          siteServer
            = Site
                {about = return "about", faq = toServant subSiteServer,
                 subSite = return "faq",
                 home = return "So long and thanks for all the :<|>"}
```
Now, GHC tells us that the problem is in the `faq` field, and that we passed a `SubSite` route rather than an `"faq"` route.

This approach is based on my [solga](https://github.com/chpatrick/solga) library, which also has these benefits and is a lot simpler than Servant.