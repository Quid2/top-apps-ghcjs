Example WWW applications for [quid2-net](https://github.com/tittoassini/quid2-net), using ghcjs.

Applications:
* [chat](app/Chat/chat.hs), also [live version](http://quid2.org/app/chat)
  * Simple chat application, developed using [ghcjs-dom](https://hackage.haskell.org/package/ghcjs-dom)
* [ui](app/UI/ui.hs), also [live version](http://quid2.org/app/ui)
  * Generic UI for quid2-net, show existing channels and types, developed using [react-flux](https://hackage.haskell.org/package/react-flux).


*** Installation 

... to be written

cd www/ui;make;cd ../..

*** NOTE
If you get an error similar to this:

```
user error (The package 'Cabal' requires Cabal library version -any && >=1.10
but no suitable version is installed.)        
```

Then you will first need to install the latest stack:

```
stack upgrade
```

If it still fails, get the latest from github:

```
git clone https://github.com/commercialhaskell/stack.git;cd stack;stack build;stack install
```



