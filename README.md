Example WWW applications for [quid2-net](https://github.com/tittoassini/quid2-net), using ghcjs.

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



