Example WWW applications for [quid2-net](https://github.com/tittoassini/quid2-net), using [ghcjs](https://github.com/ghcjs/ghcjs).

Applications:
* [chat](app/Chat/chat.hs) ([live version](http://quid2.org/app/chat))
  * Simple chat application, developed using [ghcjs-dom](https://hackage.haskell.org/package/ghcjs-dom)
* [ui](app/UI) ([live version](http://quid2.org/app/ui))
  * Generic UI for quid2-net, show existing channels and types, developed using [react-flux](https://hackage.haskell.org/package/react-flux).

### Installation 

To setup the ghcjs environment:

```
stack setup
```

This is going to take a loong time.

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

Once the setup has completed successfully, you can compile the project itself:

```
stack build
```

Finally, to link the compiler output into the `www` subdirectories:

```
make
```

You can now open the apps in your browser:
* `chat` at `www/chat/index.html`
* `ui`at `www/ui/index.html` 

### Development
To automatically recompile your code:
```
stack build --file-watch
```
