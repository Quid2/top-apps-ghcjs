Example WWW applications for [top](https://github.com/tittoassini/top), using [ghcjs](https://github.com/ghcjs/ghcjs).

Applications:
* [chat](app/Chat/chat.hs) ([live version](http://quid2.org/app/chat))
  * Simple chat application, developed using [ghcjs-dom](https://hackage.haskell.org/package/ghcjs-dom)
* [ui](app/UI) ([live version](http://quid2.org/app/ui)).
  * Generic UI for top, shows existing channels and types, developed using [react-flux](https://hackage.haskell.org/package/react-flux).

### Installation 

Start by installing the [quid2](https://github.com/tittoassini/quid2) project, of which this is a sub-project, then switch to this project to continue the installation:

```
cd top-apps-ghcjs
```

#### GHCJS setup

Some versions of `stack` do not build `ghcjs` correctly so upgrade to the latest `stack`:

```
stack upgrade
```

Then to setup the ghcjs environment:

```
stack setup
```

This is going to take a looong time but needs to be done only once.

If you get an error similar to this:

```
user error (The package 'Cabal' requires Cabal library version -any && >=1.10
but no suitable version is installed.)        
```

Get the very latest `stack` from github, and retry:

```
git clone https://github.com/commercialhaskell/stack.git;cd stack;stack build;stack install
```

#### Development

To automatically recompile the code of one of the applications, after every change, for example for the `ui` app:

```
cd www/ui
make dev
```

You can then open and use locally the apps in your browser:
* `chat` at `www/chat/index-dev.html`
* `ui`at `www/ui/index-dev.html` 

##### Deployment

To produce a final, optimised version, ready for deployment on a web site. 

First of all, install the closure compiler:

```
npm install closurecompiler -g
```

Then: 

```
cd www/ui
make
```

You can then upload the app's `www` subdirectory to any web site and access the app via the `index.html` file, for example: [quid2.org/app/ui](http://quid2.org/app/ui).

