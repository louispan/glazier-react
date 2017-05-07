[![Hackage](https://img.shields.io/hackage/v/glazier-react.svg)](https://hackage.haskell.org/package/glazier-react)
[`Glazier.React`](https://github.com/louispan/glazier-react) contains efficient haskell bindings to React JS where render will only be called for the react components with changed states.

It uses the haskell [glazier](https://github.com/louispan/glazier) library to enable composable windows

# Features

## Efficient rendering
Using `React.PureComponent` and react `render` will only be called for component who's state actually changed, instead of requiring react to diff the entire DOM.

## Composable widgets
[`Glazier`](https://github.com/louispan/glazier) allows disciplined and lawful ways of creating composable widgets. Larger  can be created out of other widgets without modifying existing widget code, or manual ["lifting state"](https://facebook.github.io/react/docs/lifting-state-up.html) into larger widgets.

For example, [List Widget](https://github.com/louispan/glazier-react-widget/blob/master/src/Glazier/React/Widgets/List.hs) creates a list of any other widget.

## Isolation of IO
The  stateful effects are pure and do not involve IO. This has the benefit of allowing better testing of the intention of gadgets; increasing confidence of the behaviour of the gadget,  reducing the surface area of IO misbehaviour.

There are only two places where IO is allowed:
1. in the gadget `Command` interpreter,
2. in the event callback handlers, due to the need to read properties from `javascript` and dispatching `Actions`. Besides dispatching `Actions`, it is bad practice to create any other observable side effects in event handlers.

## Combine multiple concurrent stateful effects
AFAIK, Haskell is the only language where you can [combine multiple concurrent stateful effects consistently.](https://github.com/louispan/glazier#combine-multiple-concurrent-stateful-effects)

## Compile using GHC as well as GHCJS
`Glazier.React` uses [ghcjs-base-stub](https://github.com/louispan/ghcjs-base-stub) allows compiling GHCJS projects using GHC, which means you can develop using intero.

## Easier management of GHCJS callbacks
`Glazier.React` uses [disposable](https://github.com/louispan/disposable) to ease cleanup of GHCJS callbacks. It also uses a Free Monad [`Maker`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Maker.hs) DSL to ease creation of callbacks for widgets.

## blaze/lucid style do notation
React elements can be coded using blaze/lucid-style `do` notation using [`ReactMlT`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Markup.hs)

## Haskell-driven rendering
All state and processing is in Haskell, meaning only a simple shim `React.Component` is required. This  reduces the amount of `javascript` required and reduces the need for complex stateful integration with `React`.

# Examples

## TodoMVC
This is a fully featured TodoMVC in in Haskell and ReactJS using the [glazier-react](https://github.com/louispan/glazier-react) library.

For a live demo, see https://louispan.github.io/glazier-react-examples/

For more details, see the [todo example README.md](https://github.com/louispan/glazier-react-examples/tree/master/examples/todo)

# Documentation

## React
* Please refer to [react docs](https://facebook.github.io/react/docs). You only need to read up to [handling events](https://facebook.github.io/react/docs/handling-events.html).
* Also read [Lists and Keys](https://facebook.github.io/react/docs/lists-and-keys.html), and [Refs and the DOM](https://facebook.github.io/react/docs/refs-and-the-dom.html).
* Ignore controlled input in [Forms](https://facebook.github.io/react/docs/forms.html). In my experience, controlled input is error-prone and it is better to use it uncontrolled.
    * Using uncontrolled input doesn't stop you from subscribing to onChange and obtaining the latest value of the input. Just do not force a render with react[`setState`](https://facebook.github.io/react/docs/react-component.html#setstate).

## Glazier
Please read the [README.md](https://github.com/louispan/glazier) for a brief overview of glazier.

## Markup
[`Glazier.React.Markup`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Markup.hs) is a StateT monad that enables blaze/lucid style `do` notation to markup React elements to render.

```
bh (strJS "footer") [("className", strJS "footer")] $ do
    bh (strJS "span") [ ("className", strJS "todo-count")
                      , ("key", strJS "todo-count")] $ do
            bh (strJS "strong") [("key", strJS "items")]
                (s ^. activeCount . to (txt . pack . show))
            txt " items left"
```

## Event handling
React re-uses SyntheticEvent from a pool, which means it may no longer be valid if we lazily parse it. However, we still want lazy parsing so we don't parse unnecessary fields.

Additionally, we don't want to block during the event handling.The reason this is a problem is because Javascript is single threaded, but Haskell is lazy.
Therefore GHCJS threads are a strange mixture of synchronous and asynchronous threads, where a synchronous thread might be converted to an asynchronous thread if a "black hole" is encountered.
See https://github.com/ghcjs/ghcjs-base/blob/master/GHCJS/Concurrent.hs

[`Glazier.React.Event`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Event.hs) uses the event handling idea from the haskell [`react-flux`](https://hackage.haskell.org/package/react-flux/docs/React-Flux-PropertiesAndEvents.html) library to allow lazy parsing of event safely.

Event handling should only be done via [`eventHandler` or `eventHandlerM`](https://github.com/louispan/glazier-react/blob/33f4e244cff1a3e98ee1845f9ae2392818b9e512/src/Glazier/React/Event.hs#L90).

```
eventHandlerM :: (Monad m, NFData a) => (evt -> m a) -> (a -> m b) -> (evt -> m b)
```

This safe interface requires two input functions:
1. a function to reduce SyntheticEvent to a NFData. The mkEventCallback will ensure that the NFData is forced which will ensure all the required fields from Synthetic event has been parsed. This function must not block.
2. a second function that uses the NFData. This function is allowed to block.

 mkEventHandler results in a function that you can safely pass into 'GHC.Foreign.Callback.syncCallback1' with 'GHCJS.Foreign.Callback.ContinueAsync'.

## Simple and efficient React.Component integration
`Glazier.React` only uses `ReactJS` as a thin layer for rendering and registering event handlers.  All state and event processing are performed in Haskell, which means only a simple shim `React.PureComponent` is required.

Only [one shim React component](https://github.com/louispan/glazier-react/blob/master/jsbits/react.js) is ever used and the only methods are required are  [`setState`](https://facebook.github.io/react/docs/react-component.html#setstate),  [`render`](https://facebook.github.io/react/docs/react-component.html#render) and [`componentDidUpdate`](https://facebook.github.io/react/docs/react-component.html#componentdidupdate),

The shim component only has one thing in it's state, a sequence number. This sequence number is only changed with [`setState`](https://facebook.github.io/react/docs/react-component.html#setstate) when the `Glazier.Gadget` determined that there is a need for re-rendering. This is easy and efficient to determine since `Gadget` is the `StateT` responsible for changing the state in the first place.

This has the benefits of:
* Only the react shim components with changed haskell state will be re-rendered.
* React is able to efficiently determine if state has changed (just a single integer comparison)
* The shim React component is very simple.

## Modelling
[`Glazier.React.Model`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Model.hs) contain many nuanced concepts of Model.

### Schema
The `Schema` is a template of the pure data for stateful logic (the nouns). It is parameterized by a type variable which specializes it to either an `Outline` or 'Model'.

### Outline
The `Outline` is the pure data for stateful logic (the nouns). It may contain 'Outline's of child widgets.
The `Outline` does not contain enough information for rendering the child widgets.

### Model
The `Model` is similar to `Outline`, except that it may also contain `Gizmos` of child widgets.
It may contain `Gizmo` (see below) of other widgets.
The `Model` contains enough information to render child widgets, but not this widget.

### Plan
The `Plan` contains the callbacks for integrating with React (the verbs). It also contains a javascript reference to the instance of shim component used for the widget. This reference is used to trigger rendering with  [`setState`](https://facebook.github.io/react/docs/react-component.html#setstate).

### Scene
`Scene` is basically a tuple of `Model` and `Plan`. It is a separate data type in order to generate convenient lenses to the fields.
`Scene` is all that a `Window` needs to purely generate rendering instructions.

### Frame
`Frame` is a type synonym of `MVar Scene`. It is a mutable holder of a copy of `Scene`. This is so how the official state from Haskell is communicated to the React [`render`](https://facebook.github.io/react/docs/react-component.html#render) callback. The [`render`](https://facebook.github.io/react/docs/react-component.html#render) callback will read the latest copy of `Scene` from the `MVar` and pass it to the widget `Window` for rendering.

### Gizmo
`Gizmo` is basically a tuple of `Scene` and `Frame`. It is a separate data type in order to generate convenient lenses to the fields.
This contains everything a widget needs for rendering and state processing.
Most state processing is performed using the pure `Model`. The `Frame` is only used for the `RenderCommand`, to put the latest `Scene` into the `Frame` when re-rendering is required.

## Maker
`MVars` for `Frame`s and `Callback`s for `Plan`s may only be created in IO.  Using Free Monads, [`Glazier.React.Maker`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Maker.hs) provides a safe way to create them without allowing other arbitrary IO.

The `Maker` can also be used create the initial `Gizmo` state for the widgets.
The `Maker` DSL has an `action` type parameter which indicated the type of action that is dispatched by the widget.
The `action` type can be mapped and hoisted to a larger `action` type, allow for embedding the smaller widget action in larger widget actions.

## Disposable
GHCJS `Callback`s has resources that are not automatically collected by the garbage collector. `Callback`s need to be released manually. The [disposable](https://github.com/louispan/disposable) library provides a safe and easy way to convert the `Callback` into a storable `SomeDisposable` that can be queued up to be released after the next rendering frame.

[disposable](https://github.com/louispan/disposable) allows generic instances of `Disposing` to be easily created, which make it easy to create instances of `Disposing` for a `Plan` of `Callback`s,  and therefore for the parent container `Scene`, `Gizmo`, and `Model` (which may contain other widget `Gizmo`s)

The [`List` widget](https://github.com/louispan/glazier-react-widget/blob/54a771f492b864ff422e31949284ea4b23aa02c6/src/Glazier/React/Widgets/List.hs#L181) shows how the disposables can be queued for destruction after the next rendered frame.

## Widget
A [`Glazier.React.Widget`](https://github.com/louispan/glazier-react/blob/master/src/Glazier/React/Widget.hs) is the combination of:
The `Maker` instruction on how to create the `Model` of that widget from an `Outline`:
```
mkModel :: Outline -> F (Maker Action) Model
```
The `Maker` instruction on how to create the `Plan` of that widget:
```
mkPlan :: Frame Model Plan -> F (Maker Action) Plan
```
The rendering instructions for that widget:
```
window:: WindowT (Scene Model Plan) ReactMl ()
```
The state changes from `Action` events:
```
gadget :: Gadget () Action (Gizmo Model Plan) (DList Command)
```
This is everything you need in order to serialize, deserialize, create, render and interact with a widget.

`Glazier.React.IsWidget` is a typeclass that provides handy XXXOf type functions to get to the type of `Command`, `Action`, `Model`, `Plan` of the Widget. It also ensures that the `Model` and `Plan` is an instance of `Disposing`.


This is useful for creating widgets that is composed of other Widgets.

## Widget best practices
Please refer to [`glazier-react-widget`](https://github.com/louispan/glazier-react-widget) for documentation on the best practices for creating `Glazier.React.Widgets`
