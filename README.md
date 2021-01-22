# FLOWAMOK

A traffic simulation for the ages.  Maybe a game at some point.

It's a stencil and then some.  Work in progress!

Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
libraries with associated header files.


## Building and running

First run `futhark pkg sync` once.

Then run `make run` to build and run in a window.


## Controls

- Space: Step through the simulation manually.
- Left arrow key: Move left in the list of scenarios.
- Right arrow key: Move left in the list of scenarios.
- `a`: Toggle auto-stepping.
- Up arrow key: Increment the steps per second in the auto-stepping mode.
- Down arrow key: Decrement the steps per second in the auto-stepping mode.
- `r`: Reset the current grid and stop auto-stepping.

## What it looks like

```futhark
import "flowamok"

type~ state = lys.state
entry init : u32 -> i64 -> i64 -> state = lys.init
entry step (s: state) : ([][]u32, state) =
  (lys.render s,
   lys.event (#keydown {key=0x20}) s)
```

```
> :anim (step, init 123u32 1000i64 1000i64, 100i64)
```


![](README-img/anim4.gif)

