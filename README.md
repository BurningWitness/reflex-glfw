# reflex-glfw

A [reflex](http://hackage.haskell.org/package/reflex) host based on [GLFW-b](http://hackage.haskell.org/package/GLFW-b).

Abandoned due to me giving up on the idea of using reflex within a videogame:
the splitting of the control flow only really makes sense in systems where most, if not every,
action is reversible. Something as simple as creating and destroying windows becomes a
tremendous hassle when put into a system like this, with tons of side cases that never
even existed originally.
