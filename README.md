haste-ffi-parser
================

Programm for easier writing ffi for the haste haskell to js compiler.

Writing ffi bindings for the hastec haskell to javascript compiler can be annoying (see [haste ffi doc][1]). haste-ffi-parser allows writing ffi bindings similar as for uhc.

In your hs file write:

```haskell
foreign import jscall "<js-expression>" hs-name :: Signature
```

Where <js-expression> is the javascript expression to be associated with the hs-name. When it contains "%N" (with N being a positive number), it is replaced with N-th parameter to the haskell function.
The hs-name is the name of the haskell function and Signature its signature.

Example:

```haskell
foreign import jscall "%1.prob = %2" setProb :: Obj -> Prop -> IO ()
```

Now convert your has file with:

```bash
haste-ffi-parser <source-file> <basename>
```

This creates <basename>.hs and <basename>.js. The second one must be included in the haste build with --with-js.

In addition to creating the javascript function, it also:

* Converts String to JSString
* Converts functions and IO monads to callbacks

[1]: https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt

