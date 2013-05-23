haste-ffi-parser
================

Programm for easier writing ffi for the haste haskell to js compiler (and potentially also for other compilers in the future).

Writing ffi bindings for the hastec haskell to javascript compiler can be annoying (see [haste ffi doc][1]). haste-ffi-parser allows writing ffi bindings similar as for uhc.

In your hs file write:

```haskell
foreign import cpattern "<js-expression>" hs-name :: Signature
```

Where <js-expression> is the javascript expression to be associated with the hs-name. When it contains "%N" (with N being a positive number), it is replaced with N-th parameter to the haskell function.
The hs-name is the name of the haskell function and Signature its signature.

Example:

```haskell
foreign import jscall "%1.prob = %2" setProb :: Obj -> Prop -> IO ()
```

Now convert your has file with:

```bash
haste-ffi-parser -i <source-file> -o <haskell-output-file> -j <javascript-output-file>
```

This creates <haskell-output-file>.hs and <javascript-output-file>.js. The second one must be included in the haste build with --with-js.

In addition to creating the javascript function, you can also

* Converts functions and IO monads to callbacks
* Define conversion function for types haste handles different than javascipt.

To define a conversion function, create a file and write a list of 3-tuples of strings with

* The Name of the type
* The name function to convert haskell -> javascript
* The name of the function to convert javascript -> haskell

The list as to be written as a haskell "[(String,String,String)]".
For example, if you want to convert strings with "fromJS" and "toJS", your conversion file would be:

```
[("String","fromJS","toJS")]
```

Now you run

```bash
haste-ffi-parser -c <conversion-file> -i <source-file> -o <haskell-output-file> -j <javascript-output-file>
```


[1]: https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt

