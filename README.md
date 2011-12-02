accelerate-opencl
=================
This is a partially implemented OpenCL backend for the Accelerate library
for General purpose GPU programming.
See: http://www.cse.unsw.edu.au/~chak/project/accelerate/

The backend is still in a very experimental stage and currently
completely lacks support of the following operations:

 * Higher dimension folds
 * Segmented folds
 * Scan (prefix sum) operations
 * Stencil operations
 * Array indexing

Installation
------------
The accelerate-opencl depends on the following two Cabal packages
which are not yet available on Hackage in recent enough versions.

**hopencl**

This library provides Haskell bindings to the OpenCL API, and is
available from http://github.com/HIPERFIT/hopencl

**language-c-quote (with OpenCL extensions)**

The version of language-c-quote currently available at Hackage does
not contain support for OpenCL C. These extensions can be found in the
most recent version of language-c-quote on Github:
https://github.com/mainland/language-c-quote

Examples
--------
The Accelerate team at UNSW provides a set of examples, available as a
Cabal-package at https://github.com/tmcdonell/accelerate-examples

Making a `cabal install --flags="-cuda"` (disabling the CUDA backend)
of this package, will install an executable which can be used to run a
suite of tests.

All the examples can then be executed by: `accelerate-examples --opencl -n 5000`

Which will execute all examples on 5000 element arrays. See
`accelerate-examples --help` for further instructions.

Questions and bug reports
-------------------------
Feel free to contact me by email at <dybber@dybber.dk> with any
questions or bug reports.

Alternatively, bugs can be submitted through the bug tracker at:
http://github.com/HIPERFIT/accelerate-opencl/issues

Acknowledgements 
----------------
The work on this backend was initially done as a graduate project at
the University of Copenhagen, and afterwards as a paid internship at
the HIPERFIT research center (http://hiperfit.dk).

I would like to thank Manuel Chakravarty, Trevor McDonell and the rest
of the team behind Accelerate. For comments on my work and for making
the necessary changes to Accelerate to accomodate a new backend.
