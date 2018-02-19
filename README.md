# BFComp

This is an optimizing Brainfuck compiler. It takes a file as an argument, and returns the optimized code on STDOUT.

In some cases this can disprove totality, in which case the non-total code will be replaced with `(infinite loop)`.
