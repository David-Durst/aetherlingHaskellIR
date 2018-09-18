# Aetherling Simulated Apps

Except for `SimpleExamples`, these directories hold Haskell main
functions that instantiate an Aetherling pipeline from the `../Apps`
directory and run the functional simulator on the pipeline. The input
and output gets read/written to images in the `Images` directory.

For now, to run them, make sure that you've run `stack build` and
`stack install`, then, with the current directory being this
directory, run one of the executables built (see output of `stack
install` to see which directory the executables ended up in).
