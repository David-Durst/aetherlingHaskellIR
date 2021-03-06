# Aetherling's Haskell SpaceTime Intermediate Representation
[![Build Status](https://travis-ci.com/David-Durst/aetherlingHaskellIR.svg?branch=master)](https://travis-ci.com/David-Durst/aetherlingHaskellIR)

To build this part of the project:
1. Install the latest version of [stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the [Aetherling Haskell IR repository](https://github.com/David-Durst/aetherlingHaskellIR)
3. Build the project:
```
stack build
```
4. Look at the examples in [src/examplesExe/Main.hs](src/examplesExe/Main.hs). The output of running this file is at [src/examplesExe/Main.output](src/examplesExe/Main.output).
5. Either 
    1. Run the produced binary to see all the examples and their space/time requirements. You can find the binary by looking at the output of the build command
    2. Load the package in the Haskell interpreter and try out your own examples. This can be done by running:
    ```
    stack ghci
    ```

# Notes:
run with the following to get profiling for debugging and memory analysis:
```
AetherlingSTIR-Example-exe +RTS -xc
```
