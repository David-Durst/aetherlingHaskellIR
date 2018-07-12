module Main where
import Examples

main = do
  describeMethod "basic combinonal adder" combinationalAdd
  describeMethod "fully parallel 4 ints per clock reduce" reduce44 
  describeMethod "register delayed, fully parallel 4 ints per clock reduce"reduce44Delayed 
  describeMethod "fully sequential 4 ints per 4 clocks reduce" reduce41
  describeMethod "4 ints per clock map" map4
  describeMethod "1 pixel per clock, 3 pixel stencil linebuffer" lb13
  describeMethod "2 pixels per clock, 3 pixel stencil linebuffer" lb23
  describeMethod "underutilized to only every other clock - 1 pixel per clock, 3 pixel stencil linebuffer" lb13Underutil
  describeMethod "back-to-back 1 pixel per clock, 3 pixel stencil linebuffers" lbChain
  describeMethod "basic memory reading one int per clock" memReadInt
  describeMethod "basic memory writing one int per clock" memWriteInt
  describeMethod "A reshape converting int[2]{1} to int{2} every two clocks" spaceAndTimeReshape
  describeMethod "Reshape converting int[3]{1} to int{3} every three clocks and a an underuitilized constant generator to feed it" constantSpaceTimeReshape
  describeMethod "duplicating the outputs but not the inputs of an adder" duplicateAdd
  describeMethod "1 pixel per clock 3 pixel stencil convolution" conv1PxPerClock
