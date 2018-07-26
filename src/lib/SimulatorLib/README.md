# The Simulator

STSimulate.hs and the files in the SimulatorLib directory implement a
functional simulator for Aetherling pipelines: this simulator does not
concern itself with the actual timing of the circuit, but instead
works with "streams" of input/output values that have a precise
ordering but not precisely specified timing. This document describes:

1. How to use the simulator
2. How to extend the simulator with a combinational leaf op
3. Summary of simulator implementation
4. How to extend the simulator with a custom op



# How to Use the Simulator

simulateHighLevel (imported from STSimulate) is the function to call
for the simulator; all other functions defined are internal functions.
This function takes three arguments:
  a. The Op instance to simulate
  b. simulated input port values
  c. simulated memory input values
and returns a tuple of
  a. output port values
  b. memory output values
Except for the Op argument, all are lists-of-lists of ValueType.

The ValueType data type was created to facilitate passing data into
and out of the simulator. For each TokenType there is a corresponding
ValueType constructor that takes a single parameter for its value (Int
for V_Int, Bool for V_Bit, [ValueType] for V_Array; note that V_Array
does not have a length or type parameter).

The outer list of the input ports argument (or output ports return
value) corresponds to the list of input ports (or output ports), and
the inner list corresponds to the stream of values read from that
port. For example, in

> inputs = [[V_Int 0, V_Int 2, V_Int 4], [V_Int 30, V_Int 20, V_Int 10]]
> simulateHighLevel (Add T_Int) inputs [] -- Empty memory argument
([[V_Int 30,V_Int 22,V_Int 14]],[])

we simulate a 2-input-port adder, where the 0th input port gets 0, 2,
4 as its input stream and the 1st port 30, 20, 10. If this circuit
were timed at 1 output per clock cycle, then this simulation
corresponds to an adder calculating 0+30=30, 2+20=22, 4+10=14 on the
0th, 1st, and 2nd clock cycles.

As mentioned the simulator does not concern itself with exact timing,
so the lengths of the inner lists need not match. Example:

> add4stream = ArrayReshape [T_Int] [T_Array 1 T_Int] |>>=| ReduceOp 1 4 (Add T_Int)
> pipeline = (add4stream |&| Underutil 4 (Shl 4 T_Int)) |>>=| Underutil 4 (Max T_Int)
> :{
| inputs =
|         [ [V_Int 1, V_Int 2, V_Int 3, V_Int 4, V_Int 5, V_Int 6, V_Int 7, V_Int 8],
|           [V_Int 0, V_Int 10] ]
| :}
> simulateHighLevel pipeline inputs []
([[V_Int 10,V_Int 160]],[])

Here, we get [max (1+2+3+4) (0 shl 4), max (5+6+7+8) (10 shl 4)] as
the output stream. No action needed to be taken by the user to match
the V_Int 10 argument with the ...V_Int 5, V_Int 6, V_Int 7, V_Int 8]
arguments.

The memory input/output format is similar: the outer list entries
correspond to the MemRead/MemWrites and each inner list represents the
stream of values on one MemRead/MemWrite. The MemReads and MemWrites
are matched with outer list entries by the order that they would be
encountered by a depth first search of the pipeline AST. Example:

> :{
| pipeline =
|   (MemRead T_Bit |&| MemRead T_Bit) |>>=|
|   ArrayReshape [T_Bit, T_Bit] [T_Array 2 T_Bit] |>>=|
|   MapOp 2 ((MemRead T_Bit |&| Not T_Bit) |>>=| XOr T_Bit)
| inputs =
|           [ [V_Bit True, V_Bit False],
|             [V_Bit False, V_Bit True],
|             [V_Bit True, V_Bit False],
|             [V_Bit True, V_Bit False] ]
| :}
> simulateHighLevel pipeline [] inputs
([[V_Array [V_Bit True,V_Bit False],V_Array [V_Bit True,V_Bit False]]],[])

This pipeline compares memory inputs 0 and 2 and memory inputs 1 and 3
for equality, emitting the result of the 2 comparisons as a
2-array. The MemReads are numbered using the following logic:

ComposeSeq:
  ComposePar:
    Encounter 2 MemRead T_Bit: inputs 0 and 1.
  ArrayReshape: No MemReads.
  MapOp:
    Encounter 2 parallel copies of MemRead T_Bit: inputs 2 and 3.

This is not the most elegant solution to the issue of handling memory IO.

Note: Some Ops may not be simulated properly if one of their input
streams is empty (particularly combinational ones). Any Ops you
implement may do the same (but it's recommended that they report this
error at the preprocessing stage – see below).



# 2. How to Extend the Simulator with a Combinational Leaf Op.

Suppose that a new Op is added to STAST.hs and it needs to be
implemented in the simulator. This can easily be done without
understanding the simulator internals if the new Op is combinational
and contains no child Ops. Steps:

a. Write a [ValueType]->[ValueType] function that simulates the
   behavior of the Op (Foo) in one clock cycle: the input is a list of
   inputs for each input port and the output is similar. For example,
   conceptually the function for add should do [x, y] -> [x+y]. For
   certain patterns, helpers like simhlBinaryOp can further simplify
   this task. By convention the new function should be in a file in
   the SimulateLib directory and its name should be something like
   simhlFoo (simhl = simulate high level).

b. In the list of simhl pattern matches in STSimulate.hs, add an entry
   that looks something like

simhl Foo inStrs state = (simhlCombinational simhlFoo inStrs, state)

   Basically, for the first value of the tuple, simhlCombinational
   takes your function as an argument and adapts it to work with the
   simulator, and the second value is the state parameter passed
   through without modification (don't worry about it).

c. In the list of simhlPre pattern matches, add an entry that looks
   like

simhlPre opStack@(Foo:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState

   You don't need to implement any function yourself for this step; as
   far as the preprocessor is concerned, all combinational devices are
   just the same and the simhlPreCombinational function handles this.



# Summary of Simulator Implementation

When simulateHighLevel is called, it delegates its work to 3 functions:

a. simhlCheckInputs, which checks that the port inputs match with the
   inPorts of the simulated Op.

b. simhlPre, which recursively "preprocesses" the AST of the Op in order to:
   1. Calculate the intermediate stream lengths, using this for
      warnings about mismatched stream lengths and to find the maximum
      stream length, which is needed by the Constant_Int/_Bit ops.
   2. Check that the memory inputs match the types and count of
      MemRead ops.
   3. Check that the Ops in the pipeline are well-formed, producing
      warnings or errors if needed.
   This is all done by passing the SimhlPreState instance through
   recursive calls of simhlPre, recording warnings and maximum stream
   within.

c. simhl, which performs the actual simulation, calling itself
   recursively if needed. This function takes an input that is a
   list-of-lists of ValueType (just like the port inputs argument in
   simulateHighLevel) along with a mysterious state parameter, and
   returns a tuple of output list-of-lists and the same mystery state
   type.

At each stage we can make more assumptions about the pipeline: when
simhlPre is called we can assume that the ports match and the user
port inputs are correct, and when simhl is called we can skip most
error checking (note that the preprocessing step didn't exist for a
long time, so there's a lot of extra paranoid error checking).

In general, the state parameter will not need to be directly modified
by any call of simhl; it only needs to pass on the state parameter to
any recursive simhl calls on child Ops, and return the updated state
in case the child op did modify the state. The state's main job is to
pass out and collect MemRead/MemWrite data in the DFS order explained
earlier, and it does not represent any sense of "simulated circuit
state".

Exception: ReduceOp does not properly handle the state. This is why
it's not allowed to Reduce over a MemRead/MemWrite.



# How to Extend the Simulator with a Custom Op

To implement a new Op in the simulator, you need to implement a
preprocessor pass for the Op and implement the behavior of the
simulated Op.

Preprocessor pass:

The preprocessor pass function takes as input

a. A stack of Ops (as a list), the head of which is the Op being
   preprocessed. The idea is that each Op is the child of the next Op
   in the list, and the stack can be formatted in errors/warnings.
b. A list of expected input port stream lengths (as Maybe Int –
   explained later).
c. A SimhlPreState parameter, whose main job is to collect warnings
   and record the longest output stream length.

Your job is to calculate the tuple of return values:

d. The list of expected output port stream lengths.
e. The new SimhlPreState, updated as explained in (c) above. If there
   are any child ops to be simulated, the state must be folded through
   them all, i.e., each child op receives the state returned by the
   previous child op, the first receives the initial state, and the
   output state of the last child op (updated as in (c)) is the return
   value state of the function.

Fortunately there are helper functions (in SimulatorLib/State.hs) to
make this easier, but before that I need to explain the Maybe Int. The
input stream length might be indeterminate because it comes from a
constant generator (which can create an arbitrarily long repeating
output). In this case Nothing will be passed in the entry
corresponding to the input port wired to the constant. The functions
simhlMinStrLen and simhlMaxStrLen help you work around this.

The simhlPreResult function helps you return the output tuple. It
takes as arguments

f. The stack of Ops (for error reporting)
g. The list of expected output port stream lengths.
h. An optional warning to record in the state (as Maybe [Char]).
i. The output state of the last child op (or the input state if no
   child ops).

and produces the output tuple updated as explained above.

Once this function is implemented, add a pattern-match entry to
simhlPre in STSimulate.hs that dispatches to your function. You may
need to pass in the simhlPre function itself as an additional
parameter if you need to call it recursively in your function; note
that SimhlPre in SimulatorLib/State.hs is the type of simhlPre.

Example: implementation of preprocessor pass for MappedOp

simhlPreMap :: SimhlPre -> [Op] -> [Maybe Int] -> SimhlPreState
            -> ([Maybe Int], SimhlPreState)
simhlPreMap simhlPre opStack@(MapOp par op:_) inStrLens inState
(Some error checking by pattern matching omitted)
1   let
2     -- Fold lambda. Go through par copies of the op, threading the state
3     -- through each one and taking the minimum of their output stream lengths.
4     -- Note: they could be different due to MemRead.
5     f :: ([Maybe Int], SimhlPreState) -> Op -> ([Maybe Int], SimhlPreState)
6     f (fOutStrLens, fInState) op =
7       let
8         (thisOutStrLen, fOutState) = simhlPre (op:opStack)
9                                      inStrLens fInState
10        newOutStrLens = [simhlMinStrLen [a,b]
11                        | (a,b) <- zip thisOutStrLen fOutStrLens]
12      in
13        (newOutStrLens, fOutState)
14    
15    (outStrLens, newState) =
16      foldl f (replicate (length $ outPorts op) Nothing, inState)
17              (replicate par op)
18  in
19    simhlPreResult opStack outStrLens Nothing newState

Things to note:

Line 7: When we recursively call simhlPre on each copy of the child
mappedOp, the child op is prepended to the opStack to maintain (a).

Lines 6, 8, 16: The fold and fold lambda ensures that the new state returned
by the recursive simhlPre call is used as the input state for the next
recursive call (fInState -> fOutState)

Line 19: We use simhlPreResult at the end (using the new state
returned by the fold, not the original inState!) to update the
state. Nothing is passed as arg 3 since we don't detect any warnings
here.

Then, under simhlPre in STSimulate.hs, we have

simhlPre opStack@(MapOp _ _:_) inStrLens inState =
    simhlPreMap simhlPre opStack inStrLens inState

which dispatches to the earlier function, passing in simhlPre itself
since the map preprocessor pass calls simhlPre recursively.

Simulator pass:

The simulator pass function takes as input

a. A list of lists of ValueType, in the same order as in simulateHighLevel.
b. A SimhlState parameter, which in most cases just has to be folded
   through all simhl calls on child ops.

and returns as an output tuple.

a. A list of lists of ValueType for output port values.
b. Updated SimhlState parameter.

Once the function is implemented, add a pattern-match entry under
simhl in STSimulate.hs. Similar to before, you may need to pass the
simhl function as an additional parameter (of type Simhl).

Example: ComposePar simulator implementation

simhlPar :: Simhl -> Op -> [[ValueType]] -> SimhlState
         -> ( [[ValueType]], SimhlState )
simhlPar simhl (ComposePar []) inStrs state = ([], state)
simhlPar simhl (ComposePar (op:moreOps)) inStrs inState =
1   let
2     (opInStrs, moreInStrs) = splitAt (length $ inPorts op) inStrs
3     (opOutStrs, nextState) = simhl op opInStrs inState
4     (moreOutStrs, endState) = simhl (ComposePar moreOps) moreInStrs nextState
5   in
6     (opOutStrs ++ moreOutStrs, endState)

Things to note:

Line 2: Before calling simhl recursively on the composed child op, we
need to collect the child's input. Since the outer dimension of
inStrs :: [[ValueType]] corresponds to input ports, we can just
collect the first (child op inPort count) entries of the list to get
the child's list of input streams.

Lines 3, 4, 6: Note how the state is carefully passed through the
calls of simhl of child ops (some done through a recursive call on
simhl ComposePar), named as inState, nextState, and finally endState.

Then, in the simhl function, we have

simhl op@(ComposePar _) inStrs state =
    simhlPar simhl op inStrs state

Similar to before, simhl itself must be passed as a parameter.

If you get weird errors involving MemRead or MemWrite, that probably
means that the state wasn't properly passed through your simulated op,
e.g. you accidentally recycled an old State value.

