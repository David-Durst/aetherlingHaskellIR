module Aetherling.Simulator.Combinational (
    simhlCombinational,
    simhlBinaryIntOp,
    simhlBinaryBitOp,
    simhlUnaryIntOp,
    simhlUnaryBitOp,
    simhlIntCmpOp,
    simhlPreCombinational
) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Simulator.State

-- | Helper function for simulating combinational devices.  Takes an
-- implementation function ([ValueType]->[ValueType]) and a list of
-- lists of ValueType in the usual format for
-- simulateHighLevel. Implementation function takes a list with
-- entries corresponding to input ports' inputs in 1 cycle and
-- produces list of output ports' outputs.
simhlCombinational :: ([ValueType]->[ValueType]) -> [[ValueType]] -> [[ValueType]]
simhlCombinational impl inStrs | any null inStrs =
  error "Aetherling internal error: cannot simulate 0-input-stream device."
simhlCombinational impl inStrs | any (null . tail) inStrs =
  let
    inputsNow = map head inStrs
    outputsNow = impl inputsNow
  in
    [[outputNow] | outputNow <- outputsNow]
simhlCombinational impl inStrs =
  let
    inputsNow = map head inStrs
    inputsLater = map tail inStrs
    outputsNow = impl inputsNow
    outputsLater = simhlCombinational impl inputsLater
  in
    [outputNow:outputLater
    |(outputNow, outputLater) <- zip outputsNow outputsLater]

-- | Given an Int -> Int -> Int implementation function, create a
-- [ValueType] -> [ValueType] function suitable for
-- simhlCombinational.
simhlBinaryIntOp :: (Int -> Int -> Int) -> [ValueType] -> [ValueType]
simhlBinaryIntOp intImpl [V_Unit, _] = [V_Unit]
simhlBinaryIntOp intImpl [_, V_Unit] = [V_Unit]
simhlBinaryIntOp intImpl [V_Int x, V_Int y] = [V_Int $ intImpl x y]
simhlBinaryIntOp _ _ = error "Aetherling internal error: binary op no match"

-- | Given a Bool -> Bool -> Bool implementation function, create a
-- [ValueType] -> [ValueType] function suitable for
-- simhlCombinational.
simhlBinaryBitOp :: (Bool -> Bool -> Bool) -> [ValueType] -> [ValueType]
simhlBinaryBitOp bitImpl [V_Unit, _] = [V_Unit]
simhlBinaryBitOp bitImpl [_, V_Unit] = [V_Unit]
simhlBinaryBitOp bitImpl [V_Bit x, V_Bit y] = [V_Bit $ bitImpl x y]
simhlBinaryBitOp _ _ = error "Aetherling internal error: binary op no match"

-- | Given an Int -> Int implementation function, create a
--[ValueType] -> [ValueType] function suitable for simhlCombinational.
simhlUnaryIntOp :: (Int -> Int) -> [ValueType] -> [ValueType]
simhlUnaryIntOp intImpl [V_Unit] = [V_Unit]
simhlUnaryIntOp intImpl [V_Int x] = [V_Int $ intImpl x]
simhlUnaryIntOp _ _ = error "Aetherling internal error: unary op no match"

-- | Given a Bool -> Bool implementation function, create a
--[ValueType] -> [ValueType] function suitable for simhlCombinational.
simhlUnaryBitOp :: (Bool -> Bool) -> [ValueType] -> [ValueType]
simhlUnaryBitOp bitImpl [V_Unit] = [V_Unit]
simhlUnaryBitOp bitImpl [V_Bit x] = [V_Bit $ bitImpl x]
simhlUnaryBitOp _ _ = error "Aetherling internal error: unary op no match"

-- | Given an (Int -> Int -> Bool) implementation function,
-- create a [ValueType] -> [ValueType] function suitable for simhlCombinational.
simhlIntCmpOp :: (Int -> Int -> Bool) -> [ValueType] -> [ValueType]
simhlIntCmpOp intImpl [V_Unit, _] = [V_Unit]
simhlIntCmpOp intImpl [_, V_Unit] = [V_Unit]
simhlIntCmpOp intImpl [V_Int x, V_Int y] = [V_Bit $ intImpl x y]
simhlIntCmpOp _ _ = error "Aetherling internal error: int cmp op no match"

-- | As far as the preprocessor pass is concerned, basically all combinational
-- devices are the same. They just produce output streams as long as their
-- shortest input stream. Combinational ops can just delegate to this
-- function for their preprocessor pass (as long as they have no child ops).
-- See README.md for elaboration.
simhlPreCombinational :: [Op] -> [Maybe Int] -> SimhlPreState
                      -> ([Maybe Int], SimhlPreState)
simhlPreCombinational [] _ _ =
    error "Aetherling internal error: simhlPreCombinational empty opStack."
simhlPreCombinational opStack@(op:_) inStrLens inState =
    let
      outPortCount = length $ outPorts op
      outStrLens = replicate outPortCount (simhlMinStrLen inStrLens)
      knownStrLens = filter (/=Nothing) inStrLens
      warning' =
        if null knownStrLens || all (== (head knownStrLens)) knownStrLens then Nothing
        else Just $ "Input stream lengths don't match " ++ show inStrLens
    in
      if any (== Just 0) inStrLens
      then error("Cannot have any 0-length input streams at\n" ++
                simhlFormatOpStack opStack)
      else simhlPreResult opStack outStrLens warning' inState


