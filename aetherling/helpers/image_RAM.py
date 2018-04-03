from PIL import Image
from bitarray import bitarray
from mantle.coreir.memory import CoreirMem, getRAMAddrWidth
from magma import *
from mantle.common.countermod import SizedCounterModM
from functools import lru_cache
from typing import Callable
from io import BytesIO
import math

BITS_PER_PIXEL_BAND = 8

class IMGData:
    def __init__(self, img, imgAsBits, pxPerClock):
        self.imgAsBits = imgAsBits
        self.bitsPerPixel = int(len(img.getbands())*BITS_PER_PIXEL_BAND)
        self.bitsPerRow = int(self.bitsPerPixel*pxPerClock)
        self.numRows = int(len(imgAsBits) / self.bitsPerRow)
        assert len(imgAsBits) % self.bitsPerRow == 0, \
            "Can't evenly divide the image into rows of " \
            "bits using the pxPerClock %i" % pxPerClock

# only load all used images once, keep them in RAM for run
@lru_cache(maxsize=None)
def loadImage(imgSrc, pxPerClock):
    img = Image.open(imgSrc)
    imgAsBits = bitarray(endian='little')
    imgAsBits.frombytes(img.tobytes())
    return IMGData(img, imgAsBits, pxPerClock)

def RAMInterface(imgSrc, memoryInput, memoryOutput, pxPerClock):
    imgData = loadImage(imgSrc, pxPerClock)
    returnedInterface = []
    if memoryInput:
        returnedInterface += ["input_wdata", In(Bits(imgData.bitsPerRow)),
                              "input_wen", In(Bit), "input_ren", In(Bit)]
    if memoryOutput:
        returnedInterface += ["output_rdata", Out(Bits(imgData.bitsPerRow)),
                              "output_ren", In(Bit)]
    return returnedInterface

def InputImageRAM(cirb, circuit, nextNodeInput, imgSrc, pxPerClock):
    imgData = loadImage(imgSrc, pxPerClock)
    imgRAM = CoreirMem(cirb, imgData.numRows, imgData.bitsPerRow)

    # this counter ensures writing to correct address always
    writeCounter = SizedCounterModM(imgData.numRows, has_ce=True)
    # this counter ensures reading from the right address
    readCounter = SizedCounterModM(imgData.numRows, has_ce=True)

    wire(writeCounter.O, imgRAM.waddr)
    wire(circuit.input_wdata, imgRAM.wdata)
    wire(readCounter.O, imgRAM.raddr)
    wire(imgRAM.rdata, nextNodeInput)
    wire(circuit.input_ren, readCounter.CE)
    wire(circuit.input_wen, imgRAM.wen)
    wire(circuit.input_wen, writeCounter.CE)
    return imgRAM


def OutputImageRAM(cirb, circuit, prevNodeOutput, writeValidSignal, imgSrc, pxPerClock):
    imgData = loadImage(imgSrc, pxPerClock)
    imgRAM = CoreirMem(cirb, imgData.numRows, imgData.bitsPerRow)

    # this counter ensures writing to correct address always
    writeCounter = SizedCounterModM(imgData.numRows, has_ce=True)
    # this counter ensures reading from the right address
    readCounter = SizedCounterModM(imgData.numRows, has_ce=True)

    wire(writeCounter.O, imgRAM.waddr)
    wire(prevNodeOutput, imgRAM.wdata)
    wire(readCounter.O, imgRAM.raddr)
    wire(imgRAM.rdata, circuit.output_rdata)
    wire(circuit.output_ren, readCounter.CE)
    wire(writeValidSignal, imgRAM.wen)
    wire(writeValidSignal, writeCounter.CE)
    return imgRAM

def LoadImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock):
    imgData = loadImage(imgSrc, pxPerClock)
    sim.set_value(testcircuit.input_wen, [1], scope)
    sim.set_value(testcircuit.input_ren, [0], scope)
    sim.set_value(testcircuit.output_ren, [0], scope)
    for i in range(imgData.numRows):
        bitsStartIndex = i*imgData.bitsPerRow
        bitsEndIndex = (i+1)*imgData.bitsPerRow
        sim.set_value(testcircuit.input_wdata,
                      imgData.imgAsBits[bitsStartIndex:bitsEndIndex], scope)
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
    sim.set_value(testcircuit.input_wen, [0], scope)
    sim.set_value(testcircuit.input_ren, [1], scope)

def DumpImageRAMForSimulation(sim, testcircuit, scope, imgSrc, pxPerClock,
                              validityChecker, dstPath = None):
    """
    Get the data out of an Image RAM, run it through a function to check if it's
    valid, and write it to an output file if dstPath is set
    :param sim:
    :param testcircuit:
    :param scope:
    :param imgSrc:
    :param pxPerClock:
    :param validityChecker: A function of form (imgData, RAMRowIndex, resultData) -> Bool.
    The function should return true if valid, false if otherwise. The resultData
    is a bitarray, imgData is an IMGData object
    :param dstPath: Where to write the resulting image. If default value of None, no
    writing is done
    :return:
    """
    imgData = loadImage(imgSrc, pxPerClock)
    sim.set_value(testcircuit.input_ren, [0], scope)
    sim.set_value(testcircuit.output_ren, [1], scope)
    sim.evaluate()
    imgResult = bitarray(endian='little')
    for i in range(imgData.numRows):
        imgResult.extend(sim.get_value(testcircuit.output_rdata, scope))
        bitsStartIndex = i * imgData.bitsPerRow
        bitsEndIndex = (i + 1) * imgData.bitsPerRow
        assert validityChecker(imgData, i, imgResult[bitsStartIndex:bitsEndIndex])
        sim.evaluate()
        sim.advance_cycle()
        sim.evaluate()
    if dstPath is not None:
        image = Image.frombytes(mode="RGB", size=(10,10), data=imgResult.tobytes())
        image.save(dstPath)

def validIfEqual(imgData, rowIndex, resultData):
    bitsStartIndex = rowIndex * imgData.bitsPerRow
    bitsEndIndex = (rowIndex + 1) * imgData.bitsPerRow
    return imgData.imgAsBits[bitsStartIndex:bitsEndIndex] == resultData