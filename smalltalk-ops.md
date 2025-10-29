# Smalltalk-80 Operations

Research on the bytecode operations supported by the original Smalltalk-80 virtual machine.

## Sources
- **Blue Book**: Smalltalk-80: The Language and Its Implementation by Adele Goldberg and David Robson
- **Chapter 28**: Formal Specification of the Virtual Machine
- **Online references**: http://www.mirandabanda.org/bluebook/bluebook_chapter28.html

## Overview

Smalltalk-80 uses a bytecode-based virtual machine with 256 possible bytecodes (0-255). The bytecodes are grouped by function into several categories.

## Four Basic Bytecode Types

1. **Stack bytecodes** - Move object pointers between object memory and evaluation stack
2. **Jump bytecodes** - Change the instruction pointer of the active context
3. **Send bytecodes** - Invoke CompiledMethods or primitive routines
4. **Return bytecodes** - Terminate execution of CompiledMethods

## Bytecode Categories and Ranges

### Push Operations (0-119)

**Instance Variables (0-15)**
- Single byte instructions to push first 16 instance variables of receiver

**Temporary Variables (16-31)**
- Single byte instructions to push first 16 temporary frame locations

**Literal Constants (32-63)**
- Single byte instructions to push first 32 literal constants

**Literal Variables (64-95)**
- Single byte instructions to push first 32 literal variables

**Special Push (112-119)**
- 112-115: Push receiver variable
- 116: Push temporary location
- 117: Push literal constant
- 118: Push literal variable
- 119: Push context (thisContext)

### Special Constant Push (120-127)

- 120: Push receiver (self)
- 121: Push true
- 122: Push false
- 123: Push nil
- 124-127: Reserved

### Extended Instructions (128-143)

Complex instructions requiring additional bytes:
- 128: Extended push - can access up to 64 instance vars, temps, literals
- 129: Extended store
- 130: Extended store and pop
- 131: Single extended send
- 132: Double extended do-anything
- 133: Single extended super
- 134: Second extended send
- 135: Pop stack top
- 136-143: Reserved/Extended operations

### Short Jump Operations (144-159)

- 144-151: Jump forward (short)
- 152-159: Jump forward if false (short conditional)

### Long Jump Operations (160-175)

- 160-167: Jump forward (long)
- 168-171: Jump forward if true (long conditional)
- 172-175: Jump forward if false (long conditional)

### Arithmetic Message Sends (176-191)

Special bytecodes for common arithmetic operations:
- 176: +
- 177: -
- 178: <
- 179: >
- 180: <=
- 181: >=
- 182: =
- 183: ~=
- 184: *
- 185: /
- 186: \\
- 187: @
- 188: bitShift:
- 189: //
- 190: bitAnd:
- 191: bitOr:

### Special Message Sends (192-207)

Optimized sends for frequently used messages:
- 192: at:
- 193: at:put:
- 194: size
- 195: next
- 196: nextPut:
- 197: atEnd
- 198: ==
- 199: class
- 200: blockCopy:
- 201: value
- 202: value:
- 203: do:
- 204: new
- 205: new:
- 206: x
- 207: y

### Message Send Operations (208-255)

**Send Literal Selector (208-223)**
- 0 arguments, selectors 0-15

**Send Literal Selector (224-239)**
- 1 argument, selectors 0-15

**Send Literal Selector (240-255)**
- 2 arguments, selectors 0-15

## F# Model

```fsharp
module SmalltalkOps

type BytecodeCategory =
  | Stack
  | Jump
  | Send
  | Return

type Bytecode =
  // Stack - Push operations (0-119)
  | PushReceiverVariable of index: byte      // 0-15
  | PushTemporaryVariable of index: byte     // 16-31
  | PushLiteralConstant of index: byte       // 32-63
  | PushLiteralVariable of index: byte       // 64-95
  | PushReceiver                             // 120 (self)
  | PushTrue                                 // 121
  | PushFalse                                // 122
  | PushNil                                  // 123
  | PushContext                              // 119 (thisContext)

  // Stack - Extended operations (128-143)
  | ExtendedPush of byte * byte             // 128 + descriptor byte
  | ExtendedStore of byte * byte            // 129 + descriptor byte
  | ExtendedStoreAndPop of byte * byte      // 130 + descriptor byte
  | PopStackTop                             // 135

  // Jump operations (144-175)
  | JumpForwardShort of offset: byte        // 144-151
  | JumpForwardIfFalse of offset: byte      // 152-159, 172-175
  | JumpForwardIfTrue of offset: byte       // 168-171
  | JumpForwardLong of offset: uint16       // 160-167

  // Arithmetic sends (176-191)
  | SendPlus                                // 176: +
  | SendMinus                               // 177: -
  | SendLessThan                            // 178: <
  | SendGreaterThan                         // 179: >
  | SendLessOrEqual                         // 180: <=
  | SendGreaterOrEqual                      // 181: >=
  | SendEqual                               // 182: =
  | SendNotEqual                            // 183: ~=
  | SendMultiply                            // 184: *
  | SendDivide                              // 185: /
  | SendMod                                 // 186: \\
  | SendPoint                               // 187: @
  | SendBitShift                            // 188: bitShift:
  | SendIntegerDivide                       // 189: //
  | SendBitAnd                              // 190: bitAnd:
  | SendBitOr                               // 191: bitOr:

  // Special sends (192-207)
  | SendAt                                  // 192: at:
  | SendAtPut                               // 193: at:put:
  | SendSize                                // 194
  | SendNext                                // 195
  | SendNextPut                             // 196: nextPut:
  | SendAtEnd                               // 197
  | SendIdentical                           // 198: ==
  | SendClass                               // 199
  | SendBlockCopy                           // 200: blockCopy:
  | SendValue                               // 201
  | SendValueWith                           // 202: value:
  | SendDo                                  // 203: do:
  | SendNew                                 // 204
  | SendNewWith                             // 205: new:
  | SendX                                   // 206
  | SendY                                   // 207

  // Message sends (208-255)
  | SendLiteralSelector of args: byte * selector: byte  // 208-255
  | SingleExtendedSend of byte * byte       // 131
  | DoubleExtendedDoAnything of byte * byte // 132
  | SingleExtendedSuper of byte * byte      // 133
  | SecondExtendedSend of byte * byte       // 134

  // Return operations
  | ReturnReceiver                          // Part of return bytecodes
  | ReturnTrue                              // Part of return bytecodes
  | ReturnFalse                             // Part of return bytecodes
  | ReturnNil                               // Part of return bytecodes
  | ReturnTopFromMethod                     // Part of return bytecodes
  | ReturnTopFromBlock                      // Part of return bytecodes

let opcodeToCategory (bytecode: Bytecode) : BytecodeCategory =
  match bytecode with
  | PushReceiverVariable _
  | PushTemporaryVariable _
  | PushLiteralConstant _
  | PushLiteralVariable _
  | PushReceiver | PushTrue | PushFalse | PushNil | PushContext
  | ExtendedPush _ | ExtendedStore _ | ExtendedStoreAndPop _
  | PopStackTop -> Stack

  | JumpForwardShort _ | JumpForwardIfFalse _
  | JumpForwardIfTrue _ | JumpForwardLong _ -> Jump

  | SendPlus | SendMinus | SendLessThan | SendGreaterThan
  | SendLessOrEqual | SendGreaterOrEqual | SendEqual | SendNotEqual
  | SendMultiply | SendDivide | SendMod | SendPoint
  | SendBitShift | SendIntegerDivide | SendBitAnd | SendBitOr
  | SendAt | SendAtPut | SendSize | SendNext | SendNextPut
  | SendAtEnd | SendIdentical | SendClass | SendBlockCopy
  | SendValue | SendValueWith | SendDo | SendNew | SendNewWith
  | SendX | SendY | SendLiteralSelector _
  | SingleExtendedSend _ | DoubleExtendedDoAnything _
  | SingleExtendedSuper _ | SecondExtendedSend _ -> Send

  | ReturnReceiver | ReturnTrue | ReturnFalse | ReturnNil
  | ReturnTopFromMethod | ReturnTopFromBlock -> Return
```

## Key Design Insights

1. **Compactness**: Common operations (first 16 instance vars, temps) get single-byte encodings
2. **Special message optimization**: Arithmetic and collection operations have dedicated bytecodes
3. **Extensibility**: Extended bytecode format allows access to larger pools of literals/variables
4. **Literal frame**: The first 32 literals can be accessed with single-byte instructions
5. **Conditional execution**: Multiple jump variants for short/long jumps and true/false conditions
6. **Message sends**: Three-tier approach - special (1 byte), literal with 0-2 args (1 byte), extended (2+ bytes)

## References

- Goldberg, A., & Robson, D. (1983). *Smalltalk-80: The Language and Its Implementation*. Addison-Wesley.
- Blue Book Chapter 28: http://www.mirandabanda.org/bluebook/bluebook_chapter28.html
- Mariano Martinez Peck's Introduction to Smalltalk Bytecodes: https://marianopeck.blog/2011/05/21/introduction-to-smalltalk-bytecodes/
