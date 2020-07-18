Polymorphic Design
====

## Summary

To provide the common structure for CEREScript series, we adopt a polymorphic design to CEREScript.

## Background

### CEREScript series

Many packages of Holos Works / PPUZZL Group and Graphers shares the same script structure: CEREScript.
However, because they aim different purposes, they do not use the CEREScript directly but uses variants of CEREScript like CEREScript-Core, CEREScript, SGript and etc.
Each variant implementation has more/less features among each other.
In such situations, ArchCEREScript is designed to provide the common structure of CEREScript series.

Then, how do we design CEREScript series for its flexibility?

### CEREScript as polymorphic

We redesign CEREScript as Polymorphic.

CEREScript-Core, CEREScript and SGript would apply its prefer type to CEREScript's type parameter.

## Benefits

### Optimization

At old CEREScript-Core00, we implement extended instruction set of CEREScript-Core as `CRSExtendedN CHeader VP VP ...`,
But this means that every extended instruction set should be represented by type `CHeader` which would be `Text` or `ByteString`.
This is not good choice at performance aspect, because even it is already parsed, the interpreter still need to string-comparing to identify the instruction without redefine the archetype CEREScript itself.

## ArchCEREScript: Archetype of CEREScript

## Design

### Plugin-able Instruction Set

````haskell
data CEREScript variablePosition value additionalInstructionSet
  = CRSAbc ...
  | CRSBcd ...
  | CRSCde ...
  | CRSExtended CHeader ...
  | CRSAdditional (additionalInstructionSet value variablePosition)
````

* `value` would accept custom `Value` type of the new interpreter
* `variablePosition` would accept custom variable storage structure of the new interpreter
* `additionalInstructionSet` would accept additional instruction set of the new interpreter

Of course, `additionalInstructionSet` also may need the new type of `value` and `variablePosition`, but if those parameters are given by the new interpreter, the programmer could defined `additionalInstructionSet` with `value` and `variablePosition` when he/she write `additionalInstructionSet`.

### Design Example

````haskell
data CEREScript vp v ais
  = CRSAbc
  | CRSBcd (vp v)
  | CRSCde (vp v) (vp v)
  | CRSExtended1 CHeader (vp v)
  | CRSExtended2 CHeader (vp v) (vp v)
  | CRSAdditional (ais vp v)

data AIS vp v
  = AISAbc
  | AISBcd (vp v)
  | AISCde (vp v) (vp v)
  | AISExtended1 CHeader (vp v)
  | AISExtended2 CHeader (vp v) (vp v)

data VP1 v = VPX Int | VPY Text | VPHere v
data VT1 = VTInt | VTStr | VTCustom
data V1 vts vt1 vt2 = VInt Int | VStr String | V1 vt1 | V2 vt2

data CustomType1 = Enum1 | Enum2
data CustomType2 = CT2 { ct2i :: Int, ct2s :: String }

type CHeader = Text

aCRS1, aCRS2 :: CEREScript VP1 (V1 VTS1 CustomType1 CustomType2) AIS
aCRS1 = CRSAdditional (AISBcd (VPHere (V1 Enum1)))
aCRS2 = CRSBcd (VPX 1)
````

## Issues

### Should `v` be defined with more then one independent type?

When the interpreter wants to handle more than one customized type, what should we do?
Should ArchCEREScript provide more then one explicit data type parameter?
Or the interpreter should provide those types as a single sum type?

#### Pros

* Could handle custom data types in a single container type

#### Cons

* Forces every required class to every custom data types
