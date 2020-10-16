ArchCEREScript (αρχηCEREScript)
====

The common structure of CEREScript.

## Object

Provide the common and flexible structure for CEREScript variants.

## Features

Only provides framework, but not work by itself.
Also provides common components for making CEREscript variants

## How to use ArchCEREScript

### How to use ArchCEREScript Parser

ArchCEREScript Parser also not work independently.
When you define AnCEREScript upon ArchCEREScript, you may need to define `parseAnCEREScript` as like as `parseArchCEREScript parseVariablePlace parseVariableIndex parseValueContainer ...`

## Variants of CEREScript

* CEREScript-SuperSet
  * Practical and Experimental implementation of CEREScript for extreme and full-featured design
* CEREScript-Core
  * For CoMPleT Engine / Holos Works / PPUZZL Group
  * + Turing-Complete instruction set
  * + Interactive instruction
* CEREScript-MB
  * For MaterialBalancer
  * Simplified instruction set
  * - Interactive instruction
  * Focused on dependency and branch
* SGript
  * For HistoryGrapher / StorioGrapher
  * + Branching instructions
* CERES / Demeter
  * For CERE / Project Chloe
  * + Integrate TEOE Model to instruction set
