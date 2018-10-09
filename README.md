# MIX

A MIX emulator in Emacs Lisp

[MIX](https://en.wikipedia.org/wiki/MIX) is a hypothetical computer used by Donald Knuth for his monograph [TAOCP](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming).
It is a minimal register machine with two general purpose registers and 4000 words of memory.

This repository contains a MIX emulator for Emacs.
It shows the current state of the machine, and allows to watch code execution.

## Usage

Open the file in emacs, and hit `C-c C-b` (eval buffer). You should see a new buffer with output like this:

```
#
# MIX
#
rA:  +00.00.00.01.56
rX:  +00.00.00.00.00
rI1: +00.00.00.00.00
rI2: +00.00.00.00.00
rI3: +00.00.00.00.00
rI4: +00.00.00.00.00
rI5: +00.00.00.00.00
rI6: +00.00.00.00.00
rJ : +00.00.00.00.00
rIP: +00.00.00.00.38
cmp: G. overflow:0.
----------------------------------------------------
  M[ 0]: +00.00.00.00.03   M[20]: +00.00.00.01.18
  M[ 1]: +00.00.00.01.36   M[21]: +00.00.00.01.05
  M[ 2]: +00.00.00.01.56   M[22]: +00.00.00.01.19
  M[ 3]: +00.00.00.00.00   M[23]: +00.00.00.00.58
  M[ 4]: +00.00.00.00.00   M[24]: +00.00.00.00.32
  M[ 5]: +00.00.00.01.59   M[25]: +00.00.00.00.00
  M[ 6]: +00.00.00.00.00   M[26]: +00.00.00.00.00
  M[ 7]: +00.00.00.00.00   M[27]: +00.00.00.00.00
  M[ 8]: +00.00.00.00.00   M[28]: +00.00.00.00.00
  M[ 9]: +00.00.00.00.00   M[29]: +00.00.00.00.00
  M[10]: +00.00.00.00.00   M[30]: +00.00.00.00.08
  M[11]: +00.00.00.00.00   M[31]: +00.01.00.00.01
  M[12]: +00.00.00.00.00   M[32]: +00.03.00.00.03
  M[13]: +00.00.00.00.00   M[33]: +00.01.00.00.56
  M[14]: +00.00.00.00.00   M[34]: +00.31.00.04.39
  M[15]: +00.00.00.00.00   M[35]: +00.02.00.00.09
  M[16]: +00.00.00.00.00   M[36]: +00.20.00.00.60
  M[17]: +00.00.00.00.00   M[37]: +00.02.00.00.61
  M[18]: +00.00.00.00.00 > M[38]: +00.00.00.00.05
  M[19]: +00.00.00.00.00   M[39]: +00.00.00.00.00
```

The example MIX program is currently hard coded at the very end of the source file.
