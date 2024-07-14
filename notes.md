# Opener

- \Qcircuit @ { ... }

# Single Cells

## Simple

- \control
- \controlo

- \gate{#1}
- \targ
- \qswap
- \meter
- \meterB{#1}
- \measure{#1}
- \measureD{#1}
- \measuretab{#1}

- \ghost{#1}
- \pureghost{}
- \cghost{#1}
- \nghost{#1}

### Aligners

- \lstick{#1}
- \rstick{#1}
- \ustick{#1}
- \dstick{#1}

## Explicit wire

### Horizontal

- \qw[#1]
- \qwa[#1]
- \cw[#1]
- \cwa[#1]

### Vertical

- \qwx[#1]
- \cwx[#1]

- \ctrl{#1}
- \ctrlo{#1}
- \cctrl{#1}
- \cctrlo{#1}

# Multi Cells

- \multigate{#1}{#2}
- \sgate{#1}{#2}
- \smeterB{#1}{#2}
- \multimeasure{#1}{#2}
- \multimeasureD{#1}{#2}

- \barrier[#1]{#2}

# Global Label Modifiers

- \gategroup{#1}{#2}{#3}{#4}{#5}{#6}
- \inputgroup{#1}{#2}{#3}{#4}
- \inputgroupv{#1}{#2}{#3}{#4}{#5}
- \inputgrouph{#1}{#2}{#3}{#4}{#5}

# Weird

- \push{#1}
- \cds{#1}{#2}

- \link{#1}{#2}
  -  Draws a wire or connecting line to the element #1 rows down and #2 columns forward.

# Other commands

- \metersymb
- \mbox
  - As in: \measure{\mbox{#1}}

- \bra{#1}
- \ket{#1}
- \ip{#1}{#2} % Inner product
- \op{#1}{#2} % Outer product
- \melem{#1}{#2}{#3} % Matrix element
- \expval{#1} % Expectation value
