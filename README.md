# Idris2-Cellular

[![Idris 2 Verification](https://img.shields.io/badge/Idris_2-0.8.0-blue.svg)](https://www.idris-lang.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

**Layer 5 Comonadic Cellular Automata Physics & Local Neighborhood Grids for Idris 2**

`Idris2-Cellular` forms **Layer 5** of the 10-layer constructive non-linear multiset science framework. It formalizes spatial neighborhood comonads (`Comonad`: `extract`, `duplicate`, `extend`), comonadic cellular grid structures (`CellularGrid a`), localized Weyl grid updates, boundary dissipation rules, and collision detection over discrete cellular networks without global state mutation.

---

## 📦 Core Library Architecture & Modules

### 1. `Math.Cellular.Comonad`
- **Comonad Interface:** The dual of a Monad, formalizing local inspection primitives (`extract`, `duplicate`, `extend`).
- **Comonadic Focused Grid:** `CellularGrid a` holding a focused cell state alongside its surrounding local neighbors, ensuring local rules inspect adjacent context without mutating global memory.

### 2. `Math.Cellular.Automata`
- **Comonadic Cellular State Transitions:** Local cellular automata transition functions operating concurrently across focused grid contexts.
- **Localized Weyl Grids & Fluid Dynamics:** Discrete gas/fluid particle collisions, boundary dissipation, and local energy conservation over cellular grid networks.

---

## 🚀 Building & Installing

```bash
idris2 --build Idris2-Cellular.ipkg
idris2 --install Idris2-Cellular.ipkg
```

---

## 🔬 Architectural Principles

- **Total Constructivism:** Enforces `%default total` across all cellular automata modules.
- **Comonadic Context Inspection:** Local data dependency evaluated strictly via `extract` and `duplicate` without global state leakage.
- **Zero Floating-Point Drift:** Discrete cellular automata steps computed over exact integer and multiset state counts.
