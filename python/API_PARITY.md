# R and Python API parity

This document records deliberate differences between the R and Python packages.
Matching names imply matching user-facing semantics unless noted below.

| Concept | R | Python | Status |
|---|---|---|---|
| Predictions, comparisons, slopes | Yes | Yes | Aligned core estimands |
| Average estimands | `avg_*` | `avg_*` | Aligned |
| Prediction scale | `type` | Adapter default | Planned adapter capability |
| Degrees of freedom | `df` | Adapter-provided | Deliberate Python default |
| Numerical derivative method | `numderiv` | Forward difference plus automatic/analytic paths | Different interface |
| Analytic Jacobian | Model methods and model matrix | Linear model-matrix plans | Partial; other plans fall back |
| Automatic differentiation | Extension mechanism | Optional JAX extra | Python-specific |
| Simulation inference/replay | Yes | No | Planned |
| Bootstrap/conformal inference | Yes | No | Planned |
| Bayesian posterior draws | Yes | No | Planned |
| Multiple imputation | Yes | No | Planned |

Python-specific API differences should be added here before release so structural
drift is not mistaken for an implementation bug.
