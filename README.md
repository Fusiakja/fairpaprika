# fairpaprika

**fairpaprika** is an R package implementing a **procedurally fair PAPRIKA-based preference elicitation engine** for multi-criteria decision analysis (MCDA), with a particular focus on  **health and medical decision making** .

The package provides a **UI-agnostic core engine** for pairwise preference elicitation, transparent constraint-based inference of part-worth utilities, and built-in  **diagnostics for procedural fairness, stability, and feasibility** .

---

## Key features

### Core PAPRIKA Engine
* **PAPRIKA-style preference elicitation**
  * Pairwise comparisons between alternatives differing in exactly two criteria
  * Support for strict preferences (`A` / `B`) and indifference (`E`)
* **Procedural fairness by design**
  * Enforcement of two-attribute trade-offs
  * Optional requirement of opposite-direction trade-offs
  * Guaranteed coverage of all criterion pairs
  * Explicit handling of indifference via tolerance bands
* **Robust utility inference**
  * Linear programming formulation with monotonicity and anchoring constraints
  * Optional L1 regularization for stability
  * Graceful handling of infeasible or contradictory preference sets

### Healthcare SDM Extensions ⭐ NEW
* **Decision quality assessment** - 6-dimensional quality metrics
* **Patient journey tracking** - Cognitive load, fatigue, revision patterns
* **Report generation** - Patient-friendly and clinician-facing summaries
* **Health literacy tools** - Readability assessment, plain language translations
* **Visual decision aids** - Patient-accessible treatment comparisons

### Advanced Analytics ⭐ NEW
* **Computational optimizations** - Parallel sampling, coordinate Hit-and-Run
* **Diagnostic tools** - MCMC convergence, sensitivity analysis, model comparison
* **Enhanced procedural justice** - 4-pillar framework (voice/neutrality/respect/trust)
* **Comprehensive visualizations** - 10 plotting functions for SDM workflows

### Transparent diagnostics
  * Counts of strict vs. equal decisions
  * Share of valid two-difference comparisons
  * Criterion-pair coverage checks
  * Solver feasibility status and inferred importance weights
* **Well-tested, reproducible core**
  * 117 comprehensive tests (100% passing)
  * Deterministic behavior via explicit seeds
  * Clean separation of engine, solver, and diagnostics

---

## What this package is

**fairpaprika is:**

* A reusable **algorithmic core** for PAPRIKA-style elicitation
* Suitable for integration into Shiny apps, APIs, or simulation studies
* Designed for methodological research and transparent decision support


## Installation

```r
library(fairpaprika)
```

---

## Minimal example

```r
# Define criteria and ordered levels (worst -> best)
domains <- list(
  Effect            = c("Low", "Medium", "High"),
  SideEffects       = c("High", "Low"),
  Risks             = c("High", "Low"),
  Convenience       = c("Difficult", "Medium", "Easy"),
  Monitoring        = c("High", "Low")
)

# Create engine
eng <- engine_create(domains, seed = 1)

# Run elicitation loop (UI or simulation)
repeat {
  nxt <- engine_next_question(eng)
  eng <- nxt$engine
  q   <- nxt$question
  if (is.null(q)) break

  # Here: simple simulated choice (always choose A)
  eng <- engine_add_decision(eng, pref = "A")

  if (engine_done(eng)) break
}

# Compute part-worth utilities
eng <- engine_compute(eng)

# Inspect results
print(eng)
eng$weights                 # part-worth table
eng$diagnostics$importance  # relative criterion importance

# Undo the last answer (e.g., if a respondent corrects themselves)
eng <- engine_undo_decisions(eng, n = 1L)
engine_health(eng)  # shows undo count and whether compute is due

# Undo information is also kept in the audit/export:
aud <- engine_audit_report(eng)
aud$questions$after_undo       # per-question flag
aud$run$undo_count             # total undos
```

---

## Procedural fairness and diagnostics

The engine continuously tracks whether the elicitation process satisfies key procedural fairness conditions:

* Were **only valid two-attribute trade-offs** shown?
* Were **all criterion pairs covered** at least once?
* How many **indifference (`E`) responses** occurred?
* Is the inferred utility model  **feasible and stable** ?
* Were obvious **dominance implications** respected (if enabled)?

You can inspect these checks explicitly:

```r
engine_health(eng)
```

Example output:

```
Health checks
 Decisions: 24
  - Equal (E):  3
  - Strict (A): 21
 Two-diff comparisons: 100.0%
 Criteria-pair coverage: YES
Solve status: 0
```

### Implied rankings and dominance

By default, strict dominance between alternatives is added to the implied-ranking closure (no redundant questions on dominated options). You can disable this via `settings = list(closure = list(dominance = FALSE), ...)` when creating the engine. The number of dominance edges applied is recorded in `engine$diagnostics$closure_dominance_edges`, and any conflicts in `engine$diagnostics$closure_conflict`.

In code this looks like:

```r
# Dominance enabled (default)
eng <- engine_create(domains, settings = list(closure = list(dominance = TRUE)))

# Dominance disabled
eng <- engine_create(domains, settings = list(closure = list(dominance = FALSE)))
```

---

## Handling indifference and contradictions

* Indifference (`E`) is modeled via a **tolerance band** rather than exact equality, improving robustness.
* Contradictory or infeasible preference sets are  **detected explicitly** :
  * No silent failure
  * No forced or arbitrary solutions
  * Clear diagnostic status is returned

---

## Reproducibility

* All stochastic components are controlled via an explicit `seed`
* Identical inputs and decisions always yield identical results
* The core engine is deterministic given a fixed decision sequence

---

## Testing

The package includes a comprehensive `testthat` suite covering:

* Health and fairness diagnostics
* Solver feasibility and monotonicity
* Edge cases (many indifferences, contradictions)
* Scenario-based behavior (dominant criteria, random preferences)

Run all tests with:

```r
devtools::test()
```

---

## Intended use and scope

This package is intended for:

* Methodological research on preference elicitation and fairness
* Medical and health decision-making studies

---

## Governance and Model Card

For a concise overview of assumptions, configuration schema, fairness/stop rules, logging, and privacy guidance, see the model card at `inst/model_card.md`. Use it as the authoritative reference for configuring domains, interactions, fairness, stopping, and audit settings when deploying the engine.
* Simulation studies comparing elicitation strategies
* Integration into higher-level decision support tools

### Demo dataset

A small synthetic example is available at `inst/examples/demo_session.R` to demonstrate domains, profiles, decisions, and a full engine run.

---



## License

GPT3 License (see `LICENSE` file).

---

## Quick Start: Healthcare SDM

```r
library(fairpaprika)

# Define treatment criteria (disease-agnostic)
domains <- list(
  Effect = c("Low", "Medium", "High"),
  SideEffects = c("High", "Medium", "Low"),
  Convenience = c("Low", "Medium", "High")
)

# Create engine
eng <- engine_create(domains, seed = 42)

# Collect patient preferences
for (i in 1:15) {
  nxt <- engine_next_question(eng)
  eng <- nxt$engine
  if (is.null(nxt$question)) break
  
  # Patient responds: "A", "B", or "E"
  eng <- engine_add_decision(eng, pref = "A")
  if (engine_done(eng)) break
}

# Compute results
eng <- engine_compute(eng)

# Assess decision quality
quality <- sdm_decision_quality(eng)
print(quality$overall_quality)  # 0-1 score

# Generate patient report
print_patient_summary(eng)

# Visualize results
plot_decision_quality(quality)
plot_patient_journey(sdm_journey_report(eng))

# Monitor procedural justice
pj <- procedural_justice_full(eng)
plot_justice_dashboard(eng)
```

See `vignette("healthcare_sdm")` for complete workflow.

---

## Advanced Features

### Uncertainty Quantification
```r
# Bootstrap for confidence intervals
boot <- engine_bootstrap(eng, B = 200, parallel = TRUE)
plot_importance_ci(boot)

# Check convergence
conv <- bootstrap_convergence(boot)
print(conv$converged)
```

### Sensitivity Analysis
```r
# Test robustness to parameter choices
sens <- sensitivity_analysis(eng, param = "eps_strict", 
                             range = c(0.5, 2.0), steps = 10)
plot_diagnostics(sens, type = "sensitivity")
```

### Multi-Chain MCMC
```r
# Parallel sampling with convergence diagnostics
samples <- engine_polytope_sample(eng, n = 500, chains = 4, 
                                 method = "coordinate", parallel = TRUE)
diag <- polytope_diagnostics(samples)
print(diag$converged)
```

See `vignette("tolerance_tuning")` and `vignette("interactions")` for advanced topics.

---

## Documentation

**Vignettes:**
- `healthcare_sdm` - Complete SDM workflow with MS example
- `tolerance_tuning` - Advanced parameter guidance
- `interactions` - When/how to use non-additive models

**Key Functions:**
- SDM: `sdm_decision_quality()`, `sdm_journey_report()`, `sdm_decision_burden()`
- Reports: `print_patient_summary()`, `print_clinician_summary()`
- Justice: `procedural_justice_full()`, `justice_benchmark()`
- Diagnostics: `polytope_diagnostics()`, `sensitivity_analysis()`
- Visualizations: `plot_*()` functions (10 total)

---

