# fairpaprika

`fairpaprika` is an R package for PAPRIKA-style preference elicitation in
patient decision aids and other multi-criteria decision analysis (MCDA)
workflows.

The package provides a UI-agnostic backend for constructing pairwise trade-off
questions, recording responses, applying transitive implications, and estimating
additive part-worth models by linear programming. It was developed as the
preference engine used in EPAMed-DA / EPAMed-MS, but the core functions are
disease-agnostic.

## What It Does

Core functionality:

- Build PAPRIKA-style pairwise trade-off question banks from ordered criteria.
- Record strict preferences and indifferent responses.
- Use transitivity to avoid redundant comparisons where possible.
- Estimate additive part-worth utilities with monotonicity, anchoring, and
  normalization constraints.
- Recompute results after answer revision.
- Provide basic diagnostics for feasibility, criterion-pair coverage, undo
  operations, and inferred criterion importance.

Additional modules support uncertainty analysis, benchmarking, healthcare
shared decision-making summaries, visualizations, and experimental algorithm
extensions. These modules are included for research use and should be treated as
less stable than the core PAPRIKA engine.

## Installation

From a local checkout:

```r
install.packages(c("devtools", "lpSolve", "jsonlite", "ggplot2"))
devtools::install(".")
```

After installation:

```r
library(fairpaprika)
```

For development without installation:

```r
devtools::load_all(".")
```

## Minimal Example

```r
library(fairpaprika)

domains <- list(
  Effect = c("Low", "Medium", "High"),
  SideEffects = c("High", "Low"),
  Risks = c("High", "Low"),
  Administration = c("Difficult", "Medium", "Easy"),
  Monitoring = c("High", "Low")
)

eng <- engine_create(
  domains = domains,
  seed = 1,
  settings = list(
    mode = "classic",
    tau_equal = 0.05,
    classic = list(use_regularization = FALSE)
  )
)

repeat {
  nxt <- engine_next_question(eng)
  eng <- nxt$engine

  if (is.null(nxt$question)) {
    break
  }

  # In an application, this response comes from the user interface.
  eng <- engine_add_decision(eng, pref = "A")

  if (engine_done(eng)) {
    break
  }
}

eng <- engine_compute(eng)

eng$weights
eng$diagnostics$importance
engine_health(eng)
```

## Answer Revision

The engine supports revision workflows. For example, an application can undo the
last answer and ask the next valid question again:

```r
eng <- engine_undo_decisions(eng, n = 1L)
nxt <- engine_next_question(eng)
```

If stored responses are edited directly, cached results can be cleared and the
model recomputed:

```r
eng <- engine_reset_caches(eng)
eng <- engine_compute(eng)
```

## Diagnostics

Useful diagnostics include:

```r
engine_health(eng)
eng$diagnostics$importance
eng$diagnostics$solve_status
eng$diagnostics$slack_flag
```

The package can also export audit information for development and evaluation:

```r
audit <- engine_audit_report(eng)
```

## Scope

`fairpaprika` is an algorithmic preference-elicitation backend. It does not
provide a complete patient decision aid by itself. Patient-facing information,
risk communication, clinical evidence, consent language, and regulatory framing
must be implemented and evaluated in the surrounding application.

In the EPAMed-MS prototype, `fairpaprika` is used for the pairwise preference
clarification component, while the Shiny application provides the user
interface, disease-specific content, evidence displays, and consultation
preparation workflow.

## Model Card

Assumptions, configuration options, fairness and stopping rules, logging
considerations, and deployment guidance are summarized in:

```text
inst/model_card.md
```

## License

GPL-3. See `LICENSE.md`.
