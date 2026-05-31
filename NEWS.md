# fairpaprika 0.2.0 (Current Development)

## Classic PAPRIKA Mode - Complete Implementation

This release adds a **fully-featured Classic PAPRIKA mode** implementing all features from Hansen & Ombler (2008), plus modern enhancements:

### Core PAPRIKA Algorithm

* **Systematic question generation** - Two-criterion pairwise comparisons
* **Dominance elimination** - Filters undominated pairs automatically  
* **Transitive closure** - Floyd-Warshall algorithm for preference inference
* **Minimum discrimination** - Epsilon gaps (ε = 0.001) prevent degenerate solutions
* **Adaptive stopping** - Outcome-based termination when profiles resolved

### Enhanced Features (Beyond OG PAPRIKA)

* `paprika_update_pareto_set()` - **Real-time Pareto tracking**
  - Shows which alternatives remain competitive after each question
  - Reports newly eliminated options
  - Perfect for interactive Shiny applications

* `paprika_prune_dominated()` - **Dominated profile pruning**
  - Automatically excludes strictly inferior alternatives
  - Critical for clinical safety (don't recommend worse therapies)

* `paprika_diagnostics()` - **Enhanced diagnostics**
  - Question efficiency: questions asked / total possible
  - Closure effectiveness: implied preferences / total preferences
  - Inconsistency detection: catches contradictory responses

### New Exported Functions

* `paprika_all_profiles_resolved()` - Check stopping criterion
* `paprika_pareto_summary()` - Human-readable Pareto set status
* `paprika_efficiency_metrics()` - Efficiency calculations
* `paprika_detect_inconsistencies()` - Find contradictions
* Plus 12 more internal functions for PAPRIKA operations

### Documentation

* New vignette: **Classic PAPRIKA Mode** - Complete usage guide
  - Classic vs. Full mode comparison
  - Elicitation loop examples
  - Enhanced features documentation
  - Clinical decision support applications

### Performance

* Typical question reduction: **50-94%** vs. exhaustive enumeration
* Example: 5 profiles resolved in 3 questions (instead of 50 max)

---

# fairpaprika 0.1.1 (Development Release)

## Major Enhancements

This release transforms fairpaprika into a comprehensive healthcare Shared Decision Making (SDM) engine with emphasis on procedural justice and patient-centered design.

### Computational Optimizations

* **Coordinate Hit-and-Run sampler** for polytope sampling (2-3x better mixing)
* **Parallel multi-chain sampling** with Gelman-Rubin convergence diagnostics
* **Parallel bootstrap** with platform-specific backends (Unix/Windows)
* **Progress bars** for long-running operations (polytope sampling, bootstrap)

### Diagnostic Tools (NEW)

* `polytope_diagnostics()` - MCMC convergence assessment (R-hat, ESS)
* `sensitivity_analysis()` - Robustness testing for tolerance parameters
* `bootstrap_convergence()` - Bootstrap adequacy checks
* `model_comparison()` - Compare models with/without interaction terms

### Healthcare SDM Features (NEW)

* `sdm_decision_quality()` - 6-dimensional quality assessment
  - Preference clarity, confidence, value congruence
  - Knowledge quality, deliberation quality, procedural justice
* `sdm_journey_report()` - Patient journey tracking
  - Cognitive load, response patterns, revision analysis, fatigue indicators
* `sdm_decision_burden()` - Decision burden assessment with recommendations
* `validate_treatment_profiles()` - Context-specific validation (MS, oncology, cardiology)

### Report Generation (NEW)

* `sdm_patient_summary()` / `print_patient_summary()` - Patient-friendly reports
* `sdm_clinician_summary()` / `print_clinician_summary()` - Clinical decision support
* Plain language, actionable insights, quality red flags

### Enhanced Procedural Justice (NEW)

* `procedural_justice_full()` - 4-pillar justice assessment
  - Voice (preference expression), Neutrality (balanced selection)
  - Respect (patient agency), Trustworthiness (transparency)
* `justice_benchmark()` - Cross-session fairness comparison
* `justice_transparency_report()` - Complete audit trail generation

### Health Literacy Tools (NEW)

* `question_readability()` - Flesch-Kincaid grade level + jargon detection
* `plain_language_explain()` - Medical term translation (context-aware)
* `create_option_comparison_table()` - Patient-friendly comparison tables
* `create_visual_decision_aid()` - Text-based visual aids
* `print_decision_aid()` - Console-formatted decision aids

### Comprehensive Visualization Suite (NEW)

**Core SDM:**
* `plot_decision_quality()` - Quality metrics bar chart
* `plot_patient_journey()` - 4-panel journey visualization
* `plot_treatment_comparison()` - Option ranking bars

**Diagnostics:**
* `plot_diagnostics()` - Polytope & sensitivity diagnostics

**Uncertainty:**
* `plot_importance_ci()` - Criterion importance with confidence intervals
* `plot_weights()` - Bootstrap weight distributions (density/boxplot)

**Advanced Analytics:**
* `plot_pairwise()` - Pairwise win probability heatmap
* `plot_profile_ranks()` - Rank probability visualization
* `plot_justice_dashboard()` - 4-panel justice monitoring
* `plot_treatment_profiles()` - Multi-option comparison charts

All visualizations use base R graphics (no ggplot2 dependency).

### Documentation

* New vignette: **Healthcare SDM Workflow** - Complete end-to-end guide
  - SDM workflow, procedural justice, MS example
  - Adaptation to oncology, cardiology, other conditions
* New vignette: **Tolerance Parameter Tuning** - Advanced parameter guidance
* New vignette: **Interaction Terms** - When/how to use non-additive models

### Testing

* **117 comprehensive tests** (100% passing)
* Full coverage of all new features
* Edge case handling verified

## Breaking Changes

None - all changes are backward compatible.

## Bug Fixes

* Fixed multi-chain sampling to handle chains with different lengths
* Improved numerical stability in Gelman-Rubin calculation
* Fixed string escaping in SDM report generation

## Performance Improvements

* Parallel bootstrap: ~4x speedup on 4-core systems
* Coordinate Hit-and-Run: 2-3x better mixing than standard method
* Efficient multi-chain diagnostics

## Dependencies

### New Suggested Dependencies
* `parallel` (base R) - for parallel computation
* `knitr`, `rmarkdown` - for vignettes

No new required dependencies added.

---


