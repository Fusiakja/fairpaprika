# FairPaprika Model Card

## Purpose and Scope
- Decision aid for multi-criteria tradeoffs using PAPRIKA-style questions with optional sparse interactions and fairness constraints.
- Intended for research/prototyping; not a clinical decision maker. Always pair with expert judgement.

## Core Assumptions
- Additive baseline utilities with monotone level increments; optional sparse interaction terms for selected criterion pairs.
- Anchors: worst levels fixed at 0; normalization on top levels.
- Response model: tau_equal governs A/B/E bins; eps_strict is the constraint margin.
- Fairness: pair coverage + exposure balance; optional interaction coverage; balanced randomization within top-K picks.
- Eligibility/dealbreakers are hard filters.

## Configuration Schema (key fields)
- `domains`: named list criterion -> ordered levels (worst..best).
- `interactions`: `enabled`, `pairs`, `max_pairs`, `max_fraction`, `max_abs`, `relevance_tol`, `activate` (winner_entropy, slack, min_questions).
- `selector`: sampling params (`n_samples`, `burnin`, `thin`), scoring weights (`alpha,beta,gamma,delta,kappa,zeta,eta`), `top_k`, `candidate_pool`, `pair_cap`, `eig_target`, `utility_top_k`.
- `fair`: `enabled`, `pair_coverage`, `exposure_balance`, `exposure_gap_limit`, `interaction_coverage`.
- `stop`: `win_prob`, `win_conf_streak`, `eig_threshold`, `no_progress_ig`, `no_progress_streak`, `weight_span_eps`, `topk_prob`.
- `slack`: `enabled`, `penalty`, `warn_sum`, `warn_max`.
- `eligibility`: allowed levels / max levels / excluded profiles.

## Procedural Justice & Logging
- Per-question audit: type, crit_pair, IG, fairness, cost, implied, interaction flags, seed, balance relaxation.
- Run-level audit: coverage curves, exposure gaps/Gini, interaction coverage, dominance rate, order sensitivity, slack stats.
- Procedural-justice report: coverage/exposure, family mix, burden metrics; exportable JSON/TXT.

## Explanations
- Top-3 explanations include: main drivers, against-arguments, robustness label, counterfactual criteria, interaction relevance, eligibility reasons.
- Interaction summary marks whether additive suffices (`relevance_tol`).

## Sanity Checks
- Dominance sanity: dominated options flagged if higher utility.
- Interaction magnitude sanity: warns if `max_abs` exceeded.
- Eligibility applied as hard constraints; monotonicity enforced.
- Implied rankings: dominance edges are added to the closure by default (`settings$closure$dominance = TRUE`); disable via `settings$closure = list(dominance = FALSE)` if you need additive-only closure. Count stored in `diagnostics$closure_dominance_edges`, conflicts in `diagnostics$closure_conflict`.

## Benchmarks
- `benchmark_ablation()`: simulate synthetic patients to compare configs (Top-1/Top-3 accuracy, questions, coverage) with optional noise.
- Seed/order stability tools (`seed_stability_report`, `permutation_stress_test`) quantify path dependence; use audit logs for real sessions.

## Privacy/Logging Guidance
- Store only necessary audit data; separate session logs (anonymized) from any clinical notes.
- Seeds and randomization recorded for reproducibility; avoid storing PII.
 - For deployment, bundle or reference this model card; avoid storing PII; rely on audit logs for real-session stability checks.

## Known Limitations
- Interaction questions only conditional tradeoffs; joint-improvement family not yet included.
- Bayesian Stan model assumes sparse interactions; tune shrinkage (`slab_scale`, `slab_df`) and burden multipliers (`interactions$burden`) for large criteria sets/strong priors.
- Simulation benchmarks are lightweight; not a full clinical validation—ground truth/stakeholder evaluation required.
