# Test helper functions

# Helper to add decisions properly
add_test_decisions <- function(eng, n = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  for (i in seq_len(n)) {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break
    
    # Randomly choose A or E
    pref <- sample(c("A", "A", "A", "E"), 1, prob = c(0.7, 0.1, 0.1, 0.1))
    eng <- engine_add_decision(eng, pref = pref)
    
    if (engine_done(eng)) break
  }
  
  eng
}
