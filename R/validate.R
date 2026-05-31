validate_domains <- function(domains) {
  if (!is.list(domains) || length(domains) < 2) {
    stop("domains must be a named list with >= 2 criteria.")
  }
  if (is.null(names(domains)) || any(names(domains) == "")) {
    stop("domains must be a named list (criterion names required).")
  }
  for (cn in names(domains)) {
    lv <- domains[[cn]]
    if (!is.character(lv) || length(lv) < 2) {
      stop(sprintf("domains[['%s']] must be a character vector with >=2 levels.", cn))
    }
    if (anyDuplicated(lv)) {
      stop(sprintf("domains[['%s']] has duplicate levels.", cn))
    }
  }
  domains
}

validate_engine <- function(engine) {
  if (!inherits(engine, "paprika_engine")) stop("Not a paprika_engine.")
  if (is.null(engine$domains) || is.null(engine$criteria)) stop("Engine missing domains/criteria.")
  engine
}
