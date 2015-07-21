cachedLapply <- function(x, fun, memo.cache, plapply = lapply, ...,
                         use.func.contents = FALSE,
                         compare.args.as.characters = FALSE,
                         auto.save = FALSE) {
  
  if (class(memo.cache) != "memoCache") {
    stop("cacheMemoiser accepts a memoCache object.")
  }
  
  if (use.func.contents) {
    .func.name <- digest(deparse(fun))
  } else {
    .func.name <- as.character(substitute(fun))
  }
  
  # hash the x list
  .hashed.vals <- lapply(x, function(i) {
    if (compare.args.as.characters) {
      .hash <- tolower(digest(as.character(list(i, ...))))
    } else {
      .hash <- tolower(digest(list(i, ...)))
    }
    return (paste0(.func.name, "_", .hash))
  })
  
  # find the ones that do not exist in cache database
  .eval.index <- which(!duplicated(.hashed.vals) &
                       !sapply(.hashed.vals, memo.cache$has_key))
  

  # get the results that were not available and add to the cache
  results <- plapply(x[.eval.index], fun, ...)
  
  for (i in seq_along(.eval.index)) {
    memo.cache$set(.hashed.vals[[.eval.index[i]]], results[[i]])
  }
  
  if (auto.save) {
    memo.cache$save()
  }
  
  # extract all results from cache
  return (lapply(.hashed.vals, memo.cache$get))
}

