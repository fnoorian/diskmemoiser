#' Memo-cached lApply
#'
#' Applies the given data to function, storing the results in the memory object cache.
#'
#' @param x           A vector to apply to the function
#' @param fun         An arbitrary function
#' @param memo.cache  A disk cache created by \code{\link{diskCache}} (or \code{\link{MemCache}})
#' @param plapply     Apply function to use. By default \code{\link[base]{lapply}}.
#'                    To be used with parallel processing type apply functions, .e.g, \code{\link[parallel]{mclapply}}.
#' @param use.func.contents   If TRUE, will hash \code{fun}'s contents and
#'                    use it for looking up for the memoized object.
#'                    Otherwise, function name will be used.
#'                    Useful when functions are anonymous or their contents may
#'                    change.
#' @param compare.args.as.characters If TRUE, converts args to a character
#'                    string before comparing them via memoized objects.
#'                    Useful when args may vary in their current form
#'                    (e.g., pointers)
#' @param auto.save   If TRUE, will save results to disk after every evaluation.
#'                    Otherwise results are kept in memory, until they are saved using
#'                    cache object's \code{$save}.
#' @param ...         Other inputs to \code{plapply}. These will be considered in memoization.
#'
#' @export
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

