# Utilities for memoising to disk

# Author: Farzad Noorian <farzad.noorian@sydney.edu.au>
# Maintainer: Richard Davis <richard.davis@gmail.com>

# This code is partially based on memoise package by Hadley Wickham.
# http://cran.r-project.org/web/packages/memoise/

# This program is free software, distributed under the terms of
# the GNU General Public License version 2.
# Refer to <http://www.gnu.org/licenses/> for full terms.
################################################################################

#' Disk based Memo-Cache
#'
#' Creates a new disk based cache object.
#'
#' Originally from Hadley Wickham's memoise package.
#'   changes: added load and save to disk
#'
#' @param filename  name of the file to load the cache from, or store it into.
#'
#' @return cache memoiser object
#' @export
diskCache <- function(filename = NULL) {

  .cache <- NULL
  .memo.filename <- filename

  cache_reset <- function() {
    .cache <<- new.env(TRUE, emptyenv())
  }

  cache_set <- function(key, value) {
    assign(key, value, envir = .cache)
  }

  cache_get <- function(key) {
    get(key, envir = .cache, inherits = FALSE)
  }

  cache_has_key <- function(key) {
    exists(key, envir = .cache, inherits = FALSE)
  }

  cache_load <- function(filename) {
    if (missing(filename)) {
      filename <- .memo.filename
    }
    .cache <<- readRDS(filename)
  }

  cache_save <- function(filename) {
    if (missing(filename)) {
      filename <- .memo.filename
    }

    saveRDS(.cache, file = filename)
  }

  isfilepresent <- try({file.exists(.memo.filename)}, silent=TRUE)
  if (isfilepresent == TRUE) {
    cache_load(.memo.filename)
  } else {
    cache_reset()
  }

  ret_val <- list(
    reset = cache_reset,
    load = cache_load,
    save = cache_save,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    keys = function() ls(.cache)
  )

  class(ret_val) = "memoCache"
  return(ret_val)
}

#' Disk based memoisation
#'
#' Performs memoisation of arbitrary functions to disk to speed up repeated
#' calls to those functions.
#'
#' This function defines a memoisation wrapper which saves the results of the
#' function to disk. If the function is called with the same parameters it
#' retrieves the previous result and returns it. Note that this only makes sense
#' if the function is a deterministic and not dependent on external variables.
#' Use carefully!
#'
#' @aliases cacheMemoizer
#'
#' @param fun         An arbitrary function
#' @param memo.cache  A disk cache created by \code{\link{diskCache}} (or \code{\link{MemCache}})
#' @param use.func.contents   If TRUE, will hash \code{fun}'s contents and
#'                  use it for looking up for the memoized object.
#'                  Otherwise, function name will be used.
#'                  Useful when functions are anonymous or their contents may
#'                  change.
#' @param compare.args.as.characters If TRUE, converts args to a character
#'                  string before comparing them via memoized objects.
#'                  Useful when args may vary in their current form
#'                  (e.g., pointers)
#' @param auto.save If TRUE, will save results to disk after every evaluation.
#'                  Otherwise results are kept in memory, until they are saved using
#'                  cache object's \code{$save}.
#'
#' @return A memoised function
#'
#' @references Based on \url{http://cran.r-project.org/web/packages/memoise}.
#  Also see %\url{http://cran.r-project.org/web/packages/R.cache}
#'
#' @importFrom digest digest
#' @export
cacheMemoiser <- function(fun, memo.cache,
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

  return (
    function(...) {

      if (compare.args.as.characters) {
        .hash <- tolower(digest(as.character(list(...))))
      } else {
        .hash <- tolower(digest(list(...)))
      }

      .full.hash <- paste0(.func.name, "_", .hash)

      if (memo.cache$has_key(.full.hash)) {
        return(memo.cache$get(.full.hash))
      }

      result <- fun(...)
      memo.cache$set(.full.hash, result)

      if (auto.save) {
        memo.cache$save()
      }

      return (result)
    }
  )
}

cacheMemoizer <- cacheMemoiser
