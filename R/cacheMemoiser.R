# Utilities for memoising to disk

# Author: Farzad Noorian <farzad.noorian@sydney.edu.au>
# Maintainer: Richard Davis <richard.davis@gmail.com>

# This code is partially based on memoise package by Hadley Wickham.
# http://cran.r-project.org/web/packages/memoise/

# This program is free software, distributed under the terms of
# the GNU General Public License version 2.
# Refer to <http://www.gnu.org/licenses/> for full terms.
################################################################################

diskCache <- function(filename = NULL) {
  # Originally from Hadley Wickham's memoise package
  #   changes: added load and save to disk
  # Inputs:
  #   filename: the name of file to load the cache from, or store it into
  # Returns:
  #   cache memoiser object
  
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
  
  isfilepresent <- try({file.exists(.memo.filename)})
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

cacheMemoiser <- function(fun, memo.cache) {
  
  if (class(memo.cache) != "memoCache") {
    stop("cacheMemoiser accepts a memoCache object.")
  }
  
  func.name <- as.character(substitute(fun))

  return (    
    function(...) {
      hash <- tolower(digest(list(...)) )	
      full.hash <- paste0(func.name, "_", hash)
      
      if (memo.cache$has_key(full.hash)) {
        return(memo.cache$get(full.hash))
      }
      
      result <- fun(...)
      memo.cache$set(full.hash, result)
      return (result)
    }
  )        
}

cacheMemoizer <- cacheMemoiser
