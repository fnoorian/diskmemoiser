#' In Memory Cache Memoisation
#'
#' Caches in memory object
#'
MemCache <- function() {
  
  .cache <- NULL
  
  cache_reset <- function() {
    .cache <<- new.env(TRUE, emptyenv())
  }
  
  cache_set <- function(key, value) {
    assign(key, value, envir = .cache)
  }
  
  cache_get <- function(key) {
    get(key, envir = .cache, inherits = FALSE)
  }

  cache_get_multi <- function(key_list) {
    lapply(key_list, function(key) get(key, envir = .cache, inherits = FALSE))
  }
  
  cache_has_key <- function(key) {
    exists(key, envir = .cache, inherits = FALSE)
    
  }
  
  ### create new object
  cache_reset()
  
  ret_val <- list(
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    get_multi = cache_get_multi,
    has_key = cache_has_key,
    keys = function() ls(.cache)
  )
  
  class(ret_val) = "memoCache"
  return(ret_val)
}
