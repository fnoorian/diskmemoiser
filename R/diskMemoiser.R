# Utilities for memoising to disk

# Author: Farzad Noorian <farzad.noorian@sydney.edu.au>
# Maintainer: Richard Davis <richard.davis@gmail.com>

# This code is partially based on memoise package by Hadley Wickham.
# http://cran.r-project.org/web/packages/memoise/

# This program is free software, distributed under the terms of
# the GNU General Public License version 2.
# Refer to <http://www.gnu.org/licenses/> for full terms.
################################################################################

library("digest")

#' Disk Memoisation
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
#' @aliases diskMemoizer
#' 
#' @param fun       An arbitrary function
#' @param memo.dir  An optional directory to store the memoised results. 
#'                  If not specified, defaults to "./memo"
#' @param use.func.contents   If TRUE, will hash \code{fun}'s contents and
#'                  use it for looking up for the memoized object.
#'                  Otherwise, function name will be used.
#'                  Useful when functions are anonymous or their contents may
#'                  change.
#' @param compare.args.as.characters If TRUE, converts args to a character
#'                  string before comparing them via memoized objects.
#'                  Useful when args may vary in their current form 
#'                  (e.g., pointers)
#'
#' @return A memoised function
#'
#' @references Based on \url{http://cran.r-project.org/web/packages/memoise}.
#  Also see %\url{http://cran.r-project.org/web/packages/R.cache}
#'
#' @importFrom digest digest
#' @export 
#'
#' @examples
#' myfunction.slow <- function(x, y) {
#'   Sys.sleep(1)
#'   return(x + sin(y))
#' }
#' 
#' myfunction <- diskMemoiser(myfunction.slow)
#' system.time({ result.slow <- myfunction(3, 5) })
#' system.time({ result.fast <- myfunction(3, 5) })
#' 
#' # The memoization directory defaults to ./memo, but can be reassigned:
#' myfunction <- diskMemoiser(myfunction.slow, memo.dir = "./temp")
#' result.slow <- myfunction(7, 11)
#' result.fast <- myfunction(7, 11)
diskMemoiser <- function(fun, memo.dir = "./memo", 
                         use.func.contents = FALSE,
                         compare.args.as.characters = FALSE ) {
  
  if (!file.exists(memo.dir)) {
    dir.create(memo.dir, showWarnings = FALSE, recursive = TRUE)
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
      
      file.name <- file.path(memo.dir, paste0(.func.name, "_", .hash, "-cache.RData"))
      
      if (file.exists(file.name)) {
        load(file.name)
        return(result)
      }
      
      result <- fun(...)
      save(result, file=file.name)
      return (result)
    }
  )        
}

#' @export 
diskMemoizer <- diskMemoiser
