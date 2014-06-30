# Utilities for memoising to disk

# Author: Farzad Noorian <farzad.noorian@sydney.edu.au>
# Maintainer: Richard Davis <richard.davis@sydney.edu.au>

# This code is partially based on memoise package by Hadley Wickham.
# http://cran.r-project.org/web/packages/memoise/ 

# This program is free software, distributed under the terms of
# the GNU General Public License version 2.
# Refer to <http://www.gnu.org/licenses/> for full terms.
################################################################################

library("digest")

diskMemoiser <- function(fun, memo.dir = "./memo") {

  if (!file.exists(memo.dir)) {
    dir.create(memo.dir, showWarnings = FALSE)
  }

  func.name = as.character(substitute(fun))
  return (    
    function(...) {
     hash <- tolower(digest(list(...)) )	
      file.name = file.path(memo.dir, paste0(func.name, "_", hash, "-cache.RData"))

      if (file.exists(file.name)) {
        load(file.name)
        return(result)
      }
  
      result = fun(...)
      save(result, file=file.name)
      return (result)
    }
  )        
}

diskMemoizer <- diskMemoiser
