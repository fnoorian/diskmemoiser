# This program does some regression testing for package "diskMemoiser"

# Maintainer: Richard Davis <richard.davis@sydney.edu.au>

# This program is free software, distributed under the terms of
# the GNU General Public License version 2.
# Refer to <http://www.gnu.org/licenses/> for full terms.
################################################################################

library("diskmemoiser")

myfunction.slow<-function(x,y) {
  return(x+sin(y))
}

x=3
y=5
myfunction=diskMemoiser(myfunction.slow)
result.slow=myfunction(x, y)
result.fast=myfunction(x, y)
stopifnot(result.slow == result.fast)

x=7
y=11
# The memoisation directory defaults to ./memo, but can be reassigned:
myfunction=diskMemoiser(myfunction.slow, memo.dir="./temp")
result.slow=myfunction(x, y)
result.fast=myfunction(x, y)
stopifnot(result.slow == result.fast)


