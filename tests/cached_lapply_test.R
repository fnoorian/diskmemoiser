library("diskmemoiser")

global_counter = 0
funcx <- function(x) {print(x); global_counter <<- global_counter + 1; sin(x)}
cacher = diskCache()
sin.m = cacheMemoiser(funcx, cacher, compare.args.as.characters = TRUE)
a = lapply(1:10, sin.m)

a2 = cachedLapply(c(1:15, 1:15, 1:15), funcx, cacher, compare.args.as.characters = TRUE)
a3 = lapply(c(1:15, 1:15, 1:15), sin)
stopifnot(sum(abs(as.numeric(a2) - as.numeric(a3))) < 1e-6)
stopifnot(global_counter == 15)

