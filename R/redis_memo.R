#' Memoiser with Redis Backends
#'
#' Memoises a function using redis
#'
#' @param fun                         The funciton to memoise
#' @param use.func.contents           Boolean, to use the function contents (using deparse) or simply use its character name to store the results in DB
#' @param compare.args.as.characters  Boolean, to compare the function input arguements in binary or as character
#' @param redis.host                  Redis host IP
#' @param redis.port                  Redis host port number
#'
#' @return A memoised function. When called, connect to redis to see if results are available. If not, will run the function to get results.
#'
#' @importFrom digest digest
#' @importFrom RcppRedis Redis
#' @importFrom rredis redisExists redisGetContext
#' @importFrom methods new
#'
#' @examples \dontrun{
#' delay.func <- function(x) {
#'   cat("computing ", x, "\n")
#'
#'   Sys.sleep(1)
#'
#'   return (list(x, x^2))
#' }
#'
#' redis.memoiser.flush()
#'
#' library("parallel")
#' options(mc.cores = 8)
#'
#' a = c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,
#'       2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
#'
#' print("starting sims")
#'
#' delay.func.redis.m = redis.threadsafe.memoiser(delay.func)
#'
#' total_time = system.time({
#'   x = mclapply(a, delay.func.redis.m)
#' })
#'
#' print(x)
#' print(total_time)
#' }
#'
redis.memoiser <- function (fun, use.func.contents = FALSE,
                            compare.args.as.characters = FALSE,
                            redis.host=redisGetContext()$host, redis.port=redisGetContext()$port)
{
  redis <- new(RcppRedis::Redis, host=redis.host, port=redis.port)

  if (use.func.contents) {
    .func.name <- digest(deparse(fun))
  }
  else {
    .func.name <- as.character(substitute(fun))
  }

  return(function(...) {

    if (compare.args.as.characters) {
      .hash <- tolower(digest(as.character(list(...))))
    } else {
      .hash <- tolower(digest(list(...)))
    }

    key.name <- paste0("redismemo:", .func.name, ":", .hash)

    if (rredis::redisExists(key.name)) {
      result = redis$get(key.name)
    } else {
      result <- fun(...)
      redis$set(key.name, result)
    }

    return(result)
  })
}

#'  Memoisaztion using redis for parallel environment
#'
#'  In parallel environment, one needs to connect to redis again. This function does this automatically and returns a memoised function.
#'
#' @param fun                         The funciton to memoise
#' @param use.func.contents           Boolean, to use the function contents (using deparse) or simply use its character name to store the results in DB
#' @param compare.args.as.characters  Boolean, to compare the function input arguements in binary or as character
#' @param redis.host                  Redis host IP
#' @param redis.port                  Redis host port number
#'
#' @return A memoised function. When called, connect to redis to see if results are available. If not, will run the function to get results.
#'
#' @importFrom digest digest
#' @importFrom RcppRedis Redis
#' @importFrom rredis redisConnect redisClose
#' @export
redis.threadsafe.memoiser <- function(fun, use.func.contents = FALSE,
                                 compare.args.as.characters = FALSE,
                                 redis.host="127.0.0.1", redis.port=6379) {

  MEMO.FUN <- function(...) {
    rredis::redisConnect(nodelay=TRUE, host = redis.host, port = redis.port)

    fun.m = redis.memoiser(fun,
                           use.func.contents = use.func.contents,
                           compare.args.as.characters = compare.args.as.characters,
                           redis.host = redis.host,
                           redis.port = redis.port)

    ret = fun.m(...)

    rredis::redisClose()

    return(ret)
  }

  MEMO.FUN
}

#' Delete all memoized information in redis DB
#'
#' @param host                  Redis host IP
#' @param port                  Redis host port number
#'
#' @importFrom rredis redisConnect redisClose redisDelete redisKeys
#' @export
redis.memoiser.flush <- function(host="127.0.0.1", port=6379) {

  rredis::redisConnect(nodelay=TRUE, host = host, port = port)
  #rredis::redisFlushAll()

  keys <- redisKeys("redismemo:*")

  if (length(keys) > 0) {
    redisDelete(keys)
  }

  rredis::redisClose()
}

redis.memo.test <- function() {


}
