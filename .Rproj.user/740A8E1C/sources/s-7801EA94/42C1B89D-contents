#' @title myclt
#'
#' @param n
#' @param iter
#'
#' @return a histogram
#' @export
#'
#' @examples
myclt=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  hist(sm)
  sm
}
