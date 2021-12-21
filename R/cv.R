#' @title cv
#' @description compute the integrated squared error estimated by cross validation
#' @param data numeric vector, the data from which the estimate is to be computed
#' @param bw a single number, specifying the bandwidth. There is also a function to help the user to find the appropriate bandwidth
#' @param kernel a character string, naming the type of kernel function to use. This must match one of "gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine", with default "gaussian"
#' @return  the integrated squared error estimated by cross validation with bandwidth bw
#' @examples
#' \dontrun{
#' data <- rexp(1000,1)
#' cv(data, "gaussian", 0.05)
#' }
#' @importFrom stats IQR
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @importFrom stats integrate
#' @export

cv <- function(data, kernel = "gaussian", bw){
  # First of all, check if data is a numeric
  if(is.numeric(data)==FALSE){
    print("Wrong data!")
    return(NULL)
  }
  
  # Then check if the kernel type is valid
  if(is.null(ker(0, kernel))){  
    return(NULL)
  }
  
  # If valid, begin our work
  n <- length(data)
  t <- 0
  estimation2 <- function(x){  
    s <- 0
    for(i in 1:n){
      s <- s + 1/(n*bw)*ker((data[i]-x)/bw, type = kernel)
    }
    return(s^2)
  }
  t1 <- integrate(estimation2, -Inf, Inf)$value
  for(i in 1:n){
    for(j in 1:n){
      if(j!=i){
        t <- t + ker((data[i]-data[j])/bw, kernel)/bw
      }
    }
  }
  
  t2 <- 2*t/(n*(n-1))
  return(t1 - t2)
}
