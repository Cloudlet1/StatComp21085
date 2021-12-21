#' @title ker.estimation
#' @description compute kernel density estimates of a data set
#' @param data numeric vector, the data from which the estimate is to be computed
#' @param bw a single number, specifying the bandwidth. There is also a function to help the user to find the appropriate bandwidth
#' @param kernel a character string, naming the type of kernel function to use. This must match one of "gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine", with default "gaussian"
#' @return  a function, the kernel estimate function, with one input which is the point x
#' @examples
#' \dontrun{
#' library("gss")
#' data(buffalo)
#' bw <- ker.bandwidth(buffalo, "Silverman")
#' density <- ker.estimation(buffalo, bw)
#' }
#' @importFrom stats IQR
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @export

ker.estimation<-function(data, bw, kernel="gaussian"){
  
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
  n<-length(data)
  s<-0
  estimation<-function(x){  
    m<-length(x)
    fhat<-rep(0,m)
    for(i in 1:m){
      s<-0
      for(j in 1:n){
        s<-s+1/(n*bw)*ker((data[j]-x[i])/bw,      type=kernel)
      }
      fhat[i]<-s
    }
    return(fhat)
  }
  return(estimation)
}