#' @title ker
#' @description compute the value of a certain type of kernel function at a certain point, with total 6 types of kernels
#' @param x a single number, the point to calculate
#' @param type a character stirng, naming the type of kernel function to use. This must match one of "gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine", with default "gaussian"
#' @return  a single number, the value of the kernel function at point x
#' @examples
#' \dontrun{
#' x <- ker(1,"gaussian")
#' y <- ker(2,"biweight")
#' }
#' @importFrom stats IQR
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @export

ker<-function(x, type="gaussian"){
  
  # First check the correctness of the type input
  types<-c("gaussian", "rectangular", "triangular", "epanechnikov", "biweight", "cosine")
  if(sum(type==types)==0){
    print("Undefined kernel input!")
    return(NULL)
  }
  
  # Then compute the kernel
  if(type=="gaussian"){
    w<-dnorm(x)
  }
  else if(type=="rectangular"){
    w<-ifelse(abs(x)<1,1/2,0)
  }
  else if(type=="triangular"){
    w<-ifelse(abs(x)<1,1-abs(x),0)
  }
  else if(type=="epanechnikov"){
    w<-ifelse(abs(x)<1,0.75*(1-x^2),0)
  }
  else if(type=="biweight"){
    w<-ifelse(abs(x)<1,15/16*(1-x^2)^2,0)
  }
  else if(type=="cosine"){
    w<-ifelse(abs(x)<1,pi/4*cos(x*pi/2),0)
  }
  return(w)
}