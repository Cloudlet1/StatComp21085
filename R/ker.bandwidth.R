#' @title ker.bandwidth
#' @description get the bandwidth of a data set
#' @param data numeric vector, naming the data set
#' @param rule a character string, naming the rule to use. This should match one of "Scott", "Silverman", with the default being "Silverman"
#' @return  a single number, which is just the bandwidth
#' @examples
#' \dontrun{
#' library("gss")
#' data(buffalo)
#' bw <- ker.bandwidth(buffalo, "Silverman")
#' }
#' @importFrom stats IQR
#' @importFrom stats dnorm
#' @importFrom stats sd 
#' @export

ker.bandwidth<-function(data, rule="Silverman"){
  
  # Check the data
  if(is.numeric(data)==FALSE){
    print("Wrong data!")
    return(NULL)
  }
  
  # Check the type
  types<-c("Scott", "Silverman")
  if(sum(rule==types)==0){
    print("Wrong selection rule!")
    return(NULL)
  }
  
  # Having confirmed the validity of the inputs, begin our work
  n<-length(data)
  sigma<-sd(data)
  if(rule=="Scott"){
    h<-1.06*sigma*n^(-1/5)
  }
  else if(rule=="Silverman"){
    s<-min(sigma, IQR(data)/1.34)
    h<-0.9*s*n^(-1/5)
  }
  return(h)
}