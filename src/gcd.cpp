#include <Rcpp.h>
using namespace Rcpp;

//' @title gcd
//' @description Calculate the greatest common divisor of two numbers
//' @param a an integer
//' @param b an integer
//' @return x the greatest common divisor of two numbers
//' @examples
//' \dontrun{
//' a <- 6
//' b <- 8
//' mygcd(6,8);}
//' @export
// [[Rcpp::export]]

int gcd(int a,int b){     //计算a与b的最大公约数 
      int min,r,x;
      if(a<b){
            min=a;
            a=b;
            b=min;
      }
      r=a%b;
      while(r!=0){
            a=b;
            b=r;
            r=a%b;
      }
      x=b;
      return x;
}
