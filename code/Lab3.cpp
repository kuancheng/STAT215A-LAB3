#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]

double SimilarityC(NumericVector l1Inter, NumericVector l2Inter, String method = "matching") {
	int d = l1Inter.size();
	double n11 = 0;
	double n10 = 0;
	double n01 = 0;
	double n00 = 0;

	for(int i = 0; i < d; i++){
		for(int j = 0; j < d; j++){
		  if (i == j) {
		    n00++;
		    continue;
		  }
			int num = (l1Inter[i] == l1Inter[j] ? 1 : 0) + (l2Inter[i] == l2Inter[j] ? 1 : 0);
			if (num == 2) {
				n11++;
			}
			else if (num == 1) {
			  if ((l1Inter[i] == l1Inter[j] ? 1 : 0) == 1) {
			  	n10++;
			  }
			  else{
			    n01++;
			  }
			}
			else {
				n00++;
			}
		}
	}
	if (method == "matching") {
	  return (n00 + n11) / (n00 + n11 + n01 + n10);
	}
	else if (method == "Jaccard") {
	  return n11 / (n11 + n10 + n01);
	}
	else{
	  return n11/(sqrt(n10 + n11) * sqrt(n01 + n11) );
	}
 
}


/*** R
x1 <- c(1, 1, 2, 2, 3, 4, 6)
x2 <- c(1, 1, 2, 3, 5, 6, 7)

SimilarityC(x1, x2, "Jaccard")
SimilarityC(x1, x2, "matching")
SimilarityC(x1, x2, "cosine")

*/
