// rcpp_auc.cpp
// ::rtemis::
// 2023 EDG lambdamd.org
// Ref: https://github.com/Laurae2/R_benchmarking

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double auc_cpp(NumericVector preds, NumericVector labels){
    double ncases = labels.size();
    NumericVector ranked(ncases);
    NumericVector positives = preds[labels == 1];
    double n1 = positives.size();
    Range positives_seq = seq(0, n1 - 1);
    ranked[seq(0, n1 - 1)] = positives;
    double n2 = ncases - n1;
    NumericVector negatives = preds[labels == 0];
    NumericVector x2(n2);
    ranked[seq(n1, n1 + n2)] = negatives;
    ranked = match(ranked, clone(ranked).sort());
    double AUC = (sum(ranked[positives_seq]) - n1 * (n1 + 1) / 2) / (n1 * n2);
    return AUC;
}
