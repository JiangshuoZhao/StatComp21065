## ----eval=FALSE---------------------------------------------------------------
#  function (age, female, ily)
#  {
#      p <- 0.25 + 0.3 * 1/(1 - exp(0.04 * age)) + 0.1 * ily
#      p <- p * ifelse(female, 1.25, 0.75)
#      p <- pmax(0, p)
#      p <- pmin(1, p)
#      p
#  }

## ----eval=FALSE---------------------------------------------------------------
#  double vacc3a(double age, bool female, bool ily){
#    double p = 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily;
#    p = p * (female ? 1.25 : 0.75);
#    p = std::max(p, 0.0);
#    p = std::min(p, 1.0);
#    return p;
#  }
#  NumericVector vaccC(NumericVector age, LogicalVector female,
#                      LogicalVector ily) {
#    int n = age.size();
#    NumericVector out(n);
#    for(int i = 0; i < n; ++i) {
#      out[i] = vacc3a(age[i], female[i], ily[i]);
#    }
#    return out;
#  }

