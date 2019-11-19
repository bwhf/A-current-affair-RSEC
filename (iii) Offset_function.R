#offset no. nets function for binomial models from
# Based on Ben Bolker's code at https://rpubs.com/bbolker/logregexp	

library(MASS)

logexp <- function(exposure = 1)
{
  linkfun <- function(mu) qlogis(mu^(1/exposure))
  linkinv <- function(eta)  plogis(eta)^exposure
  logit_mu_eta <- function(eta) {
    ifelse(abs(eta)>30,.Machine$double.eps,
           exp(eta)/(1+exp(eta))^2)
      }
  mu.eta <- function(eta) {       
    exposure * plogis(eta)^(exposure-1) *
      logit_mu_eta(eta)
  }
  valideta <- function(eta) TRUE
  link <- paste("logexp(", deparse(substitute(exposure)), ")",
                sep="")
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, 
                 name = link),
            class = "link-glm")
}
