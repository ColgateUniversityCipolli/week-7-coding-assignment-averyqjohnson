###############################################################################
# HW 7
# Avery Johnson
###############################################################################

# Step 1: Write a pois.prob() function
# define a function to compute Poisson probabilities
pois.prob <- function(x, lambda, type){
  if (type == "="){
    return (dpois(x, lambda)) #P(X=x) def of PMF
  } else if (type== "!=") {
    return (1 - dpois(x, lambad)) # complement rule 
  } else if (type== "<"){
    return(ppois(x-1, lambda))  # P(X < x) = P(X <= x-1)
  } else if (type == "<=") {
    return(ppois(x, lambda)) # P (X < = x) def of CDF
  } else if (type == ">") {
    return(1 - ppois(x, lambda)) # complement rule
  } else if (type == ">="){
    return(1 - ppois(x-1, lambda)) # P (X >= x) = 1 - P(X<x) = 1 - P(X <= x-1)
  }
}

# Step 2: Write a beta.prob() function
# define a function to compute Beta probabilities
beta.prob <- function(x, alpha, beta, type){
  if (type == "="){
    return(0)                 # prob of exact value of continuous dist always 0
  } else if (type== "!=") {
    return(1)                 # complement rule
  } else if (type== "<"){     
    return(pbeta(x, alpha, beta)) # P(X<x) bc continous = P(X<=x)
  } else if (type == "<=") {
    return(pbeta(x, alpha, beta)) # P(X<=x) def of CDF
  } else if (type == ">") {
    return(1 - pbeta(x, alpha, beta)) # complement rule
  } else if (type == ">="){
    return(1 - pbeta(x, alpha, beta)) # same as above bc continuous
  }
}

# example
pois.prob(x=0, lambda=2, type="=")
beta.prob(x=0.4, alpha=2, beta=5, ">=")
