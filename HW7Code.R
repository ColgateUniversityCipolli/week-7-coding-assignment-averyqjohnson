###############################################################################
# HW 7
# Avery Johnson
###############################################################################

# Step 1: Write a pois.prob() function
pois.prob <- function(x, lambda, type){
  if (type == "="){
    return (dpois(x, lambda))
  } else if (type== "!=") {
    return (1 - dpois(x, lambad))
  } else if (type== "<"){
    return(ppois(x-1, lambda))
  } else if (type == "<=") {
    return(ppois(x, lambda))
  } else if (type == ">") {
    return(1 - ppois(x, lambda))
  } else if (type == ">="){
    return(1 - ppois(x-1, lambda))
  }
}

# Step 2: Write a beta.prob() function
beta.prob <- function(x, alpha, beta, type){
  if (type == "="){
    return(0)
  } else if (type== "!=") {
    return(1)
  } else if (type== "<"){
    return(pbeta(x, alpha, beta))
  } else if (type == "<=") {
    return(pbeta(x, alpha, beta))
  } else if (type == ">") {
    return(1 - pbeta(x, alpha, beta))
  } else if (type == ">="){
    return(1 - pbeta(x, alpha, beta))
  }
}

# example
pois.prob(x=0, lambda=2, type="=")
beta.prob(x=0.4, alpha=2, beta=5, ">=")
