#############################################
######## Simulations, my-confounding ########
#### Continuous mediator, binary outcome ####
#############################################

library(sensmediation)
library(mvtnorm)

n <- 1000 # Size of data
S <- 1000 # Number of replicates
rho <- c(0,0.5) # Correlation vector used for sensitivity analyses

# Storage of results
effekter.nie2 <- matrix(nrow=S,ncol=length(rho)) # Estimated NIE for different values of rho
effekter.nde2 <- matrix(nrow=S,ncol=length(rho)) # Estimated NDE for different values of rho
CI.nie.lower2 <- matrix(nrow=S,ncol=length(rho)) # Lower bound of 95% CI of NIE for different values of rho
CI.nie.upper2 <- matrix(nrow=S,ncol=length(rho)) # Upper bound of 95% CI of NIE for different values of rho
CI.nde.lower2 <- matrix(nrow=S,ncol=length(rho)) # Lower bound of 95% CI of NDE for different values of rho
CI.nde.upper2 <- matrix(nrow=S,ncol=length(rho)) # Upper bound of 95% CI of NDE for different values of rho
nie.S <- numeric(0) # True NIE 
nde.S <- numeric(0) # True NDE
states <- list() # List to store the states of the random number generator, to be able to replicate results


R <- 0.5 # True correlation between error terms of mediator and outcome model
Sigma <- cbind(c(1,R),c(R,1)) # Covariance matrix to generate error terms for mediator and outcome models

# Simulations:
set.seed(23743)
for(i in 1:S){
 
   if(i %% 50 == 0)
    print(i)
  
  # .Random.seed <- states[[i]]
  
  epsilon <- rmvnorm(n,sigma=Sigma) # Correlated error terms, mediator and outcome models
  z.epsilon <- rnorm(n) # Error term for exposure model.
  
  X <- 104-rgamma(n,shape=7,scale=4.5) # "Age"
  
  # Generate exposure, mediator and outcome (True models):
  Z.star <- -1 + 0.01*X + z.epsilon
  Z <- ifelse(Z.star>0,1,0)
  M <- -2 + 0.14*Z + 0.011*X + epsilon[,1] 
  Y.star <- -3.5 + 0.05*Z + 1.5*M + 0.035*X + epsilon[,2] 
  Y <- ifelse(Y.star > 0, 1, 0)
  
  # Estimated models (m.model and y.model biased because error terms correlated):
  m.model <- glm(M ~ Z + X) 
  y.model <- glm(Y ~ Z + M + X, family=binomial(link='probit'))
  
  
  # Estimation of effects:
  test.my2 <- sensmediation(med.model=m.model, out.model=y.model, Rho=rho,progress=FALSE,exp.name = "Z", med.name = "M")
  
  # Storage of results
  effekter.nie2[i,] <- test.my2$NIE
  effekter.nde2[i,] <- test.my2$NDE
  CI.nie.lower2[i,] <- test.my2$CI$CI.nie[,1]
  CI.nie.upper2[i,] <- test.my2$CI$CI.nie[,2]
  CI.nde.lower2[i,] <- test.my2$CI$CI.nde[,1]
  CI.nde.upper2[i,] <- test.my2$CI$CI.nde[,2]
  
  # To get true NIE and NDE:
  true.pars2 <- test.my2$coefs.sensmed
  true.pars2$Rho <- 0
  true.pars2$coef <- NULL  
  true.pars2$coef <- as.matrix(c(-3.5,0.05,1.5,0.035))
  true.pars2$expl.coef <- NULL
  true.pars2$expl.coef <- as.matrix(c(-2,0.14,0.011)) 
  sanna.eff <- calc.effects(true.pars2, exp.name = "Z", med.name = "M") 
  nie.S[i] <- sanna.eff$effects$NIE
  nde.S[i] <- sanna.eff$effects$NDE
  
 # if(i==S)
  #  states[[i+1]] <- .Random.seed
}

mean(nie.S)
mean(effekter.nie2[, 1])
mean(effekter.nie2[, 2])
mean(nde.S)
mean(effekter.nde2[, 1])
mean(effekter.nde2[, 2])
