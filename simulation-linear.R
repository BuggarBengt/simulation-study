library(sensmediation)
library(ggplot2)

calc.nde.linear = function(z.from, z.to, x, b, t) {
  return((t[2] + t[4]*b[1] + t[4]*b[2]*z.from + t[4]*b[3]*x)*(z.to-z.from))
}

calc.nie.linear = function(z.from, z.to, b, t) {
  return((t[3]*b[2] + t[4]*b[2]*z.to)*(z.to-z.from))
}

runSimulation = function(S, n, exposure.coefs, mediator.coefs, outcome.coefs, exposure.error.sd = 1, mediator.error.sd = 1, outcome.error.sd = 1) {
  # Storage of results
  effekter.nie <- rep(NA, S) # Estimated NIE 
  effekter.nde <- rep(NA, S) # Estimated NDE 
  CI.nie.lower <- rep(NA, S) # Lower bound of 95% CI of NIE  
  CI.nie.upper <- rep(NA, S) # Upper bound of 95% CI of NIE
  CI.nde.lower <- rep(NA, S) # Lower bound of 95% CI of NDE
  CI.nde.upper <- rep(NA, S) # Upper bound of 95% CI of NDE
  nde.S <- numeric(S) # True NDE
  nie.S = calc.nie.linear(z.from = 0, z.to = 1, b = mediator.coefs, t = outcome.coefs) # True NIE 
  
  for(i in 1:S){
    if(i %% 50 == 0)
      print(i)
    
    z.epsilon <- rnorm(n, sd = exposure.error.sd) # Error terms for each model.
    m.epsilon <- rnorm(n, sd = mediator.error.sd) 
    y.epsilon <- rnorm(n, sd = outcome.error.sd) 
    
    X <- 104-rgamma(n,shape=7,scale=4.5) # "age" 
    
    # Generate exposure, mediator and outcome (True models):
    Z.star <- exposure.coefs["a0"] + exposure.coefs["a1"]*X + z.epsilon
    Z <- ifelse(Z.star>0, 1, 0)
    M <- mediator.coefs["b0"] + mediator.coefs["b1"]*Z + mediator.coefs["b2"]*X + m.epsilon
    Y <- outcome.coefs["t0"] + outcome.coefs["t1"]*Z + outcome.coefs["t2"]*M + outcome.coefs["t3"]*Z*M + outcome.coefs["t4"]*X + y.epsilon
    
    nde.S[i] = mean(calc.nde.linear(z.from = 0, z.to = 1, b = mediator.coefs, t = outcome.coefs, x = X))
    
    # Estimated models (y.model misspecified without Z-M correlation):
    m.model <- glm(M ~ Z + X) 
    y.model <- glm(Y ~ Z + M + X)
    
    # Estimation of effects:
    test.my <- sensmediation(med.model=m.model, out.model=y.model, Rho=0, progress=FALSE, exp.name = "Z", med.name = "M")
    
    # Storage of results
    effekter.nie[i] <- test.my$NIE
    effekter.nde[i] <- test.my$NDE
    CI.nie.lower[i] <- test.my$CI$CI.nie[,1]
    CI.nie.upper[i] <- test.my$CI$CI.nie[,2]
    CI.nde.lower[i] <- test.my$CI$CI.nde[,1]
    CI.nde.upper[i] <- test.my$CI$CI.nde[,2]
  }  
  
  return(list(true.nie = nie.S,
              true.nde = nde.S,
              est.nie = effekter.nie, 
              est.nde = effekter.nde, 
              CI.nie.lower = CI.nie.lower,
              CI.nie.upper = CI.nie.upper,
              CI.nde.lower = CI.nde.lower,
              CI.nde.upper = CI.nde.upper))
}

set.seed(4235)
corr.coef = seq(0, 5, 0.01)
to.plot = matrix(nrow = length(corr.coef), ncol = 3) #preallocate vector to plot
colnames(to.plot) = c("nde.bias", "nie.bias","interaction.coefficient")
result = vector(mode = "list", length = length(corr.coef))  #preallocate result list
for (i in 1:length(result)) {
  result[[i]] = runSimulation(S = 100, n = 1000, exposure.coefs = c(a0 = -3.416096, a1 = 0.036231), 
                         mediator.coefs = c(b0 = 10, b1 = 1.4, b2 = 0.011, b3 = 0), 
                         outcome.coefs = c(t0 = 10, t1 = 0.5, t2 = 0.15, t3=corr.coef[i], t4 = 0.035, t5 = 0, t6 = 0, t7 = 0))
  to.plot[i, 1] = mean(result[[i]]$est.nde) - mean(result[[i]]$true.nde)
  to.plot[i, 2] = mean(result[[i]]$est.nie) - result[[i]]$true.nie
  to.plot[i, 3] = corr.coef[i]
}
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nde.bias)) +
  geom_point() +
  geom_line()


result[[1]]$


result2 = runSimulation(S = 1000, n = 1000, exposure.coefs = c(a0 = -3.416096, a1 = 0.036231), 
                        mediator.coefs = c(b0 = 10, b1 = 1.4, b2 = 0.011, b3 = 0), 
                        outcome.coefs = c(t0 = 10, t1 = 0.5, t2 = 0.15, t3=0.0, t4 = 0.035, t5 = 0, t6 = 0, t7 = 0))


mean(ifelse(result$true.nie < result$CI.nie.upper & result$true.nie > result$CI.nie.lower, 1, 0))
mean(ifelse(result$true.nde < result$CI.nde.upper & result$true.nde > result$CI.nde.lower, 1, 0))

NIE.string = paste("True NIE: ", round(mean(result$true.nie), 4), " Est. NIE: ", round(mean(result$est.nie), digits = 4), " Avg. CI width: (", round(mean(result$CI.nie.lower), digits = 4), "; ", round(mean(result$CI.nie.upper), digits = 4), ")", sep = "") 
NDE.string = paste("True NDE: ", round(mean(result$true.nde), 4), " Est. NDE: ", round(mean(result$est.nde), digits = 4), " Avg. CI width: (", round(mean(result$CI.nde.lower), digits = 4), "; ", round(mean(result$CI.nde.upper), digits = 4), ")", sep = "") 
NIE.string
NDE.string
