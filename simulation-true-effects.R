corr.coef = seq(-50,50, 2)/100
result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
true.effects = vector(mode = "list", length = length(corr.coef))  #preallocate list
true.effects = matrix(NA, nrow = 0, ncol = 3)  #preallocate result matrix
n.true.effect = 10000000
set.seed(4235)
for (i in 1:length(corr.coef)) { #Calculate true effects for given scenario while increasing interaction effect
  temp = cbind(ZM.corr = corr.coef[i], simulate.true.effects(n = n.true.effect,
                                            exposure.coefs =  c(I = -0.4, X = 0.01),
                                            mediator.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0),
                                            outcome.coefs = c(I = 5, Z = 1, M = 0.5, ZM = corr.coef[i], X = 0.05),
                                            outcome.mediator.type = "linear"))
  true.effects = rbind(true.effects, temp)
  print(i/length(corr.coef))
}
save(true.effects, file="true.effects.RData")
