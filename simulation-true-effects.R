
corr.coef = seq(-0.5,0.5, 0.02)
result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
true.effects = vector(mode = "list", length = length(corr.coef))  #preallocate list
n.true.effect = 10000000
set.seed(4235)
for (i in 1:length(true.effects)) { #Calculate true effects for given scenario while increasing interaction effect
  true.effects[[i]] = simulate.true.effects(n = n.true.effect,
                                            exposure.coefs =  c(I = -0.4, X = 0.01),
                                            mediator.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0),
                                            outcome.coefs = c(I = 5, Z = 1, M = 0.5, ZM = corr.coef[i], X = 0.05),
                                            outcome.mediator.type = "linear")
}
t.eff = matrix(NA, nrow = length(corr.coef), ncol = 3)
colnames(t.eff) = c("Z*M.coef", "true.nie", "true.nde")
t.eff[, 1] = corr.coef
for (i in 1:length(corr.coef)) {
  t.eff[i, 2] = true.effects[[i]][1]
  t.eff[i, 3] = true.effects[[i]][2]
}
true.effects = t.eff
save(true.effects, file="true.effects.RData")