library(ggplot2)
library(rsimsum)
library(devtools)
install_github("wrathematics/openblasctl")
library(openblasctl)

corr.coef = seq(-50,50, 2)/100
result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
load("true.effects.RData")# read true.effects

set.seed(4235)
openblas_set_num_threads(1)
start_time = Sys.time()
for (i in 1:length(result)) { # run simulations while increasing interaction effect
  result[[i]] = run.simulation(iterations = 100,
                               n = 1000,
                               covariate.models = c("gamma"),
                               covariate.parameters = list(c(8, 4.5)),
                               true.exposure.coefs =  c(I = -0.4, X = 0.01),
                               true.mediator.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0),
                               true.outcome.coefs = c(I = 5, Z = 1, M = 0.5, ZM = corr.coef[i], X = 0.05, ZX = 0, MX = 0, ZMX = 0),
                               outcome.mediator.type = "linear",
                               sd.exposure = 1,
                               sd.mediator = 1,
                               sd.outcome = 1,
                               misspecified.mediator.formula = "M~Z+X",
                               misspecified.outcome.formula = "Y~Z+M+X")
  print(i/length(result))
}
end_time = Sys.time()
end_time - start_time

save(true.effects, file="true.effects3.RData") # store the true.effects
save(result, file="resultdsfs.RData") # store the results
load("true.effects.RData")# read true.effects
load("result.RData")# read results  s=1000
load("result2.RData")# read results s=10000
load("result3.RData")# read results s=100000

result.summary.NDE = vector(mode = "list", length = length(result))
result.summary.NIE = vector(mode = "list", length = length(result))
for (i in 1:length(result)) { # get different statistics of our estimates using rsimsum for each scenario
  result.summary.NDE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nde", true = true.effects[i, 3], se = "SE.nde")
  result.summary.NIE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nie", true = true.effects[i, 2], se = "SE.nie")
}

to.plot = create.data.frame.for.plotting(result.summary.NDE, result.summary.NIE, corr.coef)

save(to.plot, file="to-plot3.RData") # store the results
load("to-plot3.RData")

ggplot() + #mycket högre effekt på true nde av att öka korrelationen
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.true, col = "NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.true, col = "NIE")) +
  xlab('correlation.coef') +
  ylab('effect') +
  expand_limits(y=0)
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.est, col = "est NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=interaction.coefficient, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.est, col = "est NIE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=interaction.coefficient, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.coverage, col = "NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.coverage, col = "NIE")) +
  xlab('correlation.coef') +
  ylab('coverage') +
  ggtitle("Coverage of nominal 95% CI. Nominal = based on delta-SE?")
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.emp.SE, col = "emp. SE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.model.SE, col = "model. SE")) +
  xlab('correlation.coef') +
  ylab('SE') +
  ggtitle("Comparison of NDE SE's")
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.emp.SE, col = "emp. SE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.model.SE, col = "model. SE")) +
  xlab('correlation.coef') +
  ylab('SE') +
  ggtitle("Comparison of NIE SE's")

knitr::kable(to.plot, col.names = c("interaction.coefficient", "true.nde", "true.nie", "est.nde", "est.nie", 
                                               "nde.emp.SE", "nie.emp.SE", "nde.model.SE", "nie.model.SE", "nde.coverage", "nie.coverage"))
