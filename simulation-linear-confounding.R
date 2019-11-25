library(ggplot2)
library(rsimsum)

rhos = seq(-0.5,0.5, 0.02)
corr.coef = 0.2
result = vector(mode = "list", length = length(rhos))  #preallocate simulation result list

set.seed(4235)
tmp <- tempfile() # time simulations
Rprof(tmp)
for (i in 1:length(result)) { # run simulations while increasing interaction effect
  result[[i]] = run.simulation(iterations = 100000,
                               n = 1000,
                               covariate.models = c("gamma"),
                               covariate.parameters = list(c(8, 4.5)),
                               true.exposure.coefs =  c(I = -0.4, X = 0.01),
                               true.mediator.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0),
                               true.outcome.coefs = c(I = 5, Z = 1, M = 0.5, ZM = corr.coef, X = 0.05, ZX = 0, MX = 0, ZMX = 0),
                               outcome.mediator.type = "linear",
                               mediator.outcome.corr = rhos[i],
                               sd.exposure = 1,
                               sd.mediator = 1,
                               sd.outcome = 1,
                               misspecified.mediator.formula = "M~Z+X",
                               misspecified.outcome.formula = "Y~Z*M+X")
  print(i/length(result))
}
Rprof()
summaryRprof(tmp)

save(result, file="result.confounding0.2.RData") # store the results
load("true.effects.RData")# read true.effects
load("result.confounding-0.25.RData")# read results
test=true.effects
cbind(test, true.effects)

result.summary.NDE = vector(mode = "list", length = length(result))
result.summary.NIE = vector(mode = "list", length = length(result))
for (i in 1:length(result)) { # get different statistics of our estimates using rsimsum for each scenario
  result.summary.NDE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nde", true = mean(true.effects[[51]][, 2]), se = "SE.nde")
  result.summary.NIE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nie", true = mean(true.effects[[51]][, 1]), se = "SE.nie")
}

to.plot = create.data.frame.for.plotting(result.summary.NDE, result.summary.NIE, rhos)
colnames(to.plot)[1] = "corr"

save(to.plot, file="to-plot-confounding-0.25.RData") # store the results
load("to-plot-confounding-0.25.RData")# read true.effects

ggplot() + #mycket högre effekt på true nde av att öka korrelationen
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nde.true, col = "NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nie.true, col = "NIE")) +
  xlab('correlation.coef') +
  ylab('effect') +
  expand_limits(y=0)
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nde.est, col = "est NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=corr, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nie.est, col = "est NIE")) +
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=corr, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nde.coverage, col = "NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nie.coverage, col = "NIE")) +
  xlab('correlation.coef') +
  ylab('coverage') +
  ggtitle("Coverage of nominal 95% CI. Nominal = based on delta-SE?")
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nde.emp.SE, col = "emp. SE")) +
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nde.model.SE, col = "model. SE")) +
  xlab('correlation.coef') +
  ylab('SE') +
  ggtitle("Comparison of NDE SE's")
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nie.emp.SE, col = "emp. SE")) +
  geom_line(data = data.frame(to.plot), aes(x=corr, y = nie.model.SE, col = "model. SE")) +
  xlab('correlation.coef') +
  ylab('SE') +
  ggtitle("Comparison of NIE SE's")

knitr::kable(to.plot, col.names = colnames(to.plot))
