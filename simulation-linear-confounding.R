library(ggplot2)
library(rsimsum)
library(gridExtra)

rhos = seq(-0.5,0.5, 0.02)
corr.coef = -0.2
result = vector(mode = "list", length = length(rhos))  #preallocate simulation result list

set.seed(4235)
#tmp <- tempfile() # time simulations
#Rprof(tmp)
openblas_set_num_threads(1)
start_time = Sys.time()
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
end_time = Sys.time()
end_time - start_time
# Rprof()
# summaryRprof(tmp)

<<<<<<< HEAD
save(result, file="result.confounding0.fds2.RData") # store the results
load("true.effects.RData")# read true.effects
load("result.confounding-0.2.RData")# read results
=======
save(result, file="result.confounding-0dsadas.RData") # store the results
load("true.effects.RData")# read true.effects
load("result.confounding0.5.RData")# read results
corr.coef = 0.5
>>>>>>> b15dd47e504ec2ceed93edc2b9106eab2752b733

result.summary.NDE = vector(mode = "list", length = length(result))
result.summary.NIE = vector(mode = "list", length = length(result))
for (i in 1:length(result)) { # get different statistics of our estimates using rsimsum for each scenario
<<<<<<< HEAD
  result.summary.NDE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nde", true = mean(true.effects[[16]][, 2]), se = "SE.nde")
  result.summary.NIE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nie", true = mean(true.effects[[16]][, 1]), se = "SE.nie")
=======
  result.summary.NDE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nde", true = true.effects[true.effects[, 1]==corr.coef, 3], se = "SE.nde")
  result.summary.NIE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nie", true = true.effects[true.effects[, 1]==corr.coef, 2], se = "SE.nie")
>>>>>>> b15dd47e504ec2ceed93edc2b9106eab2752b733
}

to.plot = create.data.frame.for.plotting(result.summary.NDE, result.summary.NIE, rhos)
colnames(to.plot)[1] = "corr"

<<<<<<< HEAD
save(to.plot, file="to-plot-confounding-0.2.RData") # store the results
load("to-plot-confounding-0.2.RData")# read true.effects
=======
save(to.plot, file="to-plot-confounding0.5.RData") # store the results
load("to-plot-confounding-0.2.RData")# read true.effects
to.plot.conf.n0.2 = to.plot
load("to-plot-confounding0.RData")# read true.effects
to.plot.conf.0 = to.plot
load("to-plot-confounding0.2.RData")# read true.effects
to.plot.conf.0.2 = to.plot
load("to-plot-confounding0.5.RData")# read true.effects
to.plot.conf.0.5 = to.plot
load("to-plot3.RData")# read true.effects
>>>>>>> b15dd47e504ec2ceed93edc2b9106eab2752b733

nde1 = ggplot() + 
  geom_line(data = data.frame(nde.est = rep(to.plot[to.plot[, "interaction.coefficient"]==-0.2, "nde.est"], length(rhos))), aes(x=rhos, y = nde.est, col = "est NDE misspecified")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nde.est, col = "est NDE M-Y confounding")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot.conf.n0.2), aes(x=corr, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect') +
  ggtitle("ZM.corr = -0.2")
nde2 = ggplot() + 
  geom_line(data = data.frame(nde.est = rep(to.plot[to.plot[, "interaction.coefficient"]==0, "nde.est"], length(rhos))), aes(x=rhos, y = nde.est, col = "est NDE misspecified")) +
  geom_line(data = data.frame(to.plot.conf.0), aes(x=corr, y = nde.est, col = "est NDE M-Y confounding")) +
  geom_line(data = data.frame(to.plot.conf.0), aes(x=corr, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot.conf.0), aes(x=corr, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect') +
  ggtitle("ZM.corr = 0")
nde3 = ggplot() + 
  geom_line(data = data.frame(nde.est = rep(to.plot[to.plot[, "interaction.coefficient"]==0.2, "nde.est"], length(rhos))), aes(x=rhos, y = nde.est, col = "est NDE misspecified")) +
  geom_line(data = data.frame(to.plot.conf.0.2), aes(x=corr, y = nde.est, col = "est NDE M-Y confounding")) +
  geom_line(data = data.frame(to.plot.conf.0.2), aes(x=corr, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot.conf.0.2), aes(x=corr, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect') +
  ggtitle("ZM.corr = 0.2")
nde4 = ggplot() + 
  geom_line(data = data.frame(nde.est = rep(to.plot[to.plot[, "interaction.coefficient"]==0.5, "nde.est"], length(rhos))), aes(x=rhos, y = nde.est, col = "est NDE misspecified")) +
  geom_line(data = data.frame(to.plot.conf.0.5), aes(x=corr, y = nde.est, col = "est NDE M-Y confounding")) +
  geom_line(data = data.frame(to.plot.conf.0.5), aes(x=corr, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot.conf.0.5), aes(x=corr, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect') +
  ggtitle("ZM.corr = 0.5")
grid.arrange(nde1, nde2, nde3, nde4, nrow = 2)

ggplot() + 
  geom_line(data = data.frame(nie.est = rep(to.plot[to.plot[, "interaction.coefficient"]==-0.2, "nie.est"], length(rhos))), aes(x=rhos, y = nie.est, col = "est NIE misspecified")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nie.est, col = "est NIE M-Y confounding")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot.conf.n0.2), aes(x=corr, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect') +
  ggtitle("ZM.corr = -0.2")
ggplot() + 
  geom_line(data = data.frame(nie.est = rep(to.plot[to.plot[, "interaction.coefficient"]==-0.2, "nie.est"], length(rhos))), aes(x=rhos, y = nie.est, col = "est NIE misspecified")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nie.est, col = "est NIE M-Y confounding")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot.conf.n0.2), aes(x=corr, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect') +
  ggtitle("ZM.corr = -0.2")
ggplot() + 
  geom_line(data = data.frame(nie.est = rep(to.plot[to.plot[, "interaction.coefficient"]==-0.2, "nie.est"], length(rhos))), aes(x=rhos, y = nie.est, col = "est NIE misspecified")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nie.est, col = "est NIE M-Y confounding")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot.conf.n0.2), aes(x=corr, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect') +
  ggtitle("ZM.corr = -0.2")
ggplot() + 
  geom_line(data = data.frame(nie.est = rep(to.plot[to.plot[, "interaction.coefficient"]==-0.2, "nie.est"], length(rhos))), aes(x=rhos, y = nie.est, col = "est NIE misspecified")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nie.est, col = "est NIE M-Y confounding")) +
  geom_line(data = data.frame(to.plot.conf.n0.2), aes(x=corr, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot.conf.n0.2), aes(x=corr, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect') +
  ggtitle("ZM.corr = -0.2")










ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=rhos, y = nie.est, col = "est NIE")) +
  geom_line(data = data.frame(to.plot), aes(x=rhos, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=rhos, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=rhos, y = nde.coverage, col = "NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=rhos, y = nie.coverage, col = "NIE")) +
  xlab('correlation.coef') +
  ylab('coverage') +
  ggtitle("Coverage of nominal 95% CI. Nominal = based on delta-SE?")
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=rhos, y = nde.emp.SE, col = "emp. SE")) +
  geom_line(data = data.frame(to.plot), aes(x=rhos, y = nde.model.SE, col = "model. SE")) +
  xlab('correlation.coef') +
  ylab('SE') +
  ggtitle("Comparison of NDE SE's")
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=rhos, y = nie.emp.SE, col = "emp. SE")) +
  geom_line(data = data.frame(to.plot), aes(x=rhos, y = nie.model.SE, col = "model. SE")) +
  xlab('correlation.coef') +
  ylab('SE') +
  ggtitle("Comparison of NIE SE's")

knitr::kable(to.plot, col.names = colnames(to.plot))
