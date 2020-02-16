rho = seq(-0.5,0.5, 0.05)
result = vector(mode = "list", length = length(rho))  #preallocate simulation result list
set.seed(4235)
start_time = Sys.time()
for (i in 1:length(result)) { # run simulations while increasing interaction effect
  result[[i]] = run.simulation(iterations = 10000,
                               n = 2000,
                               covariate.models = c("x-gamma"),
                               covariate.parameters = list(c(104, 8, 4.5)),
                               true.exposure.coefs = c(I = -3.416096, X = 0.036231),
                               true.mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0),
                               true.outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0),
                               outcome.mediator.type = "probit",
                               sd.exposure = 1,
                               sd.mediator = 1,
                               sd.outcome = 1,
                               misspecified.mediator.formula = "M~Z+X",
                               misspecified.outcome.formula = "Y~Z+M+X+Z*M", # Remenber to use correct formula for confounding!
                               mediator.outcome.corr = rho[i])
  print(i/length(result))
}
end_time = Sys.time()
end_time - start_time

save(result, file="Data/Data-probit/result.probit.confounding.i10000.n2000.RData") # store the result
load(file="Data/Data-probit/result.probit.confounding.i10000.n2000.RData") # load result
load(file="Data/Data-probit/true.effects.confounding.probit.RData") # load result

result.summary.NDE = vector(mode = "list", length = length(result))
result.summary.NIE = vector(mode = "list", length = length(result))
for (i in 1:length(result)) { # get different statistics of our estimates using rsimsum for each scenario
  result.summary.NDE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nde", true = mean(true.effects[, 2]), se = "SE.nde")
  result.summary.NIE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nie", true = mean(true.effects[, 1]), se = "SE.nie")
}

to.plot = create.data.frame.for.plotting(result.summary.NDE, result.summary.NIE, rho) # rho instead of corr.coef. We want one row for each value of rho
colnames(to.plot)[1] = "corr"

save(to.plot, file="Data/Data-probit/to-plot-probit-confounding.i10000.n2000.RData") # store the results
load("Data/Data-probit/to-plot-probit-confounding.i10000.n2000.RData")# read plot data
to.plot.c = to.plot

c.nde = ggplot() + 
  ggtitle("True vs estimated NDE(confounding)") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.position = c(0.86, 0.8)) +
  geom_line(data = data.frame(to.plot.c), aes(x=corr, y = nde.est, col = "est NDE")) +
  geom_line(data = data.frame(to.plot.c), aes(x=corr, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot.c), aes(x=corr, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect')
c.nie = ggplot() + 
  ggtitle("True vs estimated NIE(confounding)") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.position = c(0.15, 0.8)) +
  geom_line(data = data.frame(to.plot.c), aes(x=corr, y = nie.est, col = "est NIE")) +
  geom_line(data = data.frame(to.plot.c), aes(x=corr, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot.c), aes(x=corr, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('total effect')
grid.arrange(c.nde, c.nie, nrow = 1)









