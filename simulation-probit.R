library(ggplot2)
library(rsimsum)
library(gridExtra)

# corr.coef = seq(-1,1, 0.08)
# result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
# set.seed(4235)
# start_time = Sys.time()
# for (i in 1:length(result)) { # run simulations while increasing interaction effect
#   result[[i]] = run.simulation(iterations = 10000,
#                                n = 2000,
#                                covariate.models = c("x-gamma"),
#                                covariate.parameters = list(c(104, 8, 4.5)),
#                                true.exposure.coefs = c(I = -3.416096, X = 0.036231),
#                                true.mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0),
#                                true.outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = corr.coef[i], X = 0.0283196, ZX = 0, MX = 0, ZMX = 0),
#                                outcome.mediator.type = "probit",
#                                sd.exposure = 1,
#                                sd.mediator = 1,
#                                sd.outcome = 1,
#                                misspecified.mediator.formula = "M~Z+X",
#                                misspecified.outcome.formula = "Y~Z+M+X")
#   print(i/length(result))
# }
# end_time = Sys.time()
# end_time - start_time

corr.coef = seq(-1,1, 0.08)
result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
set.seed(4235)
start_time = Sys.time()
for (i in 1:length(result)) { # run simulations while increasing interaction effect
  result[[i]] = run.simulation(iterations = 10000,
                               n = 2000,
                               covariate.models = c("x-gamma"),
                               covariate.parameters = list(c(104, 8, 4.5)),
                               true.exposure.coefs = c(I = -3.416096, X = 0.036231),
                               true.mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0),
                               true.outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = corr.coef[i], X = 0.0283196, ZX = 0, MX = 0, ZMX = 0),
                               outcome.mediator.type = "probit",
                               sd.exposure = 1,
                               sd.mediator = 1,
                               sd.outcome = 1,
                               misspecified.mediator.formula = "M~Z+X",
                               misspecified.outcome.formula = "Y~Z+M+X")
  print(i/length(result))
}
end_time = Sys.time()
end_time - start_time


save(result, file="Data/Data-probit/result.probit.confounding.i10000.n2000.RData") # store the result
load("Data/Data-probit/true.effects.probit.RData")# read true.effects
load("Data/Data-probit/result.probit10000.RData")# read result

result.summary.NDE = vector(mode = "list", length = length(result))
result.summary.NIE = vector(mode = "list", length = length(result))
for (i in 1:length(result)) { # get different statistics of our estimates using rsimsum for each scenario
  result.summary.NDE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nde", true = mean(true.effects[[i]][, 2]), se = "SE.nde")
  result.summary.NIE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nie", true = mean(true.effects[[i]][, 1]), se = "SE.nie")
}

to.plot = create.data.frame.for.plotting(result.summary.NDE, result.summary.NIE, corr.coef)
save(to.plot, file="Data/Data-probit/to-plot-probit.reverse.i10000.n2000.RData") # store the results
load("Data/Data-probit/to-plot-probit.i100.n2000.RData")# read plot data
load("Data/Data-probit/to-plot-probit.i100.n1000.RData")# read plot data

int.nde = ggplot() + 
  ggtitle("True vs estimated NDE(missing int.coef.)") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.position = c(0.86, 0.22)) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.est, col = "est NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=interaction.coefficient, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('interaction.coef') +
  ylab('effect')
int.nie = ggplot() + 
  ggtitle("True vs estimated NIE(missing int.coef.)") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.position = c(0.86, 0.22)) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.est, col = "est NIE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=interaction.coefficient, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('interaction.coef') +
  ylab('effect')
grid.arrange(int.nde, int.nie, nrow =1)



par(mfrow=c(2,2))
hist(result[[26]][, 2], probability = T, main = "NDE, true theta3 = 1", xlab = "est. NDE", breaks=30)
d <- density(result[[26]][, 2])
lines(d, col="blue")
hist(result[[2]][, 2], probability = T, main = "NDE, true theta3 = ?", xlab = "est. NDE", breaks=30)
d <- density(result[[2]][, 2])
lines(d, col="blue")

hist(result[[26]][, 1], probability = T, main = "NIE, true theta3 = 1", xlab = "est. NIE", breaks=30)
d <- density(result[[26]][, 1])
lines(d, col="blue")
hist(result[[2]][, 1], probability = T, main = "NIE, true theta3 = ?", xlab = "est. NIE", breaks=30)
d <- density(result[[2]][, 1])
lines(d, col="blue")
