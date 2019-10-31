library(ggplot2)

set.seed(4235)
corr.coef = seq(0, 1, 0.02)
to.plot = matrix(nrow = length(corr.coef), ncol = 11) #preallocate vector to plot
colnames(to.plot) = c("interaction.coefficient", "true.nde", "true.nie", "est.nde", "est.nie", "nde.bias", "nie.bias", "nde.percent.bias", "nie.percent.bias","nde.coverage", "nie.coverage")
result = vector(mode = "list", length = length(corr.coef))  #preallocate result list
for (i in 1:length(result)) { # run simulations while increasing interaction effect
  result[[i]] = run.simulation(iterations = 100,
                               n = 1000,
                               covariate.models = c("gamma"),
                               covariate.parameters = list(c(8, 4.5)),
                               true.exposure.coefs =  c(I = 0.35512882, X = 0.00206941),
                               true.mediator.coefs = c(I = 50, Z = 5, X = 0.11, ZX = 0),
                               true.outcome.coefs = c(I = 50, Z = 5, M = 5, ZM = corr.coef[i], X = 0.35),
                               outcome.mediator.type = "linear",
                               sd.exposure = 1,
                               sd.mediator = 1,
                               sd.outcome = 1,
                               misspecified.mediator.formula = "M~Z+X",
                               misspecified.outcome.formula = "Y~Z+M+X")
  to.plot[i, 1] = corr.coef[i]
  to.plot[i, 2] = mean(result[[i]]$true.nde)
  to.plot[i, 3] = mean(result[[i]]$true.nie)
  to.plot[i, 4] = mean(result[[i]]$est.nde)
  to.plot[i, 5] = mean(result[[i]]$est.nie)
  to.plot[i, 6] = mean(result[[i]]$est.nde - result[[i]]$true.nde)
  to.plot[i, 7] = mean(result[[i]]$est.nie) - result[[i]]$true.nie
  to.plot[i, 8] = mean((result[[i]]$est.nde - result[[i]]$true.nde)/result[[i]]$true.nde)
  to.plot[i, 9] = mean((result[[i]]$est.nie - result[[i]]$true.nie)/result[[i]]$true.nie)
  to.plot[i, 10] = mean(ifelse(result[[i]]$true.nde < result[[i]]$CI.nde.upper & result[[i]]$true.nde > result[[i]]$CI.nde.lower, 1, 0))
  to.plot[i, 11] = mean(ifelse(result[[i]]$true.nie < result[[i]]$CI.nie.upper & result[[i]]$true.nie > result[[i]]$CI.nie.lower, 1, 0))
}

write.table(to.plot, "toplot.txt") # store the results
to.plot.read = read.table("toplot.txt") # read results

ggplot() + #mycket högre effekt på true nde av att öka korrelationen
  geom_line(data = data.frame(to.plot.read), aes(x=interaction.coefficient, y = true.nde, col = "NDE")) +
  geom_line(data = data.frame(to.plot.read), aes(x=interaction.coefficient, y = true.nie, col = "NIE")) +
  xlab('correlation.coef') +
  ylab('est.effect')
ggplot() + 
  geom_line(data = data.frame(to.plot.read), aes(x=interaction.coefficient, y = est.nde, col = "est NDE")) +
  geom_line(data = data.frame(to.plot.read), aes(x=interaction.coefficient, y = true.nde, col = "true NDE")) +
  xlab('correlation.coef') +
  ylab('est.effect')
ggplot() + 
  geom_line(data = data.frame(to.plot.read), aes(x=interaction.coefficient, y = est.nie, col = "est NIE")) +
  geom_line(data = data.frame(to.plot.read), aes(x=interaction.coefficient, y = true.nie, col = "true NIE")) +
  xlab('correlation.coef') +
  ylab('est.effect')

ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = est.nde)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = est.nie)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nde.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nie.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nie.bias + nde.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nde.percent.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nie.percent.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nde.coverage)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nie.coverage)) +
  geom_point() +
  geom_line()




result2 = runSimulation(S = 1000, n = 1000, exposure.coefs = c(a0 = -3.416096, a1 = 0.036231), 
                        mediator.coefs = c(b0 = 10, b1 = 1.4, b2 = 0.011, b3 = 0), 
                        outcome.coefs = c(t0 = 10, t1 = 0.5, t2 = 0.15, t3=0.0, t4 = 0.035, t5 = 0, t6 = 0, t7 = 0))

mean(ifelse(result$true.nie < result$CI.nie.upper & result$true.nie > result$CI.nie.lower, 1, 0))
mean(ifelse(result$true.nde < result$CI.nde.upper & result$true.nde > result$CI.nde.lower, 1, 0))

NIE.string = paste("True NIE: ", round(mean(result$true.nie), 4), " Est. NIE: ", round(mean(result$est.nie), digits = 4), " Avg. CI width: (", round(mean(result$CI.nie.lower), digits = 4), "; ", round(mean(result$CI.nie.upper), digits = 4), ")", sep = "") 
NDE.string = paste("True NDE: ", round(mean(result$true.nde), 4), " Est. NDE: ", round(mean(result$est.nde), digits = 4), " Avg. CI width: (", round(mean(result$CI.nde.lower), digits = 4), "; ", round(mean(result$CI.nde.upper), digits = 4), ")", sep = "") 
NIE.string
NDE.string



# NOT RUN {
data("MIsim", package = "rsimsum")
s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
# If 'ref' is not specified, the reference method is inferred
s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
# }



