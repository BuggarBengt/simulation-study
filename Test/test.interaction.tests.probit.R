# Probit
true.exposure.coefs = c(I = -3.416096, X = 0.036231)
true.mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
true.outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

n   = 2000
X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
X2  = rgamma(n = n, shape = 5, scale = 5)
X3  = rgamma(n = n, shape = 3, scale = 3)
Z.s = -3.416096 + 0.036231*X + rnorm(n = n, mean = 0, sd = 1)
Z   = ifelse(Z.s>0, 1, 0)
M.s   = -1.6507546 + 0.2683970*Z + 0.0065543*X + rnorm(n = n, mean = 0, sd = 1)
M   = ifelse(M.s>0, 1, 0)
Y.s   = -3.7220626 + 0.2763912*Z + 1.4729651*M - 0.2583784*Z*M + 0.0283196*X + rnorm(n = n, mean = 0, sd = 1)
Y   = ifelse(Y.s>0, 1, 0)
data = data.frame(Y, M, Z, X, X2, X3)

result = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"),med.model = "probit", out.model = "probit", bootstrap = T)

# Alpha test probit

# No interaction reality
# exp.coefs = c(I = -3.5, X = 0.05)
# med.coefs = c(I = -6, Z = 2, X = 0.1, ZX = 0)
# out.coefs = c(I = -4, Z = 2, M = 2, ZM = 0, X = 0.025, ZX = 0, MX = 0, ZMX = 0)
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

iter = 100000
n    = 1000
p.values.p.test   = vector(mode = "numeric", length = iter)
p.values.def.test = vector(mode = "numeric", length = iter)
diff.def.test = vector(mode = "numeric", length = iter)
set.seed(123)
start_time = Sys.time()
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  p.values.p.test[i] = interaction.test.out.p.value(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), out.model = "probit")
  def.test = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), med.model = "probit", out.model = "probit")
  diff.def.test[i] = def.test[1, 1]
  p.values.def.test[i] = def.test[1, 3]
  
  if (i%%10 == 0) {
    print(i/iter) 
    print(paste("time left: ", (iter-i)/i*(Sys.time()-start_time)))
  }
}
end_time = Sys.time()
end_time - start_time
hist(p.values.p.test, breaks = 50)
hist(p.values.def.test)
hist(diff.def.test, breaks = 50)
mean(diff.def.test)

shapiro.test(diff.def.test)
library("car")
qqPlot(diff.def.test)

save(p.values.p.test, file = "Data/Data-test-tests/probit.p.values.p.n.1000.i.100000")
save(p.values.def.test, file = "Data/Data-test-tests/probit.p.values.diff.n.1000.i.100000")
save(diff.def.test, file = "Data/Data-test-tests/probit.diff.n.1000.i.100000")

# Power
# Interaction reality
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

interaction.coefs = seq(0, 0.5, 0.05) # true interaction coefs to get p-values for
sign.level = 0.05
austin.powers  = vector("numeric", length(interaction.coefs))
p.value.powers = vector("numeric", length(interaction.coefs))

iter = 10000
n    = 2000
set.seed(12)
start_time = Sys.time()
for (c in 1:length(interaction.coefs)) {
  p.values.p.test   = vector(mode = "numeric", length = iter)
  p.values.def.test = vector(mode = "numeric", length = iter)
  for (i in 1:iter) {
    X   = rnorm(n = n, mean = 60, 10)
    Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
    Z   = ifelse(Z.s>0, 1, 0)
    M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
    M   = ifelse(M.s>0, 1, 0)
    Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
    Y   = ifelse(Y.s>0, 1, 0)
    data = data.frame(Y, M, Z, X)
    
    p.values.p.test[i] = interaction.test.out.p.value(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), out.model = "probit")
    def.test = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), med.model = "probit", out.model = "probit")
    p.values.def.test[i] = def.test[1, 3]
    
    if (i%%10 == 0) {
      print(paste("Coef.: ", interaction.coefs[c], "percentage done:", i/iter) )
    }
  }
  austin.powers[c]  = sum(p.values.def.test<0.05)/iter
  p.value.powers[c] = sum(p.values.p.test<0.05)/iter
}
end_time = Sys.time()
end_time - start_time

austin.powers-p.value.powers
t.test(austin.powers, p.value.powers, paired = T)
d.f = data.frame(test = c(rep("p value powers", length(interaction.coefs)), rep("austin powers", length(interaction.coefs))),
                 power = c(p.value.powers, austin.powers), 
                 true.int.coef = c(interaction.coefs, interaction.coefs))
library(ggplot2)
ggplot(data = d.f, aes(x=true.int.coef, y=power, group=test)) +
  geom_line(aes(color=test))+
  geom_point(aes(color=test))+
  labs(title="Linear, n=200, i=100000",x="True interaction coefficient", y = "Power")


load(file = "Data/Data-test-tests/linear.power.diff.n.200.i.100000")
load(file = "Data/Data-test-tests/linear.power.p.value.n.200.i.100000")

save(austin.powers, file = "Data/Data-test-tests/probit.power.diff.n.2000.i.10000")
save(p.value.powers, file = "Data/Data-test-tests/probit.power.p.value.n.2000.i.10000")




# Alpha test probit boot

# No interaction reality
# exp.coefs = c(I = -3.5, X = 0.05)
# med.coefs = c(I = -6, Z = 2, X = 0.1, ZX = 0)
# out.coefs = c(I = -4, Z = 2, M = 2, ZM = 0, X = 0.025, ZX = 0, MX = 0, ZMX = 0)
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

iter = 1000
n    = 1000
p.values.nde   = vector(mode = "numeric", length = iter)
p.values.nie = vector(mode = "numeric", length = iter)
diffs = vector(mode = "numeric", length = iter)
SEs = vector(mode = "numeric", length = iter)
set.seed(124)
start_time = Sys.time()
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  def.test.boot = interaction.test.multi.def.boot(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), 
                                             med.model.type = "probit", out.model.type =  "probit", R=1000)
  p.values.nde[i] = def.test.boot[1, 3]
  p.values.nie[i] = def.test.boot[2, 3]
  diffs[i] = def.test.boot[1, 1]
  SEs[i] = def.test.boot[1, 2]
  
  if (i%%10 == 0) {
    print(i/iter) 
    print(paste("time left: ", (iter-i)/i*(Sys.time()-start_time)))
  }
}
end_time = Sys.time()
end_time - start_time
save(p.values.nde, file = "Data/Data-test-tests/probit.boot.p.values.p.n.1000.i.1000.R.1000.nonzero")
save(diffs, file = "Data/Data-test-tests/probit.boot.diffs.p.n.1000.i.1000.R.1000.nonzero")
load(file = "Data/Data-test-tests/probit.boot.p.values.p.n.1000.i.1000.R.1000")
load(file = "Data/Data-test-tests/probit.boot.diffs.p.n.1000.i.1000.R.1000")

hist(p.values.nde, breaks = 50)
hist(p.values.nie, breaks = 15)
hist(diffs, breaks = 20)
mean(diffs)
median(diffs)


load(file = "Data/Data-test-tests/probit.boot.p.values.p.n.1000.i.1000.R.1000")
load(file = "Data/Data-test-tests/probit.boot.diffs.p.n.1000.i.1000.R.1000")
p1 = p.values.nde # - mean(t)
d1 = diffs
load(file = "Data/Data-test-tests/probit.boot.p.values.p.n.2000.i.2000.R.2000")
load(file = "Data/Data-test-tests/probit.boot.diffs.p.n.2000.i.2000.R.2000")
p2 = p.values.nde # - mean(t)
d2 = diffs
load(file = "Data/Data-test-tests/probit.boot.p.values.p.n.1000.i.1000.R.1000.nonzero")
load(file = "Data/Data-test-tests/probit.boot.diffs.p.n.1000.i.1000.R.1000.nonzero")
p3 = p.values.nde # - mean(t)+true diff under H0
d3 = diffs

par(mfrow = c(2, 2))
hist(p1, breaks = 30)
hist(p2, breaks = 30)
hist(p3, breaks = 30)
plot(density(p3))













exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

iter = 1000
n    = 1000
p.center = vector(mode = "numeric", length = iter)
p.sample = vector(mode = "numeric", length = iter)
p.center.nonzero = vector(mode = "numeric", length = iter)
p.sample.nonzero = vector(mode = "numeric", length = iter)
p.conf = vector(mode = "numeric", length = iter)
p.conf.nonzero = vector(mode = "numeric", length = iter)
diffs = vector(mode = "numeric", length = iter)
SEs = vector(mode = "numeric", length = iter)
set.seed(124)
start_time = Sys.time()
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  def.test.boot = interaction.test.multi.def.boot.multi(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), 
                                                  med.model.type = "probit", out.model.type =  "probit", R=1000)
  p.center[i] = def.test.boot[1]
  p.sample[i] = def.test.boot[2]
  p.center.nonzero[i] = def.test.boot[3]
  p.sample.nonzero[i] = def.test.boot[4]
  p.conf[i] = def.test.boot[5]
  p.conf.nonzero[i] = def.test.boot[6]
  diffs[i] = def.test.boot[7]
  
  if (i%%10 == 0) {
    print(i/iter) 
    print(paste("time left: ", (iter-i)/i*(Sys.time()-start_time)))
  }
}
end_time = Sys.time()
end_time - start_time

par(mfrow = c(2, 2))
hist(p.center, breaks = 30)
hist(p.sample, breaks = 30)
hist(p.center.nonzero, breaks = 30)
hist(p.sample.nonzero, breaks = 30)
hist(p.conf, breaks = 10)
hist(p.conf.nonzero, breaks = 10)
hist(diffs, breaks = 30)

save(p.center, file = "Data/Data-test-tests/probit.boot.p.values.n.1000.i.1000.R.1000.p.center")
save(p.sample, file = "Data/Data-test-tests/probit.boot.diffs.p.n.1000.i.1000.R.1000.p.sample")
save(p.center.nonzero, file = "Data/Data-test-tests/probit.boot.p.values.p.n.1000.i.1000.R.1000.p.center.nonzero")
save(p.sample.nonzero, file = "Data/Data-test-tests/probit.boot.diffs.p.n.1000.i.1000.R.1000.p.sample.nonzero")
save(p.conf, file = "Data/Data-test-tests/probit.boot.p.values.p.n.1000.i.1000.R.1000.p.conf")
save(p.conf.nonzero, file = "Data/Data-test-tests/probit.boot.diffs.p.n.1000.i.1000.R.1000.p.conf.nonzero")
save(diffs, file = "Data/Data-test-tests/probit.boot.diffs.p.n.1000.i.1000.R.1000.diffs")








# test to plot
X   = rnorm(n = n, mean = 60, 10)
Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
Z   = ifelse(Z.s>0, 1, 0)
M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
M   = ifelse(M.s>0, 1, 0)
Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
Y   = ifelse(Y.s>0, 1, 0)
data = data.frame(Y, M, Z, X)
med.model <- glm(M~X+Z, data = data, family = binomial(link = "probit")) 
out.model <- glm(Y~M+X+Z, data = data, family = binomial(link = "probit")) 
predicted.data <- as.data.frame(predict(med.model, newdata = data, 
                                        type="link", se=TRUE))
new.data <- cbind(data, predicted.data)
std <- qnorm(0.95 / 2 + 0.5)
new.data$ymin <- model$family$linkinv(new.data$fit - std * new.data$se)
new.data$ymax <- model$family$linkinv(new.data$fit + std * new.data$se)
new.data$fit <- model$family$linkinv(new.data$fit)  # Rescale to 0-1

library(ggplot2)
# Plot everything
p <- ggplot(data, aes(x=X, y=Y)) 
p + geom_point() + 
  geom_ribbon(data=new.data, aes(y=fit, ymin=ymin, ymax=ymax), alpha=0.5) + 
  geom_line(data=new.data, aes(y=fit)) + 
  labs(x="X", y="Y") 
