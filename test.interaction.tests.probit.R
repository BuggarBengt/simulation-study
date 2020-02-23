# Probit
covariate.models = c("x-gamma")
covariate.parameters = list(c(104, 8, 4.5))
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

result = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"),med.model = "probit", out.model = "probit")

# Alpha test probit

# No interaction reality
exp.coefs = c(I = -3.5, X = 0.1)
med.coefs = c(I = -6, Z = 2, X = 0.1, ZX = 0)
out.coefs = c(I = -6, Z = 1, M = 3, ZM = 0, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

iter = 5000
n    = 2000
p.values.p.test   = vector(mode = "numeric", length = iter)
p.values.def.test = vector(mode = "numeric", length = iter)
diff.def.test = vector(mode = "numeric", length = iter)
set.seed(12)
start_time = Sys.time()
for (i in 1:iter) {
  if (i%%10 == 0) {
    print(i/iter) 
  }
  X   = rnorm(n = n, mean = 50, 10)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X, X2, X3)
  
  p.values.p.test[i] = interaction.test.out.p.value(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), out.model = "probit")
  def.test = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X") ,med.model = "probit", out.model = "probit")
  diff.def.test[i] = def.test[1, 1]
  p.values.def.test[i] = def.test[1, 3]
}
end_time = Sys.time()
end_time - start_time
hist(p.values.p.test, breaks = 50)
hist(p.values.def.test)
hist(diff.def.test, breaks = 50)

shapiro.test(diff.def.test)
library("car")
qqPlot(diff.def.test)

save(p.values.p.test, file = "Data/Data-test-tests/probit.p.values.p.n.20000.i.5000")
save(p.values.def.test, file = "Data/Data-test-tests/probit.p.values.diff.n.20000.i.5000")
save(diff.def.test, file = "Data/Data-test-tests/probit.diff.n.20000.i.5000")












