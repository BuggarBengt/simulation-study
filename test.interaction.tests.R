# Linear
exposure.coefs =  c(I = -0.4, X = 0.01)
mediator.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0)
outcome.coefs = c(I = 5, Z = 1, M = 0.5, ZM = 0.5, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

n   = 2000
X   = rgamma(n = n, shape = 8, scale = 4.5)
X2  = rgamma(n = n, shape = 5, scale = 5)
X3  = rgamma(n = n, shape = 3, scale = 3)
Z.s = -0.4 + 0.01*X + rnorm(n = n, mean = 0, sd = 1)
Z   = ifelse(Z.s>0, 1, 0)
M   = 3 + 2*Z + 0.05*X + rnorm(n = n, mean = 0, sd = 1)
Y   = 5 + 1*Z + 0.5*M + 0*Z*M + 0.05*X + 0.5*Z*M + rnorm(n = n, mean = 0, sd = 1)
data = data.frame(Y, M, Z, X, X2, X3)

result = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X", "X2", "X3"))
result[[1]]$NIE
result[[2]]$NIE

# Probit
covariate.models = c("x-gamma")
covariate.parameters = list(c(104, 8, 4.5))
true.exposure.coefs = c(I = -3.416096, X = 0.036231)
true.mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
true.outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

n   = 2000
X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
X2  = rgamma(n = n, shape = 5, scale = 5)
X3  = rgamma(n = n, shape = 3, scale = 3)
Z.s = -3.416096 + 0.036231*X + rnorm(n = n, mean = 0, sd = 1)
Z   = ifelse(Z.s>0, 1, 0)
M.s   = -1.6507546 + 0.2683970*Z + 0.0065543*X + rnorm(n = n, mean = 0, sd = 1)
Y.s   = -3.7220626 + 0.2763912*Z + 1.4729651*M - 0.2583784*Z*M + 0.0283196*X + 0.5*Z*M + rnorm(n = n, mean = 0, sd = 1)
data = data.frame(Y, M, Z, X, X2, X3)

result = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X", "X2", "X3"),med.model = "probit", out.model = "probit")
result[[1]]$NIE
result[[2]]$NIE
