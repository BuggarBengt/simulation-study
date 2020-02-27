library(sensmediation)
#KLART SOM FAN DE KORRELERAR, OM VI HAR ETT STICKPROV DÄR NIE ÄR HÖG SÅ ÄR DEN HÖG FÖR BÅDE DEF.

# Probit correlations
true.exposure.coefs = c(I = -3.416096, X = 0.036231)
true.mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
true.outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

iterations = 100
n   = 200
NDEs.def1 = vector("numeric", length = iterations)
NDEs.def2 = vector("numeric", length = iterations)
NIEs.def1 = vector("numeric", length = iterations)
NIEs.def2 = vector("numeric", length = iterations)

for (i in 1:iterations) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = -3.416096 + 0.036231*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s   = -1.6507546 + 0.2683970*Z + 0.0065543*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s   = -3.7220626 + 0.2763912*Z + 1.4729651*M + 0*Z*M + 0.0283196*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("M~Z+X", data = data, family = binomial(link = "probit")) 
  out.model <- glm("Y~Z+M+X+Z*M", data = data, family = binomial(link = "probit"))   
  est1 <- sensmediation(med.model=med.model, out.model=out.model, exp.name = "Z", med.name = "M")
  est2 <- sensmediation(med.model=med.model, out.model=out.model, exp.name = "Z", med.name = "M", alt.decomposition = T)
  
  NDEs.def1[i] = est1$NDE
  NDEs.def2[i] = est2$NDE
  NIEs.def1[i] = est1$NIE
  NIEs.def2[i] = est2$NIE
  if (i%%10 == 0) {
    print(i/iterations) 
  }
}

plot(NDEs.def1, NDEs.def2)
plot(NIEs.def1, NIEs.def2)
cor(NIEs.def1,NIEs.def2)

library(sensmediation)

# linear correlations
exp.coefs =  c(I = -0.4, X = 0.01)
med.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0)
out.coefs = c(I = 5, Z = 1, M = 0.5, ZM = 0.5, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

iterations = 100
n   = 2000
NDEs.def1.linear = vector("numeric", length = iterations)
NDEs.def2.linear = vector("numeric", length = iterations)
NIEs.def1.linear = vector("numeric", length = iterations)
NIEs.def2.linear = vector("numeric", length = iterations)

for (i in 1:iterations) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  Y = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("M~Z+X", data = data) 
  out.model <- glm("Y~Z+M+X+Z*M", data = data)
  est1 <- sensmediation(med.model=med.model, out.model=out.model, exp.name = "Z", med.name = "M")
  est2 <- sensmediation(med.model=med.model, out.model=out.model, exp.name = "Z", med.name = "M", alt.decomposition = T)
  
  NDEs.def1.linear[i] = est1$NDE
  NDEs.def2.linear[i] = est2$NDE
  NIEs.def1.linear[i] = est1$NIE
  NIEs.def2.linear[i] = est2$NIE
  if (i%%10 == 0) {
    print(i/iterations) 
  }
}

plot(NDEs.def1.linear, NDEs.def2.linear)
plot(NIEs.def1.linear, NIEs.def2.linear)
cor(NIEs.def1.linear, NIEs.def2.linear)
cor.test(x = NDEs.def1.linear, y = NDEs.def2.linear, method = "pearson")















