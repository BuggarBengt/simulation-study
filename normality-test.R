exp.coefs = c(I = -3.5, X = 0.1)
med.coefs = c(I = -6, Z = 2, X = 0.1, ZX = 0)
out.coefs = c(I = -6, Z = 1, M = 3, ZM = 0, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

iter = 4000
n    = 3000
NDEs = vector("numeric", iter) 
NIEs = vector("numeric", iter) 
set.seed(12)
start_time = Sys.time()
for (i in 1:iter) {
  if (i%%10 == 0) {
    print(i/iter) 
  }
  X   = 1+rexp(n = n, rate = 0.05)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("Y~X+Z", data = data, family = binomial(link = "probit")) 
  out.model <- glm("Y~X+Z+M+Z*M", data = data, family = binomial(link = "probit")) 
  est = sensmediation(med.model=med.model, out.model=out.model, exp.name = "Z", med.name = "M")
  
  NDEs[i] <- est$NDE
  NIEs[i] <- est$NIE
}
end_time = Sys.time()
end_time - start_time
hist(NDEs, breaks = 20)
hist(NIEs, breaks = 20)


