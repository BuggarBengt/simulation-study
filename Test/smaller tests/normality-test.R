exp.coefs = c(I = -3.5, X = 0.1)
med.coefs = c(I = -6, Z = 2, X = 0.1, ZX = 0)
out.coefs = c(I = -6, Z = 1, M = 3, ZM = 0, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

iter = 3000
n    = 3000
NDEs = vector("numeric", iter) 
NIEs = vector("numeric", iter) 
set.seed(12)
start_time = Sys.time()
for (i in 1:iter) {
  if (i%%10 == 0) {
    print(i/iter) 
  }
  X   = 30+rexp(n = n, rate = 0.1)
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
shapiro.test(NIEs)
mean(NIEs)
sd(NIEs)
sort(NIEs)[1:150]
pnorm(0.1252474, mean(NIEs), sd(NIEs))
sum(Z)
sum(M)
sum(Y)


iter = 2000
n    = 2000
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
                                                  med.model.type = "probit", out.model.type =  "probit", R=2000)
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













n=1000
b1= 0.04
b2=-2.70289
t3=0.06

iter = 10000
xs = 
  f1=vector(mode = "numeric", length = iter)
f2=vector(mode = "numeric", length = iter)
dif=vector(mode = "numeric", length = iter)
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  xs[i] = mean(X)
  f1[i] = mean(b1*X+b2)
  f2[i] = mean(t3*X+b2)
  dif[i] = f1[i]- f2[i]
}
hist(xs)
hist(f1)
hist(f2)
hist(dif)
shapiro.test(dif[1:500])


n=1000
b1= 0.04
b2=-2.70289
t3=0.06

iter = 10000
xs = 
  f1=vector(mode = "numeric", length = iter)
f2=vector(mode = "numeric", length = iter)
dif=vector(mode = "numeric", length = iter)
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  xs[i] = mean(X)
  f1[i] = mean(pnorm(b1*X+b2))
  f2[i] = mean(pnorm(t3*X+b2))
  dif[i] = f1[i]- f2[i]
}
hist(xs)
hist(f1)
hist(f2)
hist(dif)
shapiro.test(f2[1:500])


