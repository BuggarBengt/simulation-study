# alpha test
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

iter = 1000
n    = 1000
p.center = vector(mode = "numeric", length = iter)
p.sample = vector(mode = "numeric", length = iter)
p.true = vector(mode = "numeric", length = iter)
mean.boot.diffs = vector(mode = "numeric", length = iter)
true.sample.diffs = vector(mode = "numeric", length = iter)
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
  par.boot.res = interaction.test.par.boot(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), R=1000)
  p.center[i] = par.boot.res[1]
  p.sample[i] = par.boot.res[2]
  p.true[i] = par.boot.res[3]
  mean.boot.diffs[i] = par.boot.res[4]
  true.sample.diffs[i] = par.boot.res[5]
  
  if (i%%10 == 0) {
    print(i/iter) 
    print(paste("time left: ", (iter-i)/i*(Sys.time()-start_time)))
  }
}
end_time = Sys.time()
end_time - start_time

par(mfrow=c(1, 2))
hist(p.center, breaks = 30)
hist(p.sample, breaks = 30)
hist(p.true, breaks = 30)
hist(mean.boot.diffs, breaks = 30)
hist(true.sample.diffs, breaks = 30)

save(p.center, file = "Data/Data-test-tests/probit.par.boot.n.1000.i.1000.R.1000.p.center2")
save(p.sample, file = "Data/Data-test-tests/probit.par.boot.n.1000.i.1000.R.1000.p.sample2")
save(mean.boot.diffs, file = "Data/Data-test-tests/probit.par.boot.n.1000.i.1000.R.1000.boot.diffs2")
save(true.sample.diffs, file = "Data/Data-test-tests/probit.par.boot.n.1000.i.1000.R.1000.sample.diffs2")


