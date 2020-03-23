# alpha test
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

exp.coefs = c(I = -4, X = 0.04)
med.coefs = c(I = -0.5, Z = 0.5, X = 0.01, ZX = 0)
out.coefs = c(I = -2, Z = 1, M = 1, ZM = 0, X = 0.02, ZX = 0, MX = 0, ZMX = 0)

iter = 4000
n    = 500

test.NDE.bca.90 = vector(mode = "numeric", length = iter)
test.NIE.bca.90= vector(mode = "numeric", length = iter)
test.NDE.bca.95= vector(mode = "numeric", length = iter)
test.NIE.bca.95= vector(mode = "numeric", length = iter)
test.NDE.bca.99= vector(mode = "numeric", length = iter)
test.NIE.bca.99= vector(mode = "numeric", length = iter)
test.NIE.bc.95= vector(mode = "numeric", length = iter)
test.NDE.bc.95= vector(mode = "numeric", length = iter)
nde.bca = vector(mode = "numeric", length = iter)
nie.bca = vector(mode = "numeric", length = iter)
p.values.nde = vector(mode = "numeric", length = iter)
p.values.nie = vector(mode = "numeric", length = iter)
mean.boot.nde.diffs = vector(mode = "numeric", length = iter)
mean.boot.nie.diffs = vector(mode = "numeric", length = iter)
boot.nde.diff.SE = vector(mode = "numeric", length = iter)
boot.nie.diff.SE = vector(mode = "numeric", length = iter)

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
  par.boot.res = interaction.test.diffs(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), R=1000)
  nde.bca[i] = par.boot.res[1, 4]
  nie.bca[i] = par.boot.res[2, 4]
  p.values.nde[i] = par.boot.res[1, 3]
  p.values.nie[i] = par.boot.res[2, 3]
  mean.boot.nde.diffs[i] = par.boot.res[1, 1]
  mean.boot.nie.diffs[i] = par.boot.res[2, 1]
  boot.nde.diff.SE[i] = par.boot.res[1, 2]
  boot.nie.diff.SE[i] = par.boot.res[2, 2]
  test.NDE.bca.90[i] = par.boot.res[1, 4]
  test.NIE.bca.90[i] = par.boot.res[2, 4]
  test.NDE.bca.95[i] = par.boot.res[1, 5]
  test.NIE.bca.95[i] = par.boot.res[2, 5]
  test.NDE.bca.99[i] = par.boot.res[1, 6]
  test.NIE.bca.99[i] = par.boot.res[2, 6]
  test.NIE.bc.95[i] = par.boot.res[1, 7]
  test.NDE.bc.95[i] = par.boot.res[2, 7]
  
  if (i%%10 == 0) {
    print(i/iter) 
    print(paste("time left: ", (iter-i)/i*(Sys.time()-start_time)))
  }
}
end_time = Sys.time()
end_time - start_time

par(mfrow=c(1, 2))
sum(nde.bca==0)/length(nde.bca)
sum(nie.bca==0)/length(nie.bca)
sum(p.values.nde[!is.na(p.values.nde)]<0.06)/length(p.values.nde[!is.na(p.values.nde)])
sum(p.values.nie[!is.na(p.values.nie)]<0.05)/length(p.values.nde[!is.na(p.values.nie)])
hist(p.values.nde, breaks = 20)
hist(p.values.nie, breaks = 20)
hist(mean.boot.nde.diffs, breaks = 30)
hist(mean.boot.nie.diffs, breaks = 30)
hist(boot.nde.diff.SE, breaks = 30)
hist(boot.nie.diff.SE, breaks = 30)

length(p.values.nde[p.values.nde>0.1])/length(p.values.nde)
length(p.values.nde[p.values.nde>0.05])/length(p.values.nde)
length(p.values.nde[p.values.nde>0.01])/length(p.values.nde)

length(p.values.nie[p.values.nie>0.1])/length(p.values.nie)
length(p.values.nie[p.values.nie>0.05])/length(p.values.nie)
length(p.values.nie[p.values.nie>0.01])/length(p.values.nie)

sum(test.NDE.bca.90)/length(test.NDE.bca.90)
sum(test.NDE.bca.95)/length(test.NDE.bca.95)
sum(test.NDE.bca.99)/length(test.NDE.bca.99)

sum(test.NIE.bca.90)/length(test.NIE.bca.90)
sum(test.NIE.bca.95)/length(test.NIE.bca.95)
sum(test.NIE.bca.99)/length(test.NIE.bca.99)


# S1 is real scenario
save(p.values.nie, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.3000.R.1000.p.nie.s1")
save(p.values.nde, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.3000.R.1000.p.nde.s1")
save(mean.boot.nie.diffs, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.3000.R.1000.mean.nie.s1")
save(mean.boot.nde.diffs, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.3000.R.1000.mean.nde.s1")
save(boot.nie.diff.SE, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.3000.R.1000.SE.nie.s1")
save(boot.nde.diff.SE, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.3000.R.1000.SE.nde.s1")

save(nde.bca, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.2000.R.1000.SE.nie.bca.s1")
save(nde.bca, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.2000.R.1000.SE.nde.bca.s1")
save(p.values.nie, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.2000.R.1000.p.nie.s1")
save(p.values.nde, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.2000.R.1000.p.nde.s1")
save(mean.boot.nie.diffs, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.2000.R.1000.mean.nie.s1")
save(mean.boot.nde.diffs, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.2000.R.1000.mean.nde.s1")
save(boot.nie.diff.SE, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.2000.R.1000.SE.nie.s1")
save(boot.nde.diff.SE, file = "Data/Data-secondtest-tests/probit.secondtest.n.500.i.2000.R.1000.SE.nde.s1")

save(p.values.nie, file = "Data/Data-secondtest-tests/probit.secondtest.n.1000.i.1000.R.1000.p.nie.s2")
save(p.values.nde, file = "Data/Data-secondtest-tests/probit.secondtest.n.1000.i.1000.R.1000.p.nde.s2")
save(mean.boot.nie.diffs, file = "Data/Data-secondtest-tests/probit.secondtest.n.1000.i.1000.R.1000.mean.nie.s2")
save(mean.boot.nde.diffs, file = "Data/Data-secondtest-tests/probit.secondtest.n.1000.i.1000.R.1000.mean.nde.s2")
save(boot.nie.diff.SE, file = "Data/Data-secondtest-tests/probit.secondtest.n.1000.i.1000.R.1000.SE.nie.s2")
save(boot.nde.diff.SE, file = "Data/Data-secondtest-tests/probit.secondtest.n.1000.i.1000.R.1000.SE.nde.s2")

