# Linear
exposure.coefs =  c(I = -0.4, X = 0.01)
mediator.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0)
outcome.coefs = c(I = 5, Z = 1, M = 0.5, ZM = 0.5, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

n   = 200
X   = rgamma(n = n, shape = 8, scale = 4.5)
X2  = rgamma(n = n, shape = 5, scale = 5)
X3  = rgamma(n = n, shape = 3, scale = 3)
Z.s = -0.4 + 0.01*X + rnorm(n = n, mean = 0, sd = 1)
Z   = ifelse(Z.s>0, 1, 0)
M   = 3 + 2*Z + 0.05*X + rnorm(n = n, mean = 0, sd = 1)
Y   = 5 + 1*Z + 0.5*M + 0*Z*M + 0.05*X + 0.5*Z*M + rnorm(n = n, mean = 0, sd = 1)
data = data.frame(Y, M, Z, X, X2, X3)

result.p.value = interaction.test.out.p.value(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X", "X2", "X3"))
result.p.value

result = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X", "X2", "X3"))
result = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"))
result[[1]]$NIE
result[[2]]$NIE

# Alpha test linear
# No interaction reality
exp.coefs =  c(I = -0.4, X = 0.01)
med.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0)
out.coefs = c(I = 5, Z = 1, M = 0.5, ZM = 0, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

iter = 10000
n    = 2000
p.values.p.test   = vector(mode = "numeric", length = iter)
p.values.def.test = vector(mode = "numeric", length = iter)
diff.def.test = vector(mode = "numeric", length = iter)
diff.NDE.test = vector(mode = "numeric", length = iter)
diff.NIE.test = vector(mode = "numeric", length = iter)
set.seed(12)
start_time = Sys.time()
for (i in 1:iter) {
  if (i%%10 == 0) {
    print(i/iter) 
  }
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  Y = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  data = data.frame(Y, M, Z, X, X2, X3)
  
  p.values.p.test[i] = interaction.test.out.p.value(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), out.model = "gaussian")
  def.test = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X") ,med.model = "gaussian", out.model = "gaussian")
  diff.def.test[i] = def.test[1, 1]
  p.values.def.test[i] = def.test[1, 3]
  
  est.comp = interaction.test.est.comp(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X") ,med.model = "gaussian", out.model = "gaussian")
  diff.NDE.test[i] = est.comp[1]
  diff.NIE.test[i] = est.comp[2]
}
end_time = Sys.time()
end_time - start_time
hist(p.values.p.test)
hist(p.values.def.test)
plot(density(diff.def.test))
plot(density(diff.NDE.test))
plot(density(diff.NIE.test))
shapiro.test(diff.def.test[1:5000])
shapiro.test(diff.NDE.test[1:5000])
shapiro.test(diff.NIE.test[1:5000])

load(file = "Data/Data-test-tests/linear.p.values.p.n.20000.i.10000")
load(file = "Data/Data-test-tests/linear.p.values.diff.n.20000.i.10000")
load(file = "Data/Data-test-tests/linear.diff.n.20000.i.10000")
load(file = "Data/Data-test-tests/linear.nde.est.comp.n.20000.i.10000")
load(file = "Data/Data-test-tests/linear.nie.est.comp.n.20000.i.10000")

save(p.values.p.test, file = "Data/Data-test-tests/linear.p.values.p.n.20000.i.10000")
save(p.values.def.test, file = "Data/Data-test-tests/linear.p.values.diff.n.20000.i.10000")
save(diff.def.test, file = "Data/Data-test-tests/linear.diff.n.20000.i.10000")
save(diff.NDE.test, file = "Data/Data-test-tests/linear.nde.est.comp.n.20000.i.10000")
save(diff.NIE.test, file = "Data/Data-test-tests/linear.nie.est.comp.n.20000.i.10000")

# Power test linear
# Interaction reality
exp.coefs =  c(I = -0.4, X = 0.01)
med.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0)
out.coefs = c(I = 5, Z = 1, M = 0.5, ZM = 0, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

interaction.coefs = seq(0, 0.5, 0.05) # true interaction coefs to get p-values for
sign.level = 0.05
austin.powers  = vector("numeric", length(interaction.coefs))
p.value.powers = vector("numeric", length(interaction.coefs))

iter = 100000
n    = 200
set.seed(12)
start_time = Sys.time()
for (c in 1:length(interaction.coefs)) {
  p.values.p.test   = vector(mode = "numeric", length = iter)
  p.values.def.test = vector(mode = "numeric", length = iter)
  for (i in 1:iter) {
    X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
    Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
    Z   = ifelse(Z.s>0, 1, 0)
    M = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
    Y = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + interaction.coefs[c]*Z*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
    data = data.frame(Y, M, Z, X)
    
    p.values.p.test[i] = interaction.test.out.p.value(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), out.model = "gaussian")
    def.test = interaction.test.multi.def(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X") ,med.model = "gaussian", out.model = "gaussian")
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

save(austin.powers, file = "Data/Data-test-tests/linear.power.diff.n.200.i.100000")
save(p.value.powers, file = "Data/Data-test-tests/linear.power.p.value.n.200.i.100000")


