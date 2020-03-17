n=500 # För stort n får vi normality för NIE och även för differensen.
iter = 10000

exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = 0, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

NDE=vector(mode = "numeric", length = iter)
NIE=vector(mode = "numeric", length = iter)
NDE2=vector(mode = "numeric", length = iter)
NIE2=vector(mode = "numeric", length = iter)
difnde =vector(mode = "numeric", length = iter)
difnie =vector(mode = "numeric", length = iter)
exp.value = 1
control.value = 0
set.seed(1234)
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  x.med = X
  x.out = X
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("M~Z+X", data = data, family = binomial(link = "probit")) 
  out.model <- glm("Y~Z+M+M*Z+X", data = data, family = binomial(link = "probit")) 
  
  b0 <- coef(med.model)[1]
  b1 <- coef(med.model)[2]
  b2 <- matrix(data = coef(med.model)[3], ncol = 1, nrow = 1)
  b3 <- matrix(data = 0, ncol = 1, nrow = 1)
  th0 <- coef(out.model)[1]
  th1 <- coef(out.model)[2]
  th2 <- coef(out.model)[3]
  th3 <- tail(coef(out.model), n=1)
  # th3 = 0
  x.indices = !coef(out.model) %in% c(th0, th1, th2, th3) 
  th4 <- matrix(coef(out.model)[x.indices], ncol = 1)
  th5 <- matrix(data = 0, ncol = 1, nrow = 1)
  th6 <- matrix(data = 0, ncol = 1, nrow = 1)
  th7 <- matrix(data = 0, ncol = 1, nrow = 1)
  
  t.de <- 0
  t.ie <- 1
  probs.med.ie <- stats::pnorm(b0 + b1 * exp.value + x.med %*% 
                                 (b2 + b3 * exp.value)) - stats::pnorm(b0 + b1 * 
                                                                         control.value + x.med %*% (b2 + b3 * control.value))
  probs.med.de <- stats::pnorm(b0 + b1 * t.de + x.med %*% 
                                 (b2 + b3 * t.de))
  probs.out.ie <- stats::pnorm(th0 + th2 + (th1 + th3) * 
                                 t.ie + x.out %*% (th4 + th5 * t.ie + th6 + th7 * t.ie)) - 
    stats::pnorm(th0 + th1 * t.ie + x.out %*% 
                   (th4 + th5 * t.ie))
  probs.out.de1 <- stats::pnorm(th0 + th1 * exp.value + 
                                  x.out %*% (th4 + exp.value * th5)) - stats::pnorm(th0 + 
                                                                                      th1 * control.value + x.out %*% (th4 + control.value * th5))
  probs.out.de2 <- stats::pnorm(th0 + th2 + exp.value * 
                                  (th1 + th3) + x.out %*% (th4 + exp.value * th5 + 
                                                             th6 + exp.value * th7)) - stats::pnorm(th0 + th2 + 
                                                                                                      control.value * (th1 + th3) + x.out %*% (th4 + control.value * 
                                                                                                                                                 th5 + th6 + control.value * th7))
  NIE[i] <- mean(probs.out.ie * probs.med.ie)
  NDE[i] <- mean(probs.out.de1 * (1 - probs.med.de) + 
                   probs.out.de2 * probs.med.de)
  
  t.de <- 1
  t.ie <- 0
  probs.med.ie <- stats::pnorm(b0 + b1 * exp.value + x.med %*% 
                                 (b2 + b3 * exp.value)) - stats::pnorm(b0 + b1 * 
                                                                         control.value + x.med %*% (b2 + b3 * control.value))
  probs.med.de <- stats::pnorm(b0 + b1 * t.de + x.med %*% 
                                 (b2 + b3 * t.de))
  probs.out.ie <- stats::pnorm(th0 + th2 + (th1 + th3) * 
                                 t.ie + x.out %*% (th4 + th5 * t.ie + th6 + th7 * t.ie)) - 
    stats::pnorm(th0 + th1 * t.ie + x.out %*% 
                   (th4 + th5 * t.ie))
  probs.out.de1 <- stats::pnorm(th0 + th1 * exp.value + 
                                  x.out %*% (th4 + exp.value * th5)) - stats::pnorm(th0 + 
                                                                                      th1 * control.value + x.out %*% (th4 + control.value * th5))
  probs.out.de2 <- stats::pnorm(th0 + th2 + exp.value * 
                                  (th1 + th3) + x.out %*% (th4 + exp.value * th5 + 
                                                             th6 + exp.value * th7)) - stats::pnorm(th0 + th2 + 
                                                                                                      control.value * (th1 + th3) + x.out %*% (th4 + control.value * 
                                                                                                                                                 th5 + th6 + control.value * th7))
  NIE2[i] <- mean(probs.out.ie * probs.med.ie)
  NDE2[i] <- mean(probs.out.de1 * (1 - probs.med.de) + 
                    probs.out.de2 * probs.med.de)
  
  difnde[i] = NDE[i]-NDE2[i]
  difnie[i] = NIE[i]-NIE2[i]
  
  if (i%%10 == 0) {
    print(i/iter) 
  }
}

par(mfrow=c(2, 3))
hist(NDE, breaks = 30)
hist(NIE, breaks = 30)
hist(NDE2, breaks = 30)
hist(NIE2, breaks = 30)
hist(difnde, breaks = 30)
hist(difnie, breaks = 30)
shapiro.test(difnde[1:500]) # NIE not AN?










