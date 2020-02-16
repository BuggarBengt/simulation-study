# THERE IS SO MUCH DIFFERENCE IN THE ESTIMATED COEFFICIENTS WHEN N = 2000
# I WOULD SAY THAT, FOR THE LOW N IN THIS CASE, THE RANDOMNESS OF THE MODEL HAS A MUCH HIGHER IMPACT THAN MISSPECIFYING THE MODEL

# OPTIONS: RERUN PROBIT WITH OTHER DATA
true.NDE = calc.nde.probit(b = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0), t = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0), x = data[, "X"]) 
true.NIE = calc.nie.probit(b = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0), t = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0), x = data[, "X"])

data = generate.data(n = 2000,
                     covariate.models = c("x-gamma"),
                     covariate.parameters = list(c(104, 8, 4.5)),
                     exposure.coefs = c(I = -3.416096, X = 0.036231),
                     mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0),
                     outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0),
                     outcome.mediator.type = "probit",
                     sd.exposure = 1,
                     sd.mediator = 1,
                     sd.outcome = 1)

m.model <- glm("M~Z+X", data = data, family=binomial(link='probit')) 
y.model <- glm("Y~Z+M+X+Z*M", data = data, family=binomial(link='probit')) 

est <- sensmediation(med.model=m.model, out.model=y.model, Rho=0, progress=FALSE, exp.name = "Z", med.name = "M")
est$NDE
mean(true.NDE)
est$NIE
mean(true.NIE)