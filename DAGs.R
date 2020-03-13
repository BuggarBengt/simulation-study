devtools::install_github("malcolmbarrett/ggdag") #installera senaste versionen/blir inte samma via install.packages()
library(ggdag)
library(dagitty)
library(manipulate)
#Anv?nt det h?r R-paketet som bygger p? dagitty-objekt (fr?n R-paketet dagitty) men plottar med ggplot2
#Tycker det ser snyggare ut och verkar vara anv?nt r?tt mycket
#https://ggdag.netlify.com

dag <- dagitty( "dag { A -> B -> C } " )
coords <- list(
  x = c(A = 0, B = 1, C = 2),
  y = c(A = 1, B = 1, C = 1)
)
coord_df <- coords2df(coords)
coordinates(dag) <- coords2list(coord_df)
ggdag(dag, use_labels = "label")
ggdag(dag)
ggsave("pres.png")

dag <- dagitty( "dag { Z -> Y 
                Z -> M -> Y
                Z <- X -> M
                X -> Y} " )
coords <- list(
  x = c(Z = 0, M = 1, Y = 2, X = 1),
  y = c(Z = 1, M = 1.4, Y = 1, X = 2)
)
coord_df <- coords2df(coords)
coordinates(dag) <- coords2list(coord_df)
ggdag(dag)
ggdag_adjust(dag, c("X"))
ggsave("simulated-scenario.png")

dag <- dagitty( "dag { T -> W <- Y } " )
coords <- list(
  x = c(T = 0, Y = 2, W = 1),
  y = c(T = 1, Y = 1, W = 0)
)
coord_df <- coords2df(coords)
coordinates(dag) <- coords2list(coord_df)
ggdag(dag)
ggsave("DAG-example.png")
ggdag_adjust(dag, "W")
ggsave("selection-bias2.png")

ggdag(m_bias(x="t"))
ggdag_adjust(m_bias(), "m")
m_bias(x = "SSRI", y = "Lung cancer", a = "Depression", 
       b = "Smoker", m = "CAD") %>% 
  ggdag_adjust("m", use_labels = "label") + 
  annotate(geom="text", x=-0, y=0.5, label=paste("B[a-x]"), color="Black", parse=TRUE) + 
  annotate(geom="text", x=0.48, y=0.73, label=paste("B[a-m]"), color="Black", parse=TRUE) + 
  annotate(geom="text", x=1.52, y=0.73, label=paste("B[b-m]"), color="Black", parse=TRUE) + 
  annotate(geom="text", x=2, y=0.5, label=paste("B[b-y]"), color="Black", parse=TRUE)
#Blir fula cirklar om man k?r Windows d? windows inte anv?nder anti-aliasing i R 
#Det fixar sig sen om man sparar med ggsave
#https://www.youtube.com/watch?v=hqi0114mwtY
ggsave("test.png")


manipulate(
  m_bias() %>% 
    ggdag_adjust("m") + 
    annotate(geom="text", x=0.5, y=1.3, label=paste(expression("p(x) == frac(e^(B0[x]+B1[x]%*%a), 1+e^(B0[x]+B1[x]%*%a))")), color="Black", parse=TRUE, size=5) +
    annotate(geom="text", x=1.5, y=1.3, label=paste(expression("p(y) == frac(e^(B0[y]+B1[y]%*%b), 1+e^(B0[y]+B1[y]%*%b))")), color="Black", parse=TRUE, size=5) +
    annotate(geom="text", x=0.5, y=1.1, label=paste(expression("p(a) == frac(e^(B0[a]), 1+e^(B0[a]))")), color="Black", parse=TRUE, size=5) +
    annotate(geom="text", x=1.5, y=1.1, label=paste(expression("p(b) == frac(e^(B0[b]), 1+e^(B0[b]))")), color="Black", parse=TRUE, size=5) +
    annotate(geom="text", x=1, y=0.9, label=paste(expression("p(m) == frac(e^(B0[m]+B1[m]%*%a+B2[m]%*%b), 1+e^(B0[m]+B1[m]%*%a+B2[m]%*%b))")), color="Black", parse=TRUE, size=5) +
    annotate(geom="text", x=1, y=0.05, label=paste("Estimated bias = ", getBiasFromParameters(B0x, B0y, B0a, B0b, B0m, B1x, B1y, B1m, B2m, condition)), color="Black", size=5),
  condition = checkbox(TRUE, "condition on m=1"),
  B0x=slider(-5,5, step = 0.1, initial = -2, label="B0x"),
  B0y=slider(-5,5, step = 0.1, initial = -2, label="B0y"),
  B0a=slider(-5,5, step = 0.1, initial = -2, label="B0a"),
  B0b=slider(-5,5, step = 0.1, initial = -2, label="B0b"),
  B0m=slider(-5,5, step = 0.1, initial = -2, label="B0m"),
  B1x=slider(-5,5, step = 0.1, initial = 4, label="B1x"),
  B1y=slider(-5,5, step = 0.1, initial = 4, label="B1y"),
  B1m=slider(-5,5, step = 0.1, initial = 4, label="B1m"),
  B2m=slider(-5,5, step = 0.1, initial = 4, label="B2m"))









