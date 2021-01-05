library(rpipes)

rpcall("plot.Rout dev/plot.R met.rda")

startGraphics()
commandEnvironments()

plot(1:x, (1:x)^2)

