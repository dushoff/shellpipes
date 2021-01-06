library(rpipes)
rpcall("plot.Rout dev/plot.R set.rda")

startGraphics()
commandEnvironments()

plot(1:x, (1:x)^2)

