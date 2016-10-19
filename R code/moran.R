setwd(file.path("..", "Data"))



attach("corr.RData")
csa.dists <- as.matrix(csa.dists)
csa.dists.inv <- 1/csa.dists # not solve(csa.dists)
diag(csa.dists.inv) <- 0

#2014
result <- ape::Moran.I(mtdata$LifeExp14, csa.dists.inv)
print(result)

detach()
unlink("corr.RData")
source(file = file.path("..", "R code", "analyses_data.R"))

setwd(file.path(".."))

