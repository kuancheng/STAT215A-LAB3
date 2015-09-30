#Use foreach for outer loop of algorithm

##Read Library
library('foreach')
library('doParallel')

##Read data
load("../data/lingBinary.rdata")
head(lingBinary)
lingBinary_ana <- lingBinary[,-1:-5]
##Set up predefined argument##############################
k_max <- 10
n <- 100
#Register # of cores to use

ncores <- 8
registerDoParallel(ncores)
#m should be somewhere around 0.2 - 0.8
start.time <- Sys.time()
foreach(num_cluster = 2:k_max) %dopar% {
 model <- kmeans(lingBinary_ana, num_cluster) 
}
duration <- Sys.time() - start.time

print(duration)

