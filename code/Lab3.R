#Read Library
library('foreach')
library('doParallel')
library('Rcpp')
library('microbenchmark')

#source c++ similarity function
sourceCpp('Lab3.cpp')

#Read data

load("../data/lingBinary.rdata")
ling.ana <- lingBinary[,-1:-5]
ling.ana$id <- 1 : nrow(ling.ana)
#Set up predefined argument##############################
k.max <- 2
n <- 100
#Register # of cores to use

ncores <- 8
registerDoParallel(ncores)
#m should be somewhere around 0.2 - 0.8

SubSample <- function(data, m) {
  # Subset the data with proportion m 
  #
  # Args:
  #   data: Data we want to subset
  #   m: proportion we want subset from data
  #
  # Returns:
  #   Return a dataframe with subsample.  
   n.row <- nrow(data)
   sample.index <- sample(1 : n.row, floor(n.row*m))
   rtn <- data[sample.index, ]
   return (rtn)
}

InterIndex <- function(sub1, sub2) {
  # Search the index of sub1 and sub2 which they share same data point respectively.
  #
  # Args:
  #   subl: Subset of data 
  #   sub2: Another subset of data
  #
  # Returns:
  #   Dataframe with index of sub1 and sub2 which point to same data point they share. 
  matched <- match(row.names(sub1), row.names(sub2))
  sub1.index <- which(!is.na(matched))
  sub2.index <- matched[sub1.index]
  rtn <- data.frame(sub1.index, sub2.index)
  return (rtn)
}

Similarity <- function(l1.inter, l2.inter, method ="matching") {
  # Computes the similarity between with two cluster result vectors.
  #
  # Args:
  #   l1.inter: index of subsample 1 which points to same data point shared with subsample 2
  #   l2.inter: index of subsample 2 which points to same data point shared with subsample 1
  #   method: methods used to calculate similarity. There are three available methods: matching
  #           Jaccard and cosine, default is matching method. 
  #
  # Returns:
  #   The similarity values of the result of cluster 1 and cluster 2
  d <- length(l1.inter)
  #Save booleans to save more space
  c1 <- matrix(FALSE,d,d)
  c2 <- matrix(FALSE,d,d)
  for(i in 1:d){
    c1[i, -i] <- (l1.inter[-i] == l1.inter[i])
    c2[i, -i] <- (l2.inter[-i] == l2.inter[i])
  }
  if (method == "matching") { 
    return (sum(c1 == c2)/d^2)
  }
  else if (method == "Jaccard") {
    return (sum(c1 + c2 == 2)/sum(c1+c2 > 0))
  }
  else if (method == "cosine") {
    return (sum(c1+c2 == 2)/ (sqrt(sum(c1)) * sqrt(sum(c2))))
  }
}


setwd("../code")
sourceCpp('Lab3.cpp')
StabCluter <- function(data, m, n, implement = "C++", method = "matching"){
  # Implement stable cluster algorithm suggested by Benhur 
  #
  # Args:
  #   data: data to be clusted.
  #   m: proportion we want subset from data.
  #   n: number of repetitions implementing subsetting.
  #   implement: the way implementing the similarity function,there are C++ and R,
  #              default is C++.
  #   method: methods used to calculate similarity. There are three available methods: matching
  #           Jaccard and cosine, default is matching method. 
  #      
  # Returns:
  #   The similarity matrix from cluster 2 to 10.
  sim.vec <- NULL
  if (implement == "R"){
    sim.mat <- foreach(num.cluster = 4 : 4, .combine = cbind) %dopar% {
    for(i in 1 : n){
      sub1 <- SubSample(data, m)
      sub2 <- SubSample(data, m)
      inter <- InterIndex(sub1, sub2)
      l1.inter <- kmeans(sub1, num.cluster)$cluster[row.names(sub1)[inter$sub1.index]]  
      l2.inter <- kmeans(sub2, num.cluster)$cluster[row.names(sub2)[inter$sub2.index]]
      sim.vec[i] <- Similarity(l1.inter, l2.inter, method = method)
    }
       return(sim.vec)
    }
  }else{
    sim.mat <- foreach(num.cluster = 4 : 4, .combine = cbind) %dopar% {
    for(i in 1 : n){
      print(i)
      sub1 <- SubSample(data, m)
      sub2 <- SubSample(data, m)
      inter <- InterIndex(sub1, sub2)
      l1.inter <- kmeans(sub1, num.cluster)$cluster[row.names(sub1)[inter$sub1.index]] 
      l2.inter <- kmeans(sub2, num.cluster)$cluster[row.names(sub2)[inter$sub2.index]]
      sim.vec[i] <- SimilarityC(l1.inter, l2.inter, method = method)
    }
     return(sim.vec)
  }
  }
  return(sim.mat)
}
 

#write output into CSV file
output.matching <- StabCluter(ling.ana, m = 0.8, n = 100, implement = "C++", method = "matching")
output.jaccard <- StabCluter(ling.ana, m = 0.2, n = 10, implement = "R", method = "Jaccard")
output.cosine <- StabCluter(ling.ana, m = 0.8, n= 100, implement = "C++", method = "cosine")


write.csv(output.matching, file = "matching_cl.csv", row.names = FALSE)
write.csv(output.jaccard, file = "jaccard_cl.csv", row.names = FALSE)
write.csv(output.cosine, file = "cosine_cl.csv", row.names = FALSE)

#Compare C++ and R for function similarity
x1 <- sample(1:10, 29000, replace = TRUE)
x2 <- sample(1:10, 29000, replace = TRUE)
microbenchmark(Similarity(x1, x2, "matching"), SimilarityC(x1, x2, "matching"))

#Compare C++ and R for whole algorithm with m = 0.35
start.time <- Sys.time()
output.matching <- StabCluter(ling.ana, m = 0.35, n = 100, implement = "C++", method = "matching")
duration <- Sys.time() - start.time


start.time <- Sys.time()
output.matching <- StabCluter(ling.ana, m = 0.35, n = 100, implement = "R", method = "matching")
duration1 <- Sys.time() - start.time

print(duration)
print(duration1)





