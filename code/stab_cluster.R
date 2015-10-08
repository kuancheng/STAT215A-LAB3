#Read Library
library('foreach')
library('doParallel')
library('Rcpp')
library('microbenchmark')


#Read data and clean data
#dir ="./Binary.rdata"
load("../data/lingBinary.rdata")
ling.ana <- lingBinary[, -1:-6]
ling.ana$id <- 1:nrow(ling.ana)


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
  sample.index <- sample(1:n.row, floor(n.row*m), replace = FALSE)
  rtn <- data[sample.index, ]
  return (rtn)
}

Similarity <- function(l1.inter, l2.inter, method ="matching") {
  # Computes the similarity between with two cluster result vectors.
  #
  # Args:
  #   l1.inter: cluster vector of subsample 1 which are intersected with subsample 2
  #   l2.inter: cluster vector of subsample 2 which are intersected with subsample 1
  #   method: methods used to calculate similarity. There are three available methods: matching
  #           Jaccard and cosine, default is matching method. 
  #
  # Returns:
  #   The similarity values of the result of cluster 1 and cluster 2
  d <- length(l1.inter)
  #Save booleans to save more memory space
  c1 <- matrix(FALSE, d, d)
  c2 <- matrix(FALSE, d, d)
  
  #construct C matrix(in Behur 2001)
  for(i in 1:d){
    c1[i, -i] <- (l1.inter[-i] == l1.inter[i])
    c2[i, -i] <- (l2.inter[-i] == l2.inter[i])
  }
  if (method == "matching") { 
    return (sum(c1 == c2) / d^2)
  }
  else if (method == "Jaccard") {
    return (sum(c1 + c2 == 2) / sum(c1 + c2 > 0))
  }
  else if (method == "cosine") {
    return (sum(c1 + c2 == 2) / (sqrt(sum(c1)) * sqrt(sum(c2))))
  }
}




sourceCpp('Lab3.cpp')
StabCluter <- function(data, m, n, k.max, implement = "C++", method = "matching"){
  # Implement stable cluster algorithm suggested by Benhur 
  #
  # Args:
  #   data: data to be clusted.
  #   m: proportion we want subset from data.
  #   n: number of repetitions implementing subsetting.
  #   k.max: calculate similarty from number of cluster 2 to k.max 
  #   implement: the way implementing the similarity function,there are C++ and R,
  #              default is C++.
  #   method: methods used to calculate similarity. There are three available methods: matching
  #           Jaccard and cosine, default is matching method. 
  #      
  # Returns:
  #   The similarity matrix from cluster 2 to 10.
  
    #Register # of cores to use
    ncores <- 8
    registerDoParallel(ncores)
    #Assign which function to use
    if (implement == "R") {
      myfun <-  Similarity
    }else{
      myfun <- SimilarityC
    }
    
    sim.mat <- foreach(num.cluster = 2 : k.max, .combine = cbind) %dopar% {
      sim.vec <- NULL
      for(i in 1 : n){
          sub1 <- SubSample(data, m)
          sub2 <- SubSample(data, m)
          
          #Do k-means and save it as datafrmae
          result.1 <- data.frame(kmeans(sub1[ , -ncol(sub1)], num.cluster)$cluster)  
          result.2 <- data.frame(kmeans(sub2[ , -ncol(sub2)], num.cluster)$cluster)
          
          #combine cluster result
          sub1.clust <- cbind(sub1$id, result.1)
          names(sub1.clust)[1] <- "id"
          names(sub1.clust)[2] <- "cluster"
          sub2.clust <- data.frame(cbind(sub2$id, result.2))
          names(sub2.clust)[1] <- "id"
          names(sub2.clust)[2] <- "cluster"
         
          #extract intersect part 
          intersect <- merge(sub1.clust, sub2.clust, by=c("id"))
          l1.inter <- intersect$cluster.x
          l2.inter <- intersect$cluster.y
         
          sim.vec[i] <- myfun(l1.inter, l2.inter, method = method)
         
        }
        return(sim.vec)
      }
  return(sim.mat)
}


#Compare Time difference between R and C++
start.time <- Sys.time()
StabCluter(ling.ana, m = 0.35, n = 100, k.max = 10,  implement = "R", method = "Jaccard")
duration <- Sys.time() - start.time

start.time <- Sys.time()
output.jaccard <- StabCluter(ling.ana, m = 0.35, n = 100, k.max = 10, implement = "C++", method = "Jaccard")
duration1 <- Sys.time() - start.time

#produce output of csv for plot
output.matching <- StabCluter(ling.ana, m = 0.8, n = 100, k.max = 10, implement = "C++", method = "matching")
write.csv(output.matching, file = "matching_cl.csv", row.names = FALSE)

output.jaccard <- StabCluter(ling.ana, m = 0.8, n = 100, k.max = 10, implement = "C++", method = "Jaccard")
write.csv(output.jaccard, file = "jaccard_cl.csv", row.names = FALSE)

output.cosine <- StabCluter(ling.ana, m = 0.8, n= 100, k.max = 10, implement = "C++", method = "cosine")
write.csv(output.cosine, file = "cosine_cl.csv", row.names = FALSE)



