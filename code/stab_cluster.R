#Read Library
library(foreach)
library(doParallel)
library(Rcpp)
library(microbenchmark)
library(ggplot2)
library(reshape2)


#Read data and clean data
dir <- "~/Desktop/STAT215A/Lab3/STAT215A-LAB3/data/lingBinary.rdata"
load(dir)

ling.ana <- lingBinary[, -1:-6]
#change row.names of ling.ana to make it consistent with index
row.names(ling.ana) <- 1:nrow(ling.ana)


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
  sample.index <- sample(1:n.row, floor(n.row * m), replace = FALSE)
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



#source c++ code and make sure get same result as R
dir <- "~/Desktop/STAT215A/Lab3/STAT215A-LAB3/data/"
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
      myfun <- Similarity
    }else{
      myfun <- SimilarityC
    }
    
    sim.mat <- foreach(num.cluster = 2 : k.max, .combine = cbind) %dopar% {
      sim.vec <- NULL
      for(i in 1 : n){
          #We wish avoid save matrix here as much as possilbe
          #Since matrix easily run out of memory
          #also need more computation to implement
          sub1 <- SubSample(data, m)
          sub2 <- SubSample(data, m)
          inter <- InterIndex(sub1, sub2)
          
          #Get cluster vectors of intersect point
          l1.inter <- kmeans(sub1, num.cluster)$cluster[row.names(sub1)[inter$sub1.index]]  
          l2.inter <- kmeans(sub2, num.cluster)$cluster[row.names(sub2)[inter$sub2.index]]
          sim.vec[i] <- myfun(l1.inter, l2.inter, method = method)
         
        }
        return(sim.vec)
      }
  return(sim.mat)
}


#Compare Time difference between R and C++

# 1. record time eclipse for Similarity for each version of function.
duration.r <- NULL
duration.c <- NULL
for(i in  1:150){
  print(i)
  x1 <- sample(1:10, 100 * i, replace = TRUE)
  x2 <- sample(1:10, 100 * i, replace = TRUE)
  
  start.time <- Sys.time()
  Similarity(x1, x2, "matching")
  duration.r[i] <- as.numeric(Sys.time() - start.time, units = 'mins')
  
  start.time <- Sys.time()
  SimilarityC(x1, x2, "matching")
  duration.c[i] <- as.numeric(Sys.time() - start.time, units = 'mins')
}

#change unit to mins
time.data <- data.frame(k = 1:150 * 100, duration.r, duration.c)

melt.time <- melt(time.data, id="k")
names(melt.time)[2:3] <- c("language_type", "time_mins")

ggplot(data = melt.time, aes(x = k, y = time_mins, colour = language_type)) +
  geom_line()


 

#2. Compare C++ and R for whole algorithm with m = 0.35, k is around 5511
start.time <- Sys.time()
output.matching <- StabCluter(ling.ana, m = 0.35, n = 100, k.max = 10, implement = "C++", method = "matching")
duration.c <- Sys.time() - start.time


start.time <- Sys.time()
output.matching <- StabCluter(ling.ana, m = 0.35, n = 100, k.max = 10, implement = "R", method = "matching")
duration.r <- Sys.time() - start.time

print(duration.c)
print(duration.r)






#produce output of csv for plot
output.matching <- StabCluter(ling.ana, m = 0.8, n = 100, k.max = 10, implement = "C++", method = "matching")
write.csv(output.matching, file = "matching_cl_new.csv", row.names = FALSE)

output.jaccard <- StabCluter(ling.ana, m = 0.8, n = 100, k.max = 10, implement = "C++", method = "Jaccard")
write.csv(output.matching, file = "jaccard_cl_new.csv", row.names = FALSE)


output.cosine <- StabCluter(ling.ana, m = 0.8, n= 100, k.max = 10, implement = "C++", method = "cosine")
write.csv(output.cosine, file = "cosine_cl_new.csv", row.names = FALSE)

