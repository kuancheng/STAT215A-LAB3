library(foreach)
library(parallel)
library(iterators)
library(doParallel)

####starts here
setwd("D:/Courses/UC Berkeley/Fall 2015/Statistical Models Theory and Application/Lab/Lab3")

load("lingBinary.RData")



#set up cores
ncores <- 2
registerDoParallel(ncores)


get_stable<-function(data,k,m,seednum = NULL){##k is number of clusters, m is sample proportion
  
  if(!is.null(seednum))
  {set.seed(seednum)}
  else {set.seed(12345)}

  size<-m*nrow(data)
  data$id<-1:nrow(data)
  
  ##subsample
  sample1_id<-sample(1:nrow(data), size, replace = FALSE)
  sample2_id<-sample(1:nrow(data), size, replace = FALSE)
  
  sub_1<-data[sample1_id, ]
  sub_2<-data[sample2_id, ]
  
  ##k-means
  result_1<-data.frame(kmeans(sub_1[c(1:(ncol(data)-1))], centers = k,iter.max = 100000)$cluster) ##try to alleviate the impact of random starting value
  result_2<-data.frame(kmeans(sub_2[c(1:(ncol(data)-1))], centers = k,iter.max = 100000)$cluster)
  
  ##cluster result
  sub_1_clust<-data.frame(cbind(sub_1$id,result_1))
  names(sub_1_clust)[1] <- "id"
  names(sub_1_clust)[2] <- "cluster"
  sub_2_clust<-data.frame(cbind(sub_2$id,result_2))
  names(sub_2_clust)[1] <- "id"
  names(sub_2_clust)[2] <- "cluster"
  
  ##find intersected obs and its clustering membership, ensure 1-1 match
  intersect<-merge(sub_1_clust,sub_2_clust,by=c("id"))
  clust_1<-as.integer(as.character(intersect$cluster.x))
  clust_2<-as.integer(as.character(intersect$cluster.y))
    
  ##construct C_ij
  inter.dim <- dim(intersect)[1]
  C_1 <- matrix(clust_1, nr = inter.dim, nc = inter.dim) == matrix(clust_1, nr = inter.dim, nc = inter.dim, byrow = TRUE)
  C_2 <- matrix(clust_2, nr = inter.dim, nc = inter.dim) == matrix(clust_2, nr = inter.dim, nc = inter.dim, byrow = TRUE)
  diag(C_1) <- 0
  diag(C_2) <- 0
  
  ##compute similarity measure
  jaccard <- sum(C_1 * C_2)/(sum(C_1) + sum(C_2) - sum(C_1 * C_2))
  matching<- (sum(C_1 * C_2)+sum((1-C_1) * (1-C_2)))/(sum(C_1 * C_2)+sum((1-C_1) * (1-C_2))+sum((1-C_1)*C_2)+sum((1-C_2)*C_1))
  corr<-sum(C_1 * C_2)/sqrt(sum(C_1)*sum(C_2))
  
  return(c(jaccard,matching,corr))
}

###set function parameter
data<-lingBinary[c(7:474)]
m=0.8
k_max<-10
n<-100 #number of subsample comparisons

###record runtime

start.time <- Sys.time()

parallel.results <- foreach(i = 2 : k_max) %dopar% {
  
  stable_result<-vector("list",n)
  for (i in 1:n){
    stable_result[[i]]<-get_stable(data,k=k_max,m)
  }
  return(stable_result)
}
duration <- Sys.time() - start.time
