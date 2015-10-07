#Set up data from true distributions.
#Set poission with u0 = 3
#Set posssion with u1 = 11
#hidden variable from bernouli(0.25)
sample.mix <- NULL
real.group <- NULL
for(i in 1 : 1500){
  real.group[i] <- rbinom(1, 1, 0.25)
  if ( real.group[i] == 0){
    sample.mix[i]<- rpois(1, 3)
  } else{
    sample.mix[i] <- rpois(1, 11)
  }
}

#guess up initial parameters
u0.est <- 3
u1.est <- 6
pi.hidden.est <- 0.1

#Iteratively work thru E step and M step
#E step
Qi <- function(x, u0, u1, pi.hidden){
  pi0.hidden <- 1 - pi.hidden
  comp1 <- pi.hidden * dpois(x, u1)
  comp0 <- pi0.hidden * dpois(x, u0)
  return (comp1 / (comp0 + comp1))
}

#Calculte qik for each data point
for(i in 1 : 1000){
  qvec <- sapply(sample.mix, 
         function(x) Qi(x, u0 = u0.est, u1 = u1.est, pi.hidden = pi.hidden.est))
  
  qvec0 <- 1- qvec
#M step
  u0.est <- sum(qvec0 * sample.mix) / sum(qvec0)
  u1.est <- sum(qvec * sample.mix) / sum(qvec)
  pi.hidden.est <- sum(qvec) / length(sample.mix)

}

#print out result
print(u0.est)
print(u1.est)
print(pi.hidden.est)


#Calculate accuracy with threshold = 0.5.
sum(real.group == ifelse(qvec > 0.5, 1, 0))/ 1500




