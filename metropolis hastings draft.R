
proposalfunction <- function(c, phi_old) {
  rbeta(1, c * phi_old, c * (1 - phi_old))
}



run_metropolis_MCMC <- function(Alpha, Beta, c, iterations){
  chain = array(dim = c(iterations+1,1))
  chain[1,] = runif(1)
  for (i in 1:iterations){
    proposal = proposalfunction(c, chain[i,])
    probab = dbeta(proposal, 6, 4)/dbeta(chain[i,], 6, 4) * (dbeta(chain[i, ], c*proposal, c*(1-proposal))/ dbeta(proposal, c*chain[i,], c*(1-chain[i,]))) 
    if (runif(1) < min(1,probab)){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

c<-c(1,2.5,5,10)
chain_new<-sapply(c, run_metropolis_MCMC, Alpha=6, Beta=4, iterations=10000)
par(mfrow = c(4, 3))
for( j in 1:4 ) {
  plot(chain_new[, j], type="l", main = paste("Trace plot: c = ", c[j], sep=""), xlab="Step", ylab="y")
  acf(chain_new[, j], main = paste("Acf plot: c = ", c[j], sep=""))
  hist(chain_new[, j], main = paste("Histogram: c = ", c[j], sep=""), xlab="y")
}


ks_test1<-ks.test(rbeta(10000,6,4),chain_new[,1])  

ks_test






c1<-run_metropolis_MCMC(6,4,1,10000)

par(mfrow=c(1,3))  #1 row, 3 columns
plot(c1, type="l", main = paste("Trace plot: c = ", c, sep=""), xlab="Step", ylab="y")
acf(c1, main = paste("Acf plot: c = ", c, sep=""))
hist(c1, main = paste("Histogram: c = ", c, sep=""), xlab="y")

install.packages('rattle')
data(wine, package="rattle")
wine<-wine[-1]
x_new<-c(wine)
wine<-c(wine)
help(data.frame)
wine
help(kmeans)


kmeans<-function(x, clusters, iterations){
  centers
  x_new<-c(x)
    norm1<-dist(rbind(x_new[i],centers[1], method = "euclidean"))
    norm2<-dist(rbind(x_new[i],centers[2], method = "euclidean"))
    norm3<-dist(rbind(x_new[i],centers[3], method = "euclidean"))
    if (norm1<norm2) {
      cluster1[i]<-x_new[i]
    } 
    if (norm2<norm3) {
      cluster2[i]<-x_new[i]
    } else{
      cluster3[i]<-x_new[i]
    }
    return(cluster1)
    return(cluster2)
    return(cluster3)
}

centers<-c(1,2,3)
kmeans(x<-wine,centers,10)
x_new[1]

sample(wine[,1], 1, replace=TRUE)
help(sample)
help(sapply)




