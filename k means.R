x <- rbeta(runif(10000) , 6, 4)
plot(density(x))

####################

library("rattle")
data(wine, package="rattle")

#tip: use wine[-1] to exclude the "Type" variable

#INSTRUctions: Write a k-means classifier to cluster the wines 
#into 3 groups.  Use the Euclidean distance measure.


#}
data(wine)
wine<-wine[-1]
#Randomly assign 3 epicenters: create empty matrix:
epicenters <- matrix(0, nrow=3, ncol=13)
#Sample from each column to obtain an epicenter coordinate:
#i is "k" in k-means
for(i in 1:3) {
  for(j in 1:13) {
    col_sample <- sample(wine[,j], 1)
    epicenters[i,j] <- col_sample
  }
}

#consider checking that the sample falls within each column range

#calculate euclidean dist:
#for every observation to each of the three clusters: 

#initialize matrix for euclidean norms, 
#plus a column for cluster assignment at time t,
#plus a column for cluster assignment at time t+1:
all_norms <- matrix(, nrow=178, ncol=5)

for(i in 1:3) {
  for(j in 1:178) {
    e_dist <- dist(rbind(wine[j,], epicenters[i,]), method = "euclidean")
    #put in 178x3 matrix (rows are obs, cols are e dists per epicenter)
    all_norms[j, i] <- e_dist
  }
}
  all_norms
#take the min of each row; assign that col as the cluster; put it in fourth 
#col of all_norms
for(i in 1:178) {
  min_dist_index <- which.min(as.vector(all_norms[i,]))
  all_norms[i,4] <- min_dist_index
}  

#verify that each cluster has at least one point
if ((length(which(all_norms[,4]==1)) > 0)&
    (length(which(all_norms[,4]==2)) > 0)&
    (length(which(all_norms[,4]==3)) > 0) ) {
#Consider removing the print message
    print ("Each cluster has at least one point")
}
 
#return each cluster as a separate matrix?
  
#Recalculate the epicenters as the means per cluster
  #for this: (which(all_norms[,4]==1)) obtain the row numbers per 1,2,3
  #calculate the means per column using the original matrix
  #populate the epicenters matrix
  #populate all_norms columns 1-3
  #populate all_norms column 5
  #check whether cols 4,5 of all_norms are equal
    #if so, stop
    #if not, 
  all_norms[,4] <- all_norms[,5] 
  #break (all_norms[,4] <- all_norms[,5])



