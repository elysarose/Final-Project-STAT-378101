
library("rattle")
data(wine, package="rattle")

#Remove column 1 from the analysis
wine <- wine[-1]

#####INITIAL PART (1 of 2): Random sampling, and cluster placement based on 
#the random sampling:
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

#Verified that the sample falls within each column range

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
#Verified that this calculation is correct

#take the min of each row; assign that col as the cluster; put it in fourth 
#col of all_norms
for(i in 1:178) {
  min_dist_index <- which.min(as.vector(all_norms[i, c(1,2,3)]))
  all_norms[i,4] <- min_dist_index
}  

#verify that each cluster has at least one point
if ((length(which(all_norms[,4]==1)) > 0)&
    (length(which(all_norms[,4]==2)) > 0)&
    (length(which(all_norms[,4]==3)) > 0) ) {
#Consider removing the print message
    print ("Each cluster has at least one point")
}


####SECOND PART (2 OF 2) THIS IS WHERE THE LOOP BEGINS AGAIN, UNTIL NO CHANGE
#Recalculate the epicenters as the means per cluster
  #obtain the row numbers per 1,2,3
  #calculate the means per column using the original matrix
  #populate the epicenters matrix
for(i in 1:3) {
  wines_per_cluster <- wine[all_norms[,4]==i, ]
  #take the transpose here to orient it correctly:
  column_means_per_cluster <- t(colMeans(wines_per_cluster))
  epicenters[i, ] <- column_means_per_cluster
}
    
#repopulate all_norms columns 1-3 by running the function again
for(i in 1:3) {
  for(j in 1:178) {
    e_dist <- dist(rbind(wine[j,], epicenters[i,]), method = "euclidean")
    #put in 178x3 matrix (rows are obs, cols are e dists per epicenter)
    all_norms[j, i] <- e_dist
  }
}

#populate all_norms column 5
#take the min of each row; assign that col as the cluster; put it in fifth 
#col of all_norms
for(i in 1:178) {
  min_dist_index <- which.min(as.vector(all_norms[i, c(1,2,3)]))
  all_norms[i,5] <- min_dist_index
}  

#If each cluster DOESN'T have at least one point,
if ((length(which(all_norms[,5]==1)) > 0)&
    (length(which(all_norms[,5]==2)) > 0)&
    (length(which(all_norms[,5]==3)) > 0) = FALSE ) 
    {
    #then print the most recent cluster assignments, which SHOULD have at least one pt per cluster
     print(all_norms[,4])
    #If this is a weird scenario where the first iteration has less than 3 clusters,
    #(which really shouldn't happen), indicate this with a printed message:
      if ((length(which(all_norms[,4]==1)) > 0)&
         (length(which(all_norms[,4]==2)) > 0)&
         (length(which(all_norms[,4]==3)) > 0) = FALSE ) 
      {
      print("Warning: Less than three clusters")
      }
} 

#If each cluster DOES have at least one point (trying to avoid multiple nested for-loops here),
if ((length(which(all_norms[,5]==1)) > 0)&
    (length(which(all_norms[,5]==2)) > 0)&
    (length(which(all_norms[,5]==3)) > 0)) 
{   #check whether cols 4,5 of all_norms are equal:
    #if they're equal, stop and print the cluster assignments by row
    if ((identical(all_norms[,4], all_norms[,5]))==TRUE) 
    {
    print(all_norms[,4])
    #if they're NOT equal, move the most recent clusters over and loop again
    }else{
      all_norms[,4] <- all_norms[,5]
    #and go back to the start of part 2 - code this
    } 
} 

#here's the cluster chart
library(fpc)
plotcluster(wine, all_norms[ ,5])


