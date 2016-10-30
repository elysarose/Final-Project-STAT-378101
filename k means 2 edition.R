
library("rattle")
data(wine, package="rattle")
help(kmeans)
#Remove column 1 from the analysis
wine <- wine[-1]

#FIRST TRY OF FUNCTION - too long and have mistakes
k_means<-function(x, clusters, iterations){
  dimx<-dim(x)
  all_norms <- matrix(nrow=dimx[1], ncol=dimx[2]+1)
    epicenters <- matrix(0, nrow=clusters, ncol=dimx[2])
    #Sample from each column to obtain an epicenter coordinate:
    #i is "k" in k-means
    for(i in 1:clusters) {
      for(j in 1:dimx[2]) {
        col_sample <- sample(wine[,j], 1)
        epicenters[i,j] <- col_sample
      }
    }
  }
  for(i in 1:clusters) {
    for(j in 1:dimx[1]) {
      e_dist <- dist(rbind(wine[j,], epicenters[i,]), method = "euclidean")
      #put in 178x3 matrix (rows are obs, cols are e dists per epicenter)
      all_norms[j, i] <- e_dist
    }
  }
  for(i in 1:dimx[1]) {
    min_dist_index <- which.min(as.vector(all_norms[i, c(1,2,3)]))
    all_norms[i,4] <- min_dist_index
  }
  if ((length(which(all_norms[,4]==1)) > 0)&
      (length(which(all_norms[,4]==2)) > 0)&
      (length(which(all_norms[,4]==3)) > 0) ) {
    #Consider removing the print message
    print ("Each cluster has at least one point")
  }
  #second part
  for(i in 1:clusters-1) {
    column_means_per_cluster <- colMeans(wine[all_norms[,4]==i, ])
    epicenters[i, ] <- column_means_per_cluster
    for(i in 1:dimx[1]) {
      min_dist_index <- which.min(as.vector(all_norms[i, c(1,2,3)]))
      all_norms[i,4] <- min_dist_index
    }

    #verify that each cluster has at least one point
    if ((length(which(all_norms[,5]==1)) > 0)&
        (length(which(all_norms[,5]==2)) > 0)&
        (length(which(all_norms[,5]==3)) > 0) ) {
      #Consider removing the print message
      print ("Each cluster has at least one point")
    }
    
    #check whether cols 4,5 of all_norms are equal
    if ((identical(all_norms[,4], all_norms[,5]))==TRUE) {
      print("K Means where K=3", epicenters)
    }else{
      all_norms[,4] <- all_norms[,5]
      #and go back to the start of part 2 - code this
    } 
    
  }

#SECOND TRY ANOTHER FUNCTION

K_means <- function(x, centers, iterations) {

}




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

#consider checking that the sample falls within each column range

#calculate euclidean dist:
#for every observation to each of the three clusters: 

#initialize matrix for euclidean norms, 
#plus a column for cluster assignment at time t,
#plus a column for cluster assignment at time t+1:
all_norms <- matrix(nrow=178, ncol=4)


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
  min_dist_index <- which.min(as.vector(all_norms[i, c(1,2,3)]))
  all_norms[i,4] <- min_dist_index
}  
all_norms
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
    column_means_per_cluster <- colMeans(wine[all_norms[,4]==i, ])
    epicenters[i, ] <- column_means_per_cluster
}
    
#repopulate all_norms columns 1-3 by calling the function again
populate_all_norms()

#populate all_norms column 5
#take the min of each row; assign that col as the cluster; put it in fifth 
#col of all_norms
for(i in 1:178) {
  min_dist_index <- which.min(as.vector(all_norms[i, c(1,2,3)]))
  all_norms[i,5] <- min_dist_index
}  

#verify that each cluster has at least one point
if ((length(which(all_norms[,5]==1)) > 0)&
    (length(which(all_norms[,5]==2)) > 0)&
    (length(which(all_norms[,5]==3)) > 0) ) {
  #Consider removing the print message
  print ("Each cluster has at least one point")
}

#check whether cols 4,5 of all_norms are equal
if ((identical(all_norms[,4], all_norms[,5]))==TRUE) {
  print("K Means where K=3", epicenters)
    }else{
  all_norms[,4] <- all_norms[,5]
  #and go back to the start of part 2 - code this
  } 




