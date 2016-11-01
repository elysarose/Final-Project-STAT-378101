
library("rattle")
data(wine, package="rattle")

k_means<-function(x,k){  
  random_centers<-random.centers(x,k)
  all_norms<-all_norms(x,k)
  centr<-random_centers
  centr_old<-matrix(0, nrow=k, ncol=ncol(x))
  while ((identical(centr, centr_old))==FALSE){
    centers<-means(x,k)
    all_norms<-all_norms(x,k)
    centr_old<-centr
    centr<-means
  } 
  return(all_norms) 
} 
k_means(wine,3)
kmeans()


all_norms



#Remove column 1 from the analysis
wine <- wine[-1]

#####INITIAL PART (1 of 2): Random sampling, and cluster placement based on 
#the random sampling:
#Randomly assign 3 epicenters: create empty matrix:
x<-wine



random.centers<-function(x,k){
epicenters <- matrix(0, nrow=k, ncol=ncol(x))
#Sample from each column to obtain an epicenter coordinate:
#i is "k" in k-means
for(i in 1:k) {
  for(j in 1:ncol(x)) {
    col_sample <- sample(x[,j], 1)
    epicenters[i,j] <- col_sample
  }
}
return(epicenters)
}
random.centers(wine,3)

#Verified that the sample falls within each column range

#calculate euclidean dist:
#for every observation to each of the three clusters: 

#initialize matrix for euclidean norms, 
#plus a column for cluster assignment at time t,
#plus a column for cluster assignment at time t+1:
all_norms<-function(x,k){
all_norms <- matrix(0, nrow=nrow(x), ncol=k+1)
epicenters<-random.centers(x,k)
for(i in 1:k) {
  for(j in 1:nrow(x)) {
    e_dist <- dist(rbind(x[j,], epicenters[i,]), method = "euclidean")
    #put in 178x3 matrix (rows are obs, cols are e dists per epicenter)
    all_norms[j, i] <- e_dist
  }
}
#Verified that this calculation is correct
#take the min of each row; assign that col as the cluster; put it in fourth 
#col of all_norms
for(i in 1:nrow(x)) {
  min_dist_index <- which.min(as.vector(all_norms[i, c(1:k)]))
  all_norms[i,k+1] <- min_dist_index
}  
#verify that each cluster has at least one point
if ((length(which(all_norms[,4]==1)) > 0)&
    (length(which(all_norms[,4]==2)) > 0)&
    (length(which(all_norms[,4]==3)) > 0) ) {
#Consider removing the print message
    print ("Each cluster has at least one point")
}
return(all_norms)
} 
all_norms(wine,3)





####SECOND PART (2 OF 2) THIS IS WHERE THE LOOP BEGINS AGAIN, UNTIL NO CHANGE
#Recalculate the epicenters as the means per cluster
  #obtain the row numbers per 1,2,3
  #calculate the means per column using the original matrix
  #populate the epicenters matrix
means<-function(x,k){
  all_norms<-all_norms(x,k)
  epicenters<-random.centers(x,k)
for(i in 1:k) {
  x_per_cluster <- x[all_norms[,k+1]==i, ]
  #take the transpose here to orient it correctly:
  column_means_per_cluster <- t(colMeans(x_per_cluster))
  epicenters[i, ] <- column_means_per_cluster
}
  return(epicenters)
}  

means(wine,3)




k_means<-function(x,k,iterations){
  random_centers<-random.centers(x,k)
  all_norms<-all_norms(x,k)
  for (i in 1:iterations){
    centers<-means(x,k)
    all_norms<-all_norms(x,k)
  }
 return(all_norms) 
}
k_means(wine,3,3)














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

#verify that each cluster has at least one point
if ((length(which(all_norms[,5]==1)) > 0)&
    (length(which(all_norms[,5]==2)) > 0)&
    (length(which(all_norms[,5]==3)) > 0) ) {
  #KEEP GOING
} else{
  #PRINT THE CLUSTER OUTPUT FROM THE PREVIOUS STATE
}

#check whether cols 4,5 of all_norms are equal
if ((identical(all_norms[,4], all_norms[,5]))==TRUE) {
  print("K Means where K=3", all_norms)
    }else{
  all_norms[,4] <- all_norms[,5]
  #and go back to the start of part 2 - code this
  } 
