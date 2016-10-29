x <- rbeta(runif(10000) , 6, 4)
plot(density(x))

####################

library("rattle")
data(wine, package="rattle")

#tip: use wine[-1] to exclude the "Type" variable

#INSTRUctions: Write a k-means classifier to cluster the wines 
#into 3 groups.  Use the Euclidean distance measure.

#NOTES:

# NOT DOING THIS: create three clusters: 
#separate by type, take mean of each measurement within type

#all_means <- matrix(, nrow=3, ncol=13)

#for(i in 1:3) {
  
#type_subset <- subset(wine, Type == i)
#type_means <- sapply(type_subset[-1], mean, na.rm=TRUE)
#this is a 3x13 matrix indicating epicenter (cols are coordinates)
#per type (rows))
#all_means[i, ] <- type_means

#}

#Randomly assign 3 epicenters: create empty matrix:
epicenters <- matrix(, nrow=3, ncol=13)
#Sample from each column to obtain an epicenter coordinate:

for(i in 2:14)
sample(wine[ ,i], 1)



#calculate euclidean norm sqrt( (x-x)^2 + (y-y)^2 ):
#for every observation to each of the three clusters: 

#initialize matrix for euclidean norms plus a column for cluster assignment:
all_norms <- matrix(, nrow=178, ncol=4)

#for wine 1:178
for(i in 1:178) {
  #for allmeans 1:3
  for(j in 1:3) {
  e_dist <- sqrt(sum((wine[i,-1] - all_means[j, ])^2, na.rm=TRUE))
  #put in 178x3 matrix (rows are obs, cols are e dists per epicenter)
  all_norms[i, j] <- e_dist
  }
  #take the min of each row; assign that col as the cluster
  min_dist <- min(all_norms[i])
  #put the column number of that min value (indicating the cluster)
  #into the fourth col of all_norms, which indicates cluster assignment
#this doesnt work now:  all_norms[i,4] <-  which.min(min(all_norms[i])
  print(all_norms)
}

#return each cluster as a separate matrix

#verify that each cluster has one point


