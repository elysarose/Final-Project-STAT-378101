
library("rattle")
data(wine, package="rattle")

#Remove column 1 from the analysis
wine <- wine[-1]

#####INITIAL PART (1 of 2): Random sampling, and cluster placement based on 
#the random sampling:
#Randomly assign 3 epicenters: create empty matrix:
K_means<-function(x,k){ 
  epicenters <- matrix(0, nrow=3, ncol=13)
  #Sample from each column to obtain an epicenter coordinate:
  #i is "k" in k-means
  set.seed(2016)
  for(i in 1:k) {
    for(j in 1:ncol(x)) {
      col_sample <- sample(x[,j], 1)
      epicenters[i,j] <- col_sample
    }
  }
  
  #Verified that the sample falls within each column range
  
  #calculate euclidean dist:
  #for every observation to each of the three clusters: 
  
  #initialize matrix for euclidean norms, 
  #plus a column for cluster assignment at time t,
  #plus a column for cluster assignment at time t+1:
  all_norms <- matrix(0, nrow=nrow(x), ncol=k+2)
  
  
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
  
  #DELETE THIS?
  #verify that each cluster has at least one point
  #if ((length(which(all_norms[,4]==1)) > 0)&
  #    (length(which(all_norms[,4]==2)) > 0)&
  #    (length(which(all_norms[,4]==3)) > 0) ) {
  #Consider removing the print message
  #    print ("Each cluster has at least one point")
  #}
  
  ####SECOND PART (2 OF 2) THIS IS WHERE THE LOOP BEGINS, UNTIL NO CHANGE in cluster assignments
  
  #Start the repeat loop and test for identical outputs in past 2 iterations
  
  count <- 0
  
  repeat { count <- count + 1
  if  (identical(all_norms[,k+1], all_norms[,k+2])==TRUE) 
  {output <- all_norms[,k+1]
  print(output);
  break
  }else{
    
    #Recalculate the epicenters as the means per cluster
    #obtain the row numbers per 1,2,3
    #calculate the means per column using the original matrix
    #populate the epicenters matrix
    for(i in 1:k) {
      wines_per_cluster <- wine[all_norms[,k+1]==i, ]
      #take the transpose here to orient it correctly:
      column_means_per_cluster <- t(colMeans(wines_per_cluster))
      epicenters[i, ] <- column_means_per_cluster
    }
    
    #repopulate all_norms columns 1-3 by running the function again
    for(i in 1:k) {
      for(j in 1:nrow(x)) {
        e_dist <- dist(rbind(x[j,], epicenters[i,]), method = "euclidean")
        #put in 178x3 matrix (rows are obs, cols are e dists per epicenter)
        all_norms[j, i] <- e_dist
      }
    }
    
    #populate all_norms column 5
    #take the min of each row; assign that col as the cluster; put it in fifth 
    #col of all_norms
    for(i in 1:nrow(x)) {
      min_dist_index <- which.min(as.vector(all_norms[i, c(1:k)]))
      all_norms[i,k+2] <- min_dist_index
    }  
    
    #If each cluster DOESN'T have at least one point,
    for (m in 1:k) {
      is_cluster <- c()  
      is_cluster[m] <- (length(which(all_norms[,k+2]==m)) > 0)
    }    
    
    #sum across the vector:
    logic_sum <- sum(is_cluster)
    
    #If each cluster DOESN'T have at least one point,
    if (logic_sum != k)
      
    {
      #then print the t-1 cluster assignments, which SHOULD have at least one pt per cluster
      output <- all_norms[,k+1]
      print(output); break
    }
    
    #if cols 4,5 of all_norms are NOT equal, move the most recent clusters over and loop again
    if ((identical(all_norms[,k+1], all_norms[,k+2]))==FALSE) 
    {
      all_norms[,k+1] <- all_norms[,k+2]
      all_norms[,k+2] <- NA
    } 
    
  }} 
}



K_means(wine,3)
#here's the cluster chart
library(fpc)
plotcluster(wine, output)

#another chart, per the blog post. This indicates the typical values per column per cluster.
data(wine)
library(MASS)
parcoord(wine[,-1], output)

#ANALYSIS
#Yes, based on the plot the clusters appear well-separated. There is some very slight overlapping
#of clusters, although that might not be representative of the actual arrangement in 13-space.

#How well the algorithm's clusters correspond to the three wine types:
#Percentage of wines clustered correctly (if each cluster represents a wine type):
#Plotcluster (wine, wine$Type). Visually inspect: which cluster based on type most closely
#resembles which current cluster. Ensure that the matching is correct (e.g., type 1 to cluster 2)
#Calculate an accuracy ratio: COUNT/178 = 

data(wine)
plotcluster(wine[ , 2:14], wine$Type)

#Visually: cluster1 -> type1, cluster2 -> type3, cluster3 -> type2

data(wine)
wine$Output <- output
correct_matches <- length(which(wine[,1]==1 & wine[,15]==1)) +
  length(which(wine[,1]==2 & wine[,15]==3)) +
  length(which(wine[,1]==3 & wine[,15]==2))
correct_matches/178

#0.4775281 is the accuracy ratio here. It's not fantastic, but it's better 0.33, the predicted
#accuracy ratio of pure guessing (is this correct?)

data(wine)
wine <- scale(wine[-1])

#Describe what this command does, and show how it affects the results of your clustering.
#scale is a function that centers and/or scales the columns of a numeric matrix.
#Ran it; count = 7 here. Part of the difference is random.

plotcluster(wine, output)

#The clusters are completely distinct, unlike the unscaled data.
#Visually: cluster1 -> type1, cluster2 -> type3, cluster3 -> type2

data(wine)
wine$Output <- output
correct_matches <- length(which(wine[,1]==1 & wine[,15]==3)) +
  length(which(wine[,1]==3 & wine[,15]==1)) +
  length(which(wine[,1]==2 & wine[,15]==2))
correct_matches/178
#FOr above: consider writing a for loop to take the max of all matched sets, instead of 
#visual inspection

#At 0.3539326, the accuracy ratio is less than the unscaled. WHY

#REPEAT BOTH STEPs foR IRIS dATA!!!!!!!!!!

#For unscaled, it looks as though the iteration count here was 6. (count=6)
