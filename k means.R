
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
set.seed(2016)
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
  if  (identical(all_norms[,4], all_norms[,5])==TRUE) 
    {output <- all_norms[,4]
    print(output);
    break
  }else{
    
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
      (length(which(all_norms[,5]==3)) > 0) == FALSE ) 
      {
      #then print the t-1 cluster assignments, which SHOULD have at least one pt per cluster
      output <- all_norms[,4]
      print(output); break
      } 

    #if cols 4,5 of all_norms are NOT equal, move the most recent clusters over and loop again
     if ((identical(all_norms[,4], all_norms[,5]))==FALSE) 
      {
        all_norms[,4] <- all_norms[,5]
        all_norms[,5] <- NA
      } 
  
} }

  
#here's the cluster chart
library(fpc)
plotcluster(wine, output)

#another chart, per the blog post. This indicates the typical values per column per cluster.
data(wine)
library(MASS)
parcoord(wine[,-1], output)

#ANALYSIS
#Yes, based on the plot the clusters appear well-separated. There is some very slight overlapping
#of clusters.

#How well the algorithm's clusters correspond to the three wine types:
#To obtain percentage of wines clustered correctly (if each cluster represents a wine type):
#Calculate the sum of: (Type 1 wines matched to type 1 cluster) + (Type 2 wines matched to 
#type 2 cluster) + (Type 3 wines matched to type 3 cluster). Now, repeat this calculation for the
#six permutations of the cluster types, since we don't actually know which wine type corresponds
#to which cluster. Across these 6 sums, take the maximum: this is the number of correct matches.
#Calculate an accuracy ratio: number of correct matches / total observations = 

data(wine)
wine$Output <- output
correct_matches_fxn <- function(x) {
                  correct_matches <- max(  c(
  
                  length(which(x[,1]==1 & x[,15]==1)) +
                  length(which(x[,1]==2 & x[,15]==2)) +
                  length(which(x[,1]==3 & x[,15]==3)),

                  length(which(x[,1]==1 & x[,15]==1)) +
                  length(which(x[,1]==2 & x[,15]==3)) +
                  length(which(x[,1]==3 & x[,15]==2)),
                 
                  length(which(x[,1]==1 & x[,15]==2)) +
                  length(which(x[,1]==2 & x[,15]==1)) +
                  length(which(x[,1]==3 & x[,15]==3)),
                  
                  length(which(x[,1]==1 & x[,15]==2)) +
                  length(which(x[,1]==2 & x[,15]==3)) +
                  length(which(x[,1]==3 & x[,15]==1)),
                  
                  length(which(x[,1]==1 & x[,15]==3)) +
                  length(which(x[,1]==2 & x[,15]==1)) +
                  length(which(x[,1]==3 & x[,15]==2)),
                  
                  length(which(x[,1]==1 & x[,15]==3)) +
                  length(which(x[,1]==2 & x[,15]==2)) +
                  length(which(x[,1]==3 & x[,15]==1))
                  )  )
                  return(correct_matches)
                  }
correct_matches_fxn(wine)
accuracy_ratio <- correct_matches/178
accuracy_ratio

#70.2% is the accuracy ratio here. It's much higher than 33%, the estimated
#accuracy ratio of a random set. 

#Describe what this command does, and show how it affects the results of your clustering.
#scale is a function that centers and/or scales the columns of a numeric matrix.
#Ran it; count = 7 here. Part of the difference is random.

#Before re-running the code, set wine equal to the scaled matrix: 
data(wine)
wine <- scale(wine[-1])
#Run code. Then run plot:
plotcluster(wine, output)

#The clusters are completely distinct, unlike the unscaled data.

data(wine)
wine$Output <- output
correct_matches_fxn(wine)
accuracy_ratio <- correct_matches/178
accuracy_ratio

#The accuracy ratio is exactly the same. On the surface this is surprising, given that visually 
#the scaled clusters are cleaner than the unscaled data. However, the scaling does not represent
#an algorithmic change, so we should not expect a change in the accuracy ratio. The difference 
#in plots, as opposed to actual clustering differences, might reflect some very large unscaled 
#values in the original data, such as Magnesium and Proline, which are magnitudes larger than
#the other data.

#REPEAT BOTH STEPs foR IRIS dATA!!!!!!!!!!

