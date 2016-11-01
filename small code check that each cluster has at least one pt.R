#All of the code below should replace rows 99-107 of the current k means.R file.
#What do you think? If it makes sense, please add it to your version. Thanks! xo


#check that each cluster has at least one point; put T/F in a vector:
for (m in 1:k) {
is_cluster <- c()  
is_cluster[m] <- (length(which(all_norms[,5]==m)) > 0)
}    

#sum across the vector:
logic_sum <- sum(is_cluster)

#If each cluster DOESN'T have at least one point,
if (logic_sum != k)
  
{
  #then print the t-1 cluster assignments, which SHOULD have at least one pt per cluster
  output <- all_norms[,4]
  print(output); break
}