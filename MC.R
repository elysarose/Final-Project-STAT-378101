

proposalfunction <- function(c, phi_old){   #Defination of proposal function
  rbeta(1, c*phi_old,c*(1-phi_old))
}

c<-1                                        #The constant in proposal function

run_metropolis_MCMC <- function(c, iterations){   # MC algorithm
  chain = array(dim = c(iterations+1,1))
  chain[1,] = runif(1)
  for (i in 1:iterations){
    proposal = proposalfunction(c,chain[i,])
    
    probab = dbeta(proposal,6,4)/dbeta(chain[i,1],6,4)*dbeta(chain[i,],c*proposal,c*(1-proposal))/dbeta(proposal,c*chain[i,],c*(1-chain[i,])) # Acceptance Ratio
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}


chain=run_metropolis_MCMC(1, 10000)

par(mfrow=c(1,3))  #1 row, 3 columns
plot(chain); acf(chain); hist(chain,freq = FALSE)  #plot commands
curve(dbeta(x, 6, 4), add=TRUE)  #adding density function of beta(6,4) to histogram

chain_real <- c(10000,1)
for (i in 1:10000){
  chain_real[i]<- rbeta(1,6,4) #generation a real chain from beta (6,4)
}

par(mfrow=c(1,3))  #1 row, 3 columns
plot(chain_real); acf(chain_real); hist(chain_real,freq = FALSE)  #plot commands
curve(dbeta(x, 6, 4), add=TRUE)  #adding density function of beta(6,4) to histogram
#burnIn = 5000
#acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))
ks.test(chain, chain_real)