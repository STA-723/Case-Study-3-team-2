library(R2jags)
library(corrplot)

# load in data
load("df.rdata")

df$num_of_drinks = round(df$num_of_drinks,0)

# look at correlation plot
corrplot(cor(df[,48:50]), method="pie")


#df = df[sample(nrow(df),1000),]

# OBTAIN DESIGN MATRIX/ DATA MATRICES
des_mat = model.matrix(lm(num_of_drinks ~ ., data = df))

nbmodel = function(){
  
  for(i in 1:N){
    ## Likelihood
    y[i] ~ dnegbin(prob[i],r)
    prob[i] <- r/(r+(1-zero[i])*lambda.count[i]) - 1e-10*zero[i]
    lambda.count[i] <- exp(mu.count[i])
    mu.count[i] <- inprod(beta[],X[i,])
    
    ## Zero-Inflation
    zero[i] ~ dbern(pi[i])
    pi[i] <- ilogit(mu.binary[i])  
    mu.binary[i] <- inprod(alpha[],X[i,])
    
  } 
  r ~ dunif(0,50)
  
  ## Priors
  for ( i in 1:p){
    beta[i] ~ dnorm(mu.beta,tau.beta)
    alpha[i] ~ dnorm(mu.alpha,tau.alpha)
  }
  
  # prior for var to approximate p(phi)=1/phi
  tau.beta ~ dgamma(.001,.001)
  tau.alpha ~ dgamma(.001,.001)
  
}


forJags = list(X=des_mat,
               y=c(df$num_of_drinks),
               N=nrow(des_mat),
               mu.beta=0,
               mu.alpha=0,
               p = ncol(des_mat))


ZSout = jags(forJags,model=nbmodel, inits=NULL,    
              parameters.to.save=c("beta","tau.beta"), 
              n.iter=5000)



