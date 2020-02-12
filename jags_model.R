library(R2jags)

load("df.rdata")

df$num_of_drinks = round(df$num_of_drinks,0)

# OBTAIN DESIGN MATRIX/ DATA MATRICES
des_mat = model.matrix(lm(num_of_drinks ~ ., data = df))

nbmodel = function(){
  
  ## Likelihood
  for(i in 1:N){
    y[i] ~ dpois(lambda[i]*gam)
    log(lambda[i]) <- mu[i]
    mu[i] <- inprod(beta[],X[i,])
  } 
  gam ~ dgamma(1/phi,1/phi) 
  phi ~ dgamma(2,1)
  
  ## Priors
  for ( i in 1:p){
    beta[i] ~ dnorm(mu.beta,tau.beta)
  }
  
  # prior for var to approximate p(phi)=1/phi
  tau.beta ~ dgamma(.001,.001)
  
}


forJags = list(X=des_mat,
               y=c(df$num_of_drinks),
               N=nrow(des_mat),
               mu.beta=0,
               p = ncol(des_mat))


ZSout = jags(forJags,model=nbmodel, inits=NULL,    
              parameters.to.save=c("beta","tau.beta"), 
              n.iter=5000)



