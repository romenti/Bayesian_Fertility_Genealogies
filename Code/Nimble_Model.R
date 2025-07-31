#### Code to fit the model ####


# upload all relevant packages

source('code/upload_packages.R')


# load model input data

load('Data/model_data_input.RData')

# data_input: list of data input for the models
# init: list of initial values for model parameters
# const: list of constant values measuring the range of the indices




# model specification

model_nimble = nimble::nimbleCode({
  
  # Hyperparameters
  
  sigma_beta_c ~ dinvgamma(1,1)
  beta_dummy ~ dnorm(mean=0,sd=1)
  sd_sigma_beta_sp ~ dunif(0,40)
  mu_sigma_beta_sp ~ dnorm(mean=0,sd=10)
  for(j in 1:A){
    sigma_TFR[j] ~  dunif(0,40)
    beta_c[j] ~ dnorm(mean=0,sd=sigma_beta_c)
    sigma_theta[j] ~ dinvgamma(1,1)
    log_sigma_beta_sp[j] ~ dnorm(mean=mu_sigma_beta_sp,sd=sd_sigma_beta_sp)
    sigma_beta_sp[j] <- exp(log_sigma_beta_sp[j])
  }
  
  
  for(j in 1:A){
    for(p in 1:18){
      beta_sp[j,p] ~ dnorm(mean=0,sd=sigma_beta_sp[j])
    }
  }
  
  for(i in 1:Ti){
    for(j in 1:A){
      r_splines[i,j] <- inprod(Z[i,1:18,j], beta_sp[j,1:18])
      c_splines[i,j] <- BG[i,j]*beta_c[j]
    }
  }
  
  for(i in 1:Ti){
    for(j in 1:A){
      theta_mean[i,j] <- r_splines[i,j]+c_splines[i,j]
    }
  }
  
  # Likelihood for theta
  for(i in 1:Ti){
    for(j in 1:A){
      theta[i,j] ~ dnorm(mean=theta_mean[i,j],var=sigma_theta[j])
    }
  }
  
  for(j in 1:A){
    for(i in Index_TFR[j]:Ti){
      TFR[i,j] ~ dnorm(mean=TFR_nat[i,j],sd=sigma_TFR[j])
    }
  }
  
  for(j in 2:A){
    for(i in 1:(Index_TFR[j]-1)){
      TFR[Index_TFR[j]-i,j] ~ dnorm(mean=TFR[Index_TFR[j]-i+1,j],sd=sigma_TFR[j])
    }
  }
  
  for(i in 1:Ti){
    for(j in 1:A){
      
      # Priors for beta
      beta[i,1,j] ~ dnorm(mean=0,sd=1)
      beta[i,2,j] ~ dnorm(mean=0,sd=1)
      
      # Compute h from q5
      h[i,j] <- log(q5[i,j])
      
      # Likelihood for q5
      q5[i,j] ~ dbeta(q5_a[i,j], q5_b[i,j])
      
      # Priors for k
      k[i,j] ~ dnorm(mean=0,sd=1)
      
      # Compute mortality rates (mx)
      mx[i,1,j] <- exp(af[1] + bf[1] * h[i,j] + cf[1] * (h[i,j]^2) + vf[1] * k[i,j])
      mx[i,2,j] <- -0.25 * (mx[i,1,j] + log(1 - q5[i,j]))
      mx[i,3:11,j] <- exp(af[3:11] + bf[3:11] * h[i,j] + cf[3:11] * (h[i,j]^2) + vf[3:11] * k[i,j])
      
      
      # Compute lx
      
      lx[i,1,j] <- 1
      lx[i,2,j] <- lx[i,1,j] * exp(-mx[i,1,j])
      lx[i,3,j] <- lx[i,2,j] * exp(-4*mx[i,2,j])
      for(z in 4:12){
        lx[i,z,j] <- lx[i,z-1,j] * exp(-5*mx[i,z-2,j])
      }
      
      # Compute Lx
      Lx[i,1,j] <- 0.5 * (lx[i,1,j] + lx[i,2,j]) + 2 * (lx[i,2,j] + lx[i,3,j])
      Lx[i,2,j] <- 5 * (lx[i,3,j] + lx[i,4,j]) / 2
      for(z in 3:10){
        Lx[i,z,j] <- 5 * (lx[i,z+1,j] + lx[i,z+2,j]) / 2
      }
      
      # Compute gamma
      for(z in 1:7){
        gamma[i,z,j] <- m[z] + X[1,z] * beta[i,1,j] + X[2,z] * beta[i,2,j]
      }
      
      # Compute phi
      phi[i,1:7,j] <- exp(gamma[i,1:7,j]) / sum(exp(gamma[i,1:7,j]))
      
      # Compute Fx
      Fx[i,1,j] <- 0
      Fx[i,2:8,j] <- phi[i,1:7,j] * TFR[i,j] / 5
      
      # Compute Kx
      Kx[i,1:7,j] <- (Lx[i,3:9,j] / Lx[i,4:10,j] * Fx[i,1:7,j] + Fx[i,2:8,j]) * Lx[i,1,j] / 2
      Kx_cov[i,1:7,j] <- Kx[i,1:7,j]*exp((-1)*beta_dummy*dummy_usa[i,j])
      # Adjusted Kx with theta
      Kx_star[i,1:7,j] <- Kx_cov[i,1:7,j] * exp(theta[i,j])
    }
  }
  
  
  # Likelihood for C_true
  for(j in 1:A){
    for(i in Index[j]:Ti){
      C_expected_true[i,j] <- inprod(Kx_cov[i,1:7,j], W_true[i,1:7,j])
      C_true[i,j] ~ dpois(C_expected_true[i,j])
    }
  }
  
  # Likelihood for C
  for(j in 1:A){
    for(i in 1:Ti){
      C_expected[i,j] <- inprod(Kx_star[i,1:7,j], W_gen[i,1:7,j])
      C[i,j] ~ dpois(C_expected[i,j])
    }
  }
  
})



# run the model

run_single_chain <- function(chain_id, model_code, data, consts, 
                             init, monitors, niter, nburnin, thin) {
  library(nimble)
  library(nimbleHMC)
  
  # Initialize the model
  dynModel <- nimbleModel(
    code = model_code,
    data = data, 
    constants = consts, 
    buildDerivs = TRUE,
    inits = init[[chain_id]]  # Use chain-specific initial values
  )
  
  # Run HMC
  model_output <- nimbleHMC(
    dynModel, 
    data = data, 
    inits = init[[chain_id]],
    monitors = monitors, 
    thin = thin,
    niter = niter, 
    nburnin = nburnin,
    summary = TRUE,
    WAIC = FALSE,
    progressBar = getNimbleOption("MCMCprogressBar")
  )
  
  return(model_output$samples)  # Return MCMC samples
}



# Number of chains and cores
nchains <- 4
ncores <- nchains

# Define initial values for each chain
init_list <- list(
  chain1 = init,  
  chain2 = init,
  chain3 = init,
  chain4 = init)  

# Create the parallel cluster
cl <- makeCluster(ncores)


# Export necessary objects to the workers
clusterExport(cl, c("model_nimble_new", "data_input", "const", "init_list", "run_single_chain"))
clusterEvalQ(cl, {
  library(nimble)
  library(nimbleHMC)
})

# Run each chain in parallel
chain_results <- parLapply(
  cl = cl,
  X = 1:nchains,
  fun = function(chain_id) {
    run_single_chain(
      chain_id = chain_id,
      model_code = model_nimble_new,
      data = dd_cov,
      consts = const_new,
      init = init_list,
      monitors = names(init_list$chain1),
      niter = 6000,
      nburnin = 3000,
      thin = 10
    )
  }
)

stopCluster(cl)



# Save the results

# all parameters

mcmc.out_all = MCMCsummary(
  chain_results,
  params = "all",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 



# check convergence using Rhat
mcmc.out_all$Rhat[mcmc.out_all$Rhat>1.1]

ifelse(sum(mcmc.out_all$Rhat[mcmc.out_all$Rhat>=1.1])>0,
       'Rhat>1.1 for some parameters',
       'Rhat<1.1 for all parameters')


# Extract TFR posterior estimates

mcmc.out_TFR_final = MCMCsummary(
  chain_results,
  params = "TFR",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 

# add relevant variables

mcmc.out_TFR_final$years = rep(1751:1910,8)

names(mcmc.out_TFR_final)[3:5] = c('TFR_lower','TFR_median','TFR_upper')

mcmc.out_TFR_final$country = rep(c("SWE","ENG","FRA",
                                   "NOR","DEN","NLD","USA","FIN"),each=160)




# Extract theta posterior estimates (log-scale)


mcmc.out_theta_final = MCMCsummary(
  chain_results,
  params = "theta",
  excl = NULL,
  ISB = TRUE,
  exact = TRUE,
  probs = c(0.025, 0.5, 0.975),
  hpd_prob = 0.95,
  HPD = FALSE,
  pg0 = FALSE,
  digits = NULL,
  round = NULL,
  Rhat = TRUE,
  n.eff = TRUE,
  func = NULL,
  func_name = NULL
) 

mcmc.out_theta_final$years = rep(1751:1910,8)

names(mcmc.out_theta_final)[3:5] = c('theta_lower','theta_median','theta_upper')

mcmc.out_theta_final$country = rep(c("SWE","ENG","FRA",
                                   "NOR","DEN","NLD","USA","FIN"),
                                   each=160)



# save results in a .RData file

save(mcmc.out_all,mcmc.out_theta_final,mcmc.out_TFR_final,
     file='Results/bayesian_model_results.RData')

