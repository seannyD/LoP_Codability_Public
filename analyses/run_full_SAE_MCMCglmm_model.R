library(MCMCglmm)

# Load 'd'
load("../data/SAE_data_frame.rDat")

k <- length(levels(d$SAE))
I <- diag(k-1)
J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))

prior.m <- list(
  R = list(fix=1, V=0.5 * (I + J), n = 2),
  G=list(
    #Language
    G1=list(V =diag(1),n=1),  # set to 2 for RSlope 
    # Stimulus.code
    G2=list(V =diag(1),n=1),
    # consultant
    G3=list(V =diag(1),n=1)))

m.mcmcglmm <- MCMCglmm(
  SAE ~ -1 + trait * domain,
  ~ us(1):Language + 
    us(1):Stimulus.code +
    us(1):consultant,
  data   = d,
  rcov = ~ us(trait):units,
  family = "categorical",
  prior  = prior.m,
  thin   =      10,
  burnin =   10000,
  nitt   =  110000,
  verbose = F)

save(m.mcmcglmm, file="../results/SAE_mcmc_model_full.rdat")