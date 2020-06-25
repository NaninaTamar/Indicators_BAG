# Simulating the tail of 2019-nCoV epidemics
# Nanina Anderegg & J. Riou, 5 June 2020

# get simulation number
# args = as.numeric(commandArgs(trailingOnly=TRUE))

# prepare controls
require(tidyverse)

contr = expand.grid(R0=seq(from=0.4,to=1.6, by=0.2),
                    seed=seq(from=10, to=90, by=20), 
                    p=seq(from=0.1, to=0.50, by=0.2))

nsim = dim(contr)[[1]]
contr$n = 1:nsim
incidence = data.frame(matrix(0,nrow=nsim,ncol=60))
contr = cbind(contr,incidence)
contr$stopped = NA

# set limits
#seed = 1
gamma_shape = 2
max_time = 60
max_cases = 3e4
sigma = 5
k = 0.1
delta_shape = 2
delta_scale = 1.5
iota_shape = 2
iota_scale = 2.5
total_sim <- 100
# simulate
start_time <- Sys.time()

sim_cases <- lst(NA)

for(i in 1:total_sim){
for(n in 1:nsim) {
  # initialize
  it_R0 = contr[n,"R0"]
  it_seed = contr[n, "seed"]
  it_p = contr[n, "p"]
  # it_k = contr[n,"k"]
  # it_sigma = contr[n,"sigma"]
  
  cases = it_seed
  t = rep(0, it_seed)
  times = t
  times_reported = sample(t, rbinom(1, cases, it_p)) 
  times_reported = times_reported + 
    rgamma(length(times_reported), shape = iota_shape, scale = iota_scale) +
    rgamma(length(times_reported), shape = delta_shape, scale = delta_scale)
  tmax = 0
  # cases_reported = round(it_p*cases)
  while(cases > 0 & length(times) < max_cases) {
    secondary = rnbinom(cases, size=k, mu=it_R0)
    # secondary_reported = round(p*secondary)
    t.new = numeric()
    for(j in 1:length(secondary)) {
      t.new = c(t.new, t[j] + rgamma(secondary[j], shape = gamma_shape, rate = gamma_shape/sigma))
    }
    cases = length(t.new)
    t.new_reported = sample(t.new, rbinom(1, cases, it_p))    
    t.new_reported = t.new_reported + 
      rgamma(length(t.new_reported), shape = iota_shape, scale = iota_scale) + 
      rgamma(length(t.new_reported), shape = delta_shape, scale = delta_scale)
    t.new = t.new[t.new < max_time]
    t.new_reported = t.new_reported[t.new_reported < max_time]
    t = t.new
    times = c(times, t.new)
    times_reported = c(times_reported, t.new_reported)
  }
  
  # epicurve = cumsum(hist(times, breaks = 0:max_time,plot=FALSE)$counts)
  epicurve_reported = cumsum(hist(times_reported, breaks = 0:max_time, plot=FALSE)$counts)
  epi_cases_reported = hist(times_reported, breaks = 0:max_time, plot=FALSE)$counts
  
  contr[n,6:65] = epi_cases_reported
  contr[n,"stopped"] = length(times) >= max_cases
  #contr[n,"total_incidence"] = length(times)
  print(paste0(n,"/",nsim))
}
contr$sim <- i
sim_cases[[i]] <- contr
}  

end_time <- Sys.time()
# save
saveRDS(sim_cases, "../Data/sim_data2.rds")

