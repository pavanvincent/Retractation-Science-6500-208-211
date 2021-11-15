rm(list = ls())

##################################
## 0) Loading packages and set up environment
##################################

library(readxl)
library(deSolve)
library(RColorBrewer)
library(openxlsx)
library(foreach)

setwd('..')


##################################
## 1) Defining model parameters
##################################
## Date of the hospital admission file
date_file <- as.Date('2020-05-08')
## Time window used for the calibration
date_end_calibration <- as.Date('2020-05-07')
date_beginning_calibration <- as.Date('2020-03-15')
## Timing of the intervention
time_intervention <- 55
date_intervention <- as.Date('2020-03-17')
## Timing at which P[ICU|Hosp] changes
date_begin_change_ICU <- as.Date('2020-03-20')
date_end_change_ICU <- as.Date('2020-04-07')
time_begin_change <- as.numeric(time_intervention + date_begin_change_ICU - date_intervention)
time_end_change <- as.numeric(time_intervention + date_end_change_ICU - date_intervention)

##################################
## 2) Loading data
##################################
### (A) Population data
correspondance_region_names <- read_excel('Data/correspondance_region_names.xlsx')
vect_name_region <- c('ARA', 'BFC', 'BRE', 'CVL', 'COR', 'GES', 'HDF', 'IDF', 'NAQ', 'NOR', 'OCC', 'PAC', 'PDL')
n_regions <- length(vect_name_region)
pop_region <- read.csv2('Data/population_data/pop_region.csv', sep = ";", row.names = 1)

name.ageG <- row.names(pop_region) # Name of the considered age groups
n.ageG <- length(name.ageG)

### (B) Contact matrices
# Contact matrix before the lockdown
contactMatrix <- readRDS('Data/contact_matrix/mat_regular_10_80_V2.rds')
# Contact matrix during the lockdown
contactMatrix.int <- readRDS('Data/contact_matrix/lockdown_noSDeld.rds')
colnames(contactMatrix.int) <- rownames(contactMatrix.int) <-  colnames(contactMatrix) <- rownames(contactMatrix) <- name.ageG


### (C) Hospitalization data
# Loading hospital and ICU admission file
admission_file <- read.csv(paste0('Data/SIVIC/', format(date_file, '%Y%m%d'), '/SIVIC_daily_numbers_region_corrected_histo_',
                                  format(date_file, '%Y%m%d'), '.csv'))
admission_file <- admission_file[admission_file$region %in% vect_name_region,]
admission_file$date <- as.Date(admission_file$date, format = '%Y-%m-%d')

# Loading file of beds occupied
total_occupancy_file <- read.csv(paste0('Data/SIVIC/', format(date_file, '%Y%m%d'), '/SIVIC_total_numbers_region_corrected_histo_',
                                        format(date_file, '%Y%m%d'), '.csv'))
total_occupancy_file <- total_occupancy_file[total_occupancy_file$region %in% vect_name_region,]
total_occupancy_file$date <- as.Date(total_occupancy_file$date)

#### Time series of daily ICU admissions per region
mat_daily_ICU <- sapply(vect_name_region, FUN = function(name_region){
  tmp_adm <- admission_file[admission_file$region == name_region,c('date', 'icu_pred')]
  tmp_adm <- tmp_adm[order(tmp_adm$date),]
  tmp_adm[tmp_adm$date >=date_beginning_calibration & tmp_adm$date <= date_end_calibration,]$icu_pred
})
mat_daily_ICU_before_beginning <- sapply(vect_name_region, FUN = function(name_region){
  tmp_adm <- admission_file[admission_file$region == name_region,c('date', 'icu_pred')]
  tmp_adm <- tmp_adm[order(tmp_adm$date),]
  tmp_adm[tmp_adm$date < date_beginning_calibration,]$icu_pred
}) ## Not used for calibration

#### Time series of hospital admissions per region
mat_daily_hosp <- sapply(vect_name_region, FUN = function(name_region){
  tmp_hosp <- admission_file[admission_file$region == name_region,c('date', 'hosp_dd_pred')]
  tmp_hosp <- tmp_hosp[order(tmp_hosp$date),]
  tmp_hosp[tmp_hosp$date >=date_beginning_calibration & tmp_hosp$date <= date_end_calibration,]$hosp_dd_pred
})
mat_daily_hosp_before_beginning <- sapply(vect_name_region, FUN = function(name_region){
  tmp_hosp <- admission_file[admission_file$region == name_region,c('date', 'hosp_dd_pred')]
  tmp_hosp <- tmp_hosp[order(tmp_hosp$date),]
  tmp_hosp[tmp_hosp$date < date_beginning_calibration,]$hosp_dd_pred
}) ## Not used for calibration

#### Time series of ICU beds per region
mat_ICU_occ <- sapply(vect_name_region, FUN = function(name_region){
  tmp_ICU <- total_occupancy_file[total_occupancy_file$region == name_region,c('date', 'icu_pred')]
  tmp_ICU <- tmp_ICU[order(tmp_ICU$date),]
  tmp_ICU[tmp_ICU$date >=date_beginning_calibration & tmp_ICU$date <=date_end_calibration,]$icu_pred
})
mat_ICU_occ_before_beginning <- sapply(vect_name_region, FUN = function(name_region){
  tmp_ICU <- total_occupancy_file[total_occupancy_file$region == name_region,c('date', 'icu_pred')]
  tmp_ICU <- tmp_ICU[order(tmp_ICU$date),]
  tmp_ICU[tmp_ICU$date < date_beginning_calibration ,]$icu_pred
}) ## Not used for calibration

#### Time series of general ward beds per region
mat_hosp_occ <-sapply(vect_name_region, FUN = function(name_region){
  tmp_ICU <- total_occupancy_file[total_occupancy_file$region == name_region,c('date', 'icu_pred')] ## ICU beds
  tmp_hosp <- total_occupancy_file[total_occupancy_file$region == name_region,c('date', 'hosp_pred')] ## Beds (ICU + General ward)
  tmp_ICU <- tmp_ICU[order(tmp_ICU$date),]
  tmp_hosp <- tmp_hosp[order(tmp_hosp$date),]
  tmp_hosp[tmp_hosp$date >=date_beginning_calibration & tmp_hosp$date <= date_end_calibration,]$hosp_pred - tmp_ICU[tmp_ICU$date >=date_beginning_calibration & tmp_ICU$date <= date_end_calibration,]$icu_pred
})
mat_hosp_occ_before_beginning <-sapply(vect_name_region, FUN = function(name_region){
  tmp_ICU <- total_occupancy_file[total_occupancy_file$region == name_region,c('date', 'icu_pred')]
  tmp_hosp <- total_occupancy_file[total_occupancy_file$region == name_region,c('date', 'hosp_pred')]
  tmp_ICU <- tmp_ICU[order(tmp_ICU$date),]
  tmp_hosp <- tmp_hosp[order(tmp_hosp$date),]
  tmp_hosp[tmp_hosp$date < date_beginning_calibration,]$hosp_pred - tmp_ICU[tmp_ICU$date < date_beginning_calibration,]$icu_pred
}) ## Not used for calibration

### (D) Severity estimates
load('Data/severity_estimates/baseline/fitprobHosp_Base_AgeGroup.RData')
load('Data/severity_estimates/baseline/fitprobICUByAgeGivenHosp_Base_AgeGroup.RData')
fitprobHosp # P[Hosp | Inf]
fitprobICUByAgeGivenHosp # P[ICU |Hosp]

p.hosp.inf <- fitprobHosp[,'50%']
p.icu.hosp.average <- fitprobICUByAgeGivenHosp[,'50%']

##################################
## 3) Defining model parameters
##################################
##Time step and vector used when solving the ODE
dt <- 0.25
times <- seq(0, 115, dt)
times_1 <- seq(0, max(times), 1)

## Time to infectiosity (Rate out of E1)
g1<-1/4
## Time to symptom onset (Rate out of E2)
g2 <-1/1
## Time to mild cases recovery
g3 <-1/3
## Relative contribution of the different trajectories to the infection (used to compute growth rate from R0)
c2 <- g3/(g2+g3)
c3 <- g2/(g2+g3)

## Rate of ICU admissions once hospitalized
g_to_ICU <- 1/1.5

## Rate of hospital admission for patients who will require an ICU admission
time_to_hosp_ICU <- 3
g_to_hosp_ICU <- 1/time_to_hosp_ICU
## Rate of ICU admission for patients who will not require an ICU admission
g_to_hosp <- 1/(time_to_hosp_ICU + 1)


## Relative infectivity of children
relative_infectivity_children <- 1.0
susceptibility <- rep(1, n.ageG) ## Age-dependent susceptibility
infectivity <- c(rep(relative_infectivity_children, 2), rep(1.0, n.ageG - 2)) ## Age-dependent infectivity

##################################
## 4) Adjusting the contact matrices to account for relative infectivity of children
##################################
contactMatrix_corr<- matrix(0, ncol = ncol(contactMatrix), nrow = nrow(contactMatrix))
for(i in 1:nrow(contactMatrix_corr)){
  for(j in 1:ncol(contactMatrix_corr)){
    contactMatrix_corr[i,j] <- contactMatrix[i,j]*infectivity[j]*susceptibility[i]
  }
}
eigenvalues <- eigen(contactMatrix_corr)$values
max_eigenval1 <- max(Re(eigenvalues[abs(Im(eigenvalues)) < 1e-6]))


contactMatrix_corr.int<- matrix(0, ncol = ncol(contactMatrix.int), nrow = nrow(contactMatrix.int))
for(i in 1:nrow(contactMatrix_corr.int)){
  for(j in 1:ncol(contactMatrix_corr.int)){
    contactMatrix_corr.int[i,j] <- contactMatrix.int[i,j]*infectivity[j]*susceptibility[i]
  }
}
eigenvalues <- eigen(contactMatrix_corr.int)$values
max_eigenval2 <- max(Re(eigenvalues[abs(Im(eigenvalues)) < 1e-6]))


##################################
## 5) Defining functions used to compute the growthrate
## Ref: J. Wallinga, M. Lipsitch, Proc. Biol. Sci.274, 599-604 (2007)
##################################
f <- function(r, R0){
  res <- (1+r/g1)*(1+r/g2)*(1+r/g3) - c2*(1+r/g3)*R0 - c3*R0
  return(res)
}
get_growth_rate_from_R0 <- function(R0){
  step <- 1e-4
  vect_growth_rates <- seq(0, 0.7, step)
  sup_growth_rate <- vect_growth_rates[which(sapply(vect_growth_rates, FUN = function(x){f(x, R0)}) > 0)[1]]
  growth_rate <- sup_growth_rate - step/2
  return(growth_rate)
}
get_infectious_init <- function(R0, log_Inf_lockdown, time_intervention){
  growth_rate <- get_growth_rate_from_R0(R0)
  log_inf_init <- log_Inf_lockdown - growth_rate*time_intervention
  return(exp(log_inf_init))
}

##################################
## 6) Defining functions to compute to reduction in P[ICU | Hosp]
##################################
# Function used to obtain the correction for a given alpha at a certain time
get_slope <- function(alpha, t){
  tmp_slope <- 1 + (t - time_begin_change)/(time_end_change - time_begin_change)*(alpha - 1)
  return(tmp_slope)
}

# Getting data that were used to estimate the average P[ICU | Hosp]
time_beginning_average <- as.Date('2020-03-01')
time_end_average <- as.Date('2020-05-07')
date_file_average <- as.Date('2020-05-08')
national_admission_file <- read.csv(paste0('Data/SIVIC/', format(date_file_average, '%Y%m%d'), '/SIVIC_daily_numbers_corrected_histo_',
                                           format(date_file_average, '%Y%m%d'), '.csv'))
national_admission_file$date <- as.Date(national_admission_file$date, format = '%Y-%m-%d')
vect_time_average <- as.Date(seq(0, time_end_average - time_beginning_average),
                             origin = time_beginning_average)
vect_hospitalization_average <- national_admission_file[national_admission_file$date >= time_beginning_average & national_admission_file$date <= time_end_average,]$hosp_dd_pred

# Function to compute the baseline probabilty for a given alpha
# (Baseline = Value of P[ICU|Hosp] before date_begin_change_ICU)
get_baseline_proba <- function(alpha,
                               time_beginning_average,
                               time_end_average,
                               vect_hospitalization_average,
                               proba_average){
  N_tot <- sum(vect_hospitalization_average)
  
  N_before_change <- sum(vect_hospitalization_average[vect_time_average < date_begin_change_ICU])
  N_after_change <- sum(vect_hospitalization_average[vect_time_average > date_end_change_ICU])
  weighted_sum_during_decrease <- sum(sapply(0:(time_end_change - time_begin_change), FUN = function(i){
    tmp_time <- time_begin_change + i
    tmp_date <- date_begin_change_ICU + i
    tmp_slope <- get_slope(alpha, tmp_time)
    tmp_slope*vect_hospitalization_average[vect_time_average == tmp_date]
  }))
  
  baseline_proba <- proba_average*N_tot/(N_before_change + alpha*N_after_change + weighted_sum_during_decrease)
  baseline_proba
}

##################################
## 7) Defining the model for a specific region
##################################
output_from_param <- function(R0,
                              g_out_ICU,
                              g_out_hosp,
                              alpha,
                              R0_after_intervention,
                              log_Inf_lockdown,
                              name_region){
  
  ## Defining baseline transmission matrix based on the contact matrix we are using
  dur.inf<-1/g2+1/g3 # Length of infectiousness
  
  # Transmission rate before lockdown
  transRate1<-R0/dur.inf/max_eigenval1
  # Transmission rate during lockdown
  transRate2 <- R0_after_intervention/dur.inf/max_eigenval2
  B1<-transRate1*contactMatrix_corr
  B2 <- transRate2*contactMatrix_corr.int
  
  ## Defining the baseline probability based on the average P[ICU|Hosp]
  p.icu.hosp.baseline <- get_baseline_proba(alpha = alpha,
                                            time_beginning_average = time_beginning_average,
                                            time_end_average = time_end_average,
                                            vect_hospitalization_average = vect_hospitalization_average,
                                            proba_average = p.icu.hosp.average)
  
  
  popSize.ageG <- pop_region[,colnames(pop_region) == name_region]
  popSize <- sum(popSize.ageG)
  
  ## Defining the model
  ## Model structure
  SEIR.simple <- function(t,Y, parms){
    with(as.list(c(Y, parms)), {
      S <- Y[1:n.ageG]
      E1 <- Y[1*n.ageG+1:n.ageG]
      E2 <- Y[2*n.ageG+1:n.ageG]
      I.mild <- Y[3*n.ageG+1:n.ageG]
      I.hosp <- Y[4*n.ageG+1:n.ageG]
      
      I.bar.hosp <- Y[5*n.ageG+1:n.ageG]
      I.bar.ICU <- Y[6*n.ageG+1:n.ageG]
      
      H.ICU <- Y[7*n.ageG+1:n.ageG]
      ICU.1 <- Y[8*n.ageG+1:n.ageG]
      ICU.2 <- Y[9*n.ageG+1:n.ageG]
      H.1 <- Y[10*n.ageG+1:n.ageG]
      H.2 <- Y[11*n.ageG+1:n.ageG]
      ICU.cum <- Y[12*n.ageG+1:n.ageG]
      H.cum <- Y[13*n.ageG+1:n.ageG]
      RD <- Y[14*n.ageG+1:n.ageG]
      
      if(t < time_intervention){
        nInfection<-S*(B1%*%((I.mild+I.hosp+E2)/popSize.ageG))
      } else{
        nInfection<-S*(B2%*%((I.mild+I.hosp+E2)/popSize.ageG))
      }
      
      if(t < time_begin_change){
        p.icu.hosp <- p.icu.hosp.baseline
      } else if (t > time_end_change){
        p.icu.hosp <- alpha*p.icu.hosp.baseline
      } else{
        slope <- 1 + (t - time_begin_change)/(time_end_change - time_begin_change)*(alpha - 1)
        p.icu.hosp <- slope*p.icu.hosp.baseline
      }
      
      dS<- -nInfection
      dE1<- nInfection - g1*E1
      dE2<- g1*E1-g2*E2
      dI.mild <- g2*(1-p.hosp.inf)*E2 - g3*I.mild
      
      dI.hosp <- g2*p.hosp.inf*E2 - g3*I.hosp
      
      dI.bar.hosp <- g3*(1-p.icu.hosp)*I.hosp - g_to_hosp*I.bar.hosp
      dI.bar.ICU <- g3*p.icu.hosp*I.hosp - g_to_hosp_ICU*I.bar.ICU
      
      dH.ICU <- g_to_hosp_ICU*I.bar.ICU - g_to_ICU*H.ICU
      dICU.1 <- g_to_ICU*H.ICU - g_out_ICU*ICU.1
      dICU.2 <- g_out_ICU*ICU.1 - g_out_ICU*ICU.2
      
      dH.1 <- g_to_hosp*I.bar.hosp - g_out_hosp*H.1
      dH.2 <- g_out_hosp*H.1 - g_out_hosp*H.2
      
      dICU.cum <- g_to_ICU*H.ICU
      dH.cum <- g_to_hosp_ICU*I.bar.ICU + g_to_hosp*I.bar.hosp
      dRD <- g3*I.mild + g_out_hosp*H.2 + g_out_ICU*ICU.2
      
      
      list(c(dS, dE1, dE2, dI.mild, dI.hosp,
             dI.bar.hosp, dI.bar.ICU, dH.ICU,
             dICU.1, dICU.2,
             dH.1, dH.2,
             dICU.cum, dH.cum,
             dRD)) 
    })
  }
  ## Initial conditions
  n_inf_init <- get_infectious_init(R0, log_Inf_lockdown, time_intervention)
  n_inf_init_by_age <- n_inf_init*popSize.ageG/popSize
  
  Yini<-c(popSize.ageG - n_inf_init_by_age, # S      -0
          n_inf_init_by_age, # E1     -1
          rep(0, n.ageG), # E2      -2
          rep(0, n.ageG), # I.mild      -3
          rep(0, n.ageG), # I.hosp      -4
          rep(0, n.ageG), # I.bar.hosp      -5
          rep(0, n.ageG), # I.bar.ICU      -6
          rep(0, n.ageG), # H.ICU      -7
          rep(0, n.ageG), # ICU.1      -8
          rep(0, n.ageG), # ICU.2      -9
          rep(0, n.ageG), # H.1      -10
          rep(0, n.ageG), # H.2      -11
          rep(0, n.ageG), # ICU.cum      -12
          rep(0, n.ageG), # H.cum      -13
          rep(0, n.ageG) # RD        -14
  )
  N<-sum(Yini)
  
  ## Running the model
  out <- ode(y = Yini, times = times, func = SEIR.simple, parms = c(time_intervention = time_intervention), atol = 1e-5)
  return(out)
}

# Getting daily ICU admissions from model run
get_daily_ICU_admission_from_output <- function(out){
  ICU.cum <- out[,1+12*n.ageG+1:n.ageG]
  ICU.cum.all <- apply(ICU.cum, 1, sum)
  ICU_adm <- c(0,ICU.cum.all[2:length(ICU.cum.all)] - ICU.cum.all[1:(length(ICU.cum.all) - 1)])
  # Rescale it to daily admissions
  daily_ICU_adm <- sapply(1:length(times_1), FUN = function(i){
    sum(ICU_adm[which(times < times_1[i] + 1 & times >= times_1[i])])
  })
  return(daily_ICU_adm)
}

# Getting daily hospital admission from model run
get_daily_hosp_admission_from_output <- function(out){
  H.cum <- out[,1+13*n.ageG+1:n.ageG]
  H.cum.all <- apply(H.cum, 1, sum)
  H_adm <- c(0,H.cum.all[2:length(H.cum.all)] - H.cum.all[1:(length(H.cum.all) - 1)])
  # Rescale it to daily admissions
  daily_H_adm <- sapply(1:length(times_1), FUN = function(i){
    sum(H_adm[which(times < times_1[i] + 1 & times >= times_1[i])])
  })
  return(daily_H_adm)
}

# Getting ICU beds occupied from model run
get_ICU_occ_from_output <- function(out){
  ICU.1 <- out[,1+8*n.ageG+1:n.ageG]
  ICU.2 <- out[,1+9*n.ageG+1:n.ageG]
  ICU.tot <- ICU.1 + ICU.2
  ICU.tot.all <- apply(ICU.tot, 1, sum)
  daily_ICU_occ <- ICU.tot.all[times %in% times_1]
  return(daily_ICU_occ)
}

# Getting general ward beds occupied from model run
get_hosp_occ_from_output <- function(out){
  H.ICU <- out[,1+7*n.ageG+1:n.ageG]
  H.1 <- out[,1+10*n.ageG+1:n.ageG]
  H.2 <- out[,1+11*n.ageG+1:n.ageG]
  H.tot <- H.1 + H.2 + H.ICU
  H.tot.all <- apply(H.tot, 1, sum)
  daily_H_occ <- H.tot.all[times %in% times_1]
  return(daily_H_occ)
}

# Getting daily new infections from model run
get_daily_new_infections_from_output <- function(out){
  S <- out[,1+1:n.ageG]
  newInfections.noT0<-S[-dim(S)[1],]-S[-1,]
  newInfections<-rbind(rep(0,n.ageG),newInfections.noT0)
  newInfections_all <- apply(newInfections, 1, sum)
  daily_newInfections <- sapply(1:length(times_1), FUN = function(i){
    sum(newInfections_all[which(times < times_1[i] + 1 & times >= times_1[i])])
  })
  return(daily_newInfections)
}

# Getting cumulative number of infections from model run
get_cum_infections_from_output <- function(out){
  S <- out[,1+1:n.ageG]
  newInfections.noT0<-S[-dim(S)[1],]-S[-1,]
  newInfections<-rbind(rep(0,n.ageG),newInfections.noT0)
  cumInf <- cumsum(apply(newInfections, 1, sum))
  daily_cumInf <- cumInf[times %in% times_1]
  return(daily_cumInf)
}


##################################
## 8) Functions used for model calibration
##################################
## Negative binomial density
get_neg_binom_density <- function(observed, predicted, delta){
  log_density <- sapply(1:length(observed), FUN = function(i){
    m <- predicted[i]
    k <- observed[i]
    r <- m^delta
    
    res <- lgamma(r + k) - lgamma(k + 1) - lgamma(r)
    
    if(r < 1e-25 & m < 1e-25){
      res <- res - 1e25
      
    } else if(r < 1e-25){
      res <- res + 0.0
    } else if(m<1e-25){
      res <- res - 1e25
    } else{
      res <- res + r*log(r/(r+m))
      res <- res + k*log(m/(r+m))
    }
    res  
    
  })
  
  sum_log_density <- sum(log_density)
  if(sum_log_density < -1e25){
    sum_log_density <- -1e25
  }
  
  return(sum_log_density)
}

## Regional log-likelihood for a given set of parameters
neg_binom_log_lik_region <- function(R0,
                                     g_out_ICU,
                                     g_out_hosp,
                                     alpha,
                                     R0_after_intervention_region,
                                     log_Inf_lockdown_region,
                                     delta,
                                     name_region){
  
  ## Running model
  out <- output_from_param(R0,
                           g_out_ICU,
                           g_out_hosp,
                           alpha,
                           R0_after_intervention_region,
                           log_Inf_lockdown_region,
                           name_region)
  index_begin_calibration <- as.numeric(date_beginning_calibration - date_intervention) + time_intervention
  
  ## Getting observed data
  daily_ICU_admissions <- mat_daily_ICU[,colnames(mat_daily_ICU) == name_region]
  daily_hosp_admissions <- mat_daily_hosp[,colnames(mat_daily_hosp) == name_region]
  ICU_occ <- mat_ICU_occ[,colnames(mat_ICU_occ) == name_region]
  hosp_occ <- mat_hosp_occ[,colnames(mat_hosp_occ) == name_region]
  
  # ICU admissions
  pred_ICU_adm <- get_daily_ICU_admission_from_output(out)[index_begin_calibration:(index_begin_calibration + length(daily_ICU_admissions) - 1)]
  # Daily deaths
  pred_ICU_occ <- get_ICU_occ_from_output(out)[index_begin_calibration:(index_begin_calibration + length(ICU_occ) - 1)]
  # Hospital admissions
  pred_hosp_adm <- get_daily_hosp_admission_from_output(out)[index_begin_calibration:(index_begin_calibration + length(daily_hosp_admissions) - 1)]
  # Hospital beds
  pred_hosp_occ <- get_hosp_occ_from_output(out)[index_begin_calibration:(index_begin_calibration + length(hosp_occ) - 1)]
  
  LLH <- get_neg_binom_density(observed = daily_ICU_admissions, predicted = pred_ICU_adm, delta = delta) +
    get_neg_binom_density(observed = ICU_occ, predicted = pred_ICU_occ, delta = delta) +
    get_neg_binom_density(observed = daily_hosp_admissions, predicted = pred_hosp_adm, delta = delta) +
    get_neg_binom_density(observed = hosp_occ, predicted = pred_hosp_occ, delta = delta)
  
  return(LLH)
}

## Regional likelihood function
calculateLL_neg_binom_region <- function(param_region, name_region){
  
  R0 <- param_region[1]
  g_out_ICU <- param_region[2]
  g_out_hosp <- param_region[3]
  alpha <- param_region[4]
  R0_after_intervention_region <- param_region[5]
  log_Inf_lockdown_region <- param_region[6]
  delta <- param_region[7]
  
  LLH <- neg_binom_log_lik_region(R0,
                                  g_out_ICU,
                                  g_out_hosp,
                                  alpha,
                                  R0_after_intervention_region,
                                  log_Inf_lockdown_region,
                                  delta,
                                  name_region)
  
  return(LLH)
}

##################################
## 9) MCMC run
##################################
## (A) Parametrization of the MCMC
nbIteration <- 10000
n_cores <- 2
seed <- 1
set.seed(seed)

dir_results <- paste0('Results/', format(date_file, '%Y%m%d'))
ifelse(dir.exists(dir_results), '', dir.create(dir_results))
name_res <- paste0(dir_results, '/MCMC_regions_nbiter_', nbIteration,'_seed_', seed,'.rds')

run_MCMC <- F

if(run_MCMC){
  ## (B) MCMC initialization
  # Random draw of inital values of the parameters
  init_R0 <- runif(n = 1, min = 2, max = 4)
  init_g_out_ICU <- sapply(1:n_regions, FUN = function(i){runif(n = 1, min = 0.1, max = 0.2)})
  init_g_out_hosp <- sapply(1:n_regions, FUN = function(i){runif(n = 1, min = 0.1, max = 0.2)})
  init_R_after_intervention <- runif(n = 1, min = 0.5, max = 2.5)
  init_log_inf_lockdown <- sapply(1:n_regions, FUN = function(i){runif(n = 1, min = log(1e4), max = log(1e5))})
  init_delta <- runif(n = 1, min = 0, max = 2)
  init_alpha <- runif(n = 1, min = 0.5, max = 1)
  
  param <- c(init_R0, init_alpha, init_R_after_intervention, init_delta)
  param_log_inf_lockdown <- init_log_inf_lockdown
  param_g_out_ICU <- init_g_out_ICU
  param_g_out_hosp <- init_g_out_hosp
  
  n_param_national <- length(param) # Number of parameters identical across all regions
  
  ## (C) Defining proposal characteristics
  # Standard deviation of the log-normal proposals
  sdProposal_file <- read_excel('Data/MCMC_param/sd_proposal_regions.xlsx')
  sdProposal_log_inf <- as.numeric(sdProposal_file$sd_log_inf)
  sdProposal_g_out_ICU <- as.numeric(sdProposal_file$sd_g_out_ICU)
  sdProposal_g_out_hosp <- as.numeric(sdProposal_file$sd_g_out_hosp)
  sdProposal_name_reg <- sdProposal_file$region_name
  sdProposal <- c(0.028, 0.10, 0.016, 0.05) # For R0, alpha,  R_lockdown and delta
  
  ## (D) Defining priors characteristics
  inf_prior <- c(0, 0, 0,  0)
  sup_prior <- c(10, 2, 10,  2)
  
  inf_prior_log_inf <- log(1e2)
  inf_prior_g_out_ICU <- 0
  inf_prior_g_out_hosp <- 0
  
  sup_prior_log_inf <- log(1e8)
  sup_prior_g_out_ICU <- 2
  sup_prior_g_out_hosp <- 2
  
  ## (E) Structures to store the results
  logLik<-matrix(0,ncol = n_regions, nrow = nbIteration)
  
  accept_log_inf <- matrix(0, ncol = n_regions, nrow = nbIteration)
  accept_g_out_ICU <- matrix(0, ncol = n_regions, nrow = nbIteration)
  accept_g_out_hosp <- matrix(0, ncol = n_regions, nrow = nbIteration)
  accept_others<-matrix(0,ncol = n_param_national, nrow = nbIteration)
  
  parameters_log_inf <- matrix(0,ncol = n_regions,nrow = nbIteration)
  parameters_g_out_ICU <- matrix(0, ncol = n_regions, nrow = nbIteration)
  parameters_g_out_hosp <- matrix(0, ncol = n_regions, nrow = nbIteration)
  parameters_others <- matrix(0,ncol = n_param_national, nrow = nbIteration)
  
  ## (F) Register cluster
  ## Register cluster
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  ## (G) First iteration
  iteration <- 1
  log_lik_init <- foreach(i = 1:n_regions, .combine = 'c', .packages = 'deSolve') %dopar% {
    tmp_name_region <- vect_name_region[i]
    param_region <- c(param[1],
                      parameters_g_out_ICU[i] , parameters_g_out_hosp[i] ,
                      param[2], 
                      param[3],
                      param_log_inf_lockdown[i],
                      param[4])
    calculateLL_neg_binom_region(param_region, tmp_name_region)
  }
  logLik[iteration,] <- log_lik_init
  
  parameters_others[iteration,]<-param
  parameters_log_inf[iteration,] <- param_log_inf_lockdown
  parameters_g_out_ICU[iteration,] <- param_g_out_ICU
  parameters_g_out_hosp[iteration,] <- param_g_out_hosp
  
  ## (H) Running remaining iterations
  t0 <- Sys.time()
  for(iteration in 2:nbIteration){
    if(iteration %%100 == 0){
      print(paste0('Iteration :', iteration))
    }
    
    parameters_others[iteration,]<-parameters_others[iteration-1,]
    logLik[iteration,]<-logLik[iteration-1,]
    ## R0 and delta update
    
    for(iParam in 1:n_param_national){
      oldParam <- parameters_others[iteration - 1,iParam]
      ## Sampling a new candidate
      newParam <- oldParam*exp(sdProposal[iParam]*rnorm(1))
      
      if(newParam > sup_prior[iParam] || newParam < inf_prior[iParam]){
        parameters_others[iteration,iParam]<-oldParam
      } else{
        parameters_others[iteration,iParam]<-as.numeric(newParam)
        ## Computing the loglik for this new candidate
        vect_newLogLik <- foreach(i = 1:n_regions, .combine = 'c', .packages = 'deSolve') %dopar% {
          tmp_name_region <- vect_name_region[i]
          param_region <- c(parameters_others[iteration,1],
                            parameters_g_out_ICU[iteration - 1,i],
                            parameters_g_out_hosp[iteration - 1,i],
                            parameters_others[iteration, 2],
                            parameters_others[iteration, 3],
                            parameters_log_inf[iteration - 1, i],
                            parameters_others[iteration,4])
          calculateLL_neg_binom_region(param_region, tmp_name_region)
        }
        
        ## Acceptnace/Rejection step
        log_acceptance_ratio <- sum(vect_newLogLik)-sum(logLik[iteration,])+log(newParam)-log(oldParam)
        
        if(log(runif(1))<log_acceptance_ratio){
          logLik[iteration,]<-vect_newLogLik
          accept_others[iteration,iParam]<-1
        } else{
          parameters_others[iteration,iParam]<-oldParam
        }
      }
    } ## End of update R0 and delta
    
    
    
    tmp_res <- foreach(iRegion=1:n_regions, .combine = 'cbind', .packages = 'deSolve') %dopar% {
      tmp_name_region <- vect_name_region[iRegion]
      
      oldLogLik <- logLik[iteration,iRegion]
      
      ## Update g_out_ICU
      oldParam <- parameters_g_out_ICU[iteration - 1, iRegion]
      newParam_g_out_ICU <- oldParam*exp(sdProposal_g_out_ICU[sdProposal_name_reg == tmp_name_region]*rnorm(1))
      
      if(newParam_g_out_ICU > sup_prior_g_out_ICU || newParam_g_out_ICU < inf_prior_g_out_ICU){
        newParam_g_out_ICU<-oldParam
        accept_g_out_ICU <- 0
        newParam_g_out_ICU <- oldParam
      } else{
        ## Computing the loglik for this new candidate
        newLogLik <- calculateLL_neg_binom_region(c(parameters_others[iteration, 1],
                                                    newParam_g_out_ICU,
                                                    parameters_g_out_hosp[iteration - 1, iRegion],
                                                    parameters_others[iteration, 2],
                                                    parameters_others[iteration, 3],
                                                    parameters_log_inf[iteration - 1, iRegion],
                                                    parameters_others[iteration, 4]),
                                                  tmp_name_region)
        
        
        ## Acceptnace/Rejection step
        log_acceptance_ratio <- newLogLik - oldLogLik + log(newParam_g_out_ICU)-log(oldParam)
        
        if(log(runif(1))<log_acceptance_ratio){
          ## Accept
          accept_g_out_ICU <- 1
          oldLogLik <- newLogLik
        } else{
          ## Reject
          accept_g_out_ICU <- 0
          newParam_g_out_ICU <- oldParam
        }
      }
      
      ## Update g_out_hosp
      oldParam <- parameters_g_out_hosp[iteration - 1, iRegion]
      newParam_g_out_hosp <- oldParam*exp(sdProposal_g_out_hosp[sdProposal_name_reg == tmp_name_region]*rnorm(1))
      if(newParam_g_out_hosp > sup_prior_g_out_hosp || newParam_g_out_hosp < inf_prior_g_out_hosp){
        newParam_g_out_hosp<-oldParam
        accept_g_out_hosp <- 0
        newParam_g_out_hosp <- oldParam
      } else{
        ## Computing the loglik for this new candidate
        newLogLik <- calculateLL_neg_binom_region(c(parameters_others[iteration, 1],
                                                    newParam_g_out_ICU,
                                                    newParam_g_out_hosp,
                                                    parameters_others[iteration, 2],
                                                    parameters_others[iteration, 3],
                                                    parameters_log_inf[iteration - 1, iRegion],
                                                    parameters_others[iteration, 4]),
                                                  tmp_name_region)
        
        ## Acceptance/Rejection step
        log_acceptance_ratio <- newLogLik - oldLogLik + log(newParam_g_out_hosp)-log(oldParam)
        
        if(log(runif(1))<log_acceptance_ratio){
          ## Accept
          accept_g_out_hosp <- 1
          oldLogLik <- newLogLik
        } else{
          ## Reject
          accept_g_out_hosp <- 0
          newParam_g_out_hosp <- oldParam
        }
      }
      
      
      
      ## Update log_inf_lockdown
      oldParam <- parameters_log_inf[iteration - 1, iRegion]
      newParam_loginf <- oldParam*exp(sdProposal_log_inf[sdProposal_name_reg == tmp_name_region]*rnorm(1))
      if(newParam_loginf > sup_prior_log_inf || newParam_loginf < inf_prior_log_inf){
        newParam_loginf<-oldParam
        accept_log_inf <- 0
        newParam_loginf <- oldParam
        newLogLik <- oldLogLik
      } else{
        ## Computing the loglik for this new candidate
        newLogLik <- calculateLL_neg_binom_region(c(parameters_others[iteration, 1],
                                                    newParam_g_out_ICU,
                                                    newParam_g_out_hosp,
                                                    parameters_others[iteration, 2],
                                                    parameters_others[iteration, 3],
                                                    newParam_loginf,
                                                    parameters_others[iteration, 4]),
                                                  tmp_name_region)
        
        ## Acceptnace/Rejection step
        log_acceptance_ratio <- newLogLik - oldLogLik + log(newParam_loginf)-log(oldParam)
        
        if(log(runif(1))<log_acceptance_ratio){
          ## Accept
          accept_log_inf <- 1
        } else{
          ## Reject
          accept_log_inf <- 0
          newParam_loginf <- oldParam
          newLogLik <- oldLogLik
        }
      }
      c(newLogLik,
        accept_g_out_ICU,
        accept_g_out_hosp,
        accept_log_inf,
        newParam_g_out_ICU,
        newParam_g_out_hosp,
        newParam_loginf)
    }
    
    logLik[iteration,] <- tmp_res[1,]
    
    accept_g_out_ICU[iteration,] <- tmp_res[2,]
    accept_g_out_hosp[iteration,] <- tmp_res[3,]
    accept_log_inf[iteration,] <- tmp_res[4,]
    
    parameters_g_out_ICU[iteration,] <- tmp_res[5,]
    parameters_g_out_hosp[iteration,] <- tmp_res[6,]
    parameters_log_inf[iteration,] <- tmp_res[7,]
    
    
  }
  t1 <- Sys.time()
  
  print(t1 - t0)
  
  
  list_res <- list(logLik = logLik,
                   parameters_others = parameters_others,
                   parameters_log_inf = parameters_log_inf,
                   parameters_g_out_ICU = parameters_g_out_ICU,
                   parameters_g_out_hosp = parameters_g_out_hosp,
                   accept_others = accept_others,
                   accept_log_inf = accept_log_inf,
                   accept_g_out_ICU = accept_g_out_ICU,
                   accept_g_out_hosp = accept_g_out_hosp)
  
  print(name_res)
  saveRDS(list_res, file = name_res)
  
  
}


##################################
## 10) Analyzing MCMC runs
## Functions allow to look at the results of 4 chains with different seeds simultaneously
##################################
BI <- 2000 # Burn-in period
list_res <- readRDS(name_res)
source('Scripts/functions_results_regions.R')

MyPal <- brewer.pal(8, 'Spectral')

## Trace plots
plot_chains(list_res, list_res, list_res, list_res, BI)

## Acceptance rates
acc_rates(list_res, list_res, list_res, list_res, BI)

## Posterior densities
posterior_densities(list_res, list_res, list_res, list_res, BI)


##################################
## 11) Simulation from the posterior
##################################
## Running simulation from the posterior distribution
n_simul_1 <- 2
n_simul_2 <- 10
list_res_simul_regions <- simul_from_posterior(list_res, BI, n_simul_1, n_simul_2, seed = 1)

tmp_pred_daily_ICU_adm_list <- list_res_simul_regions$pred_daily_ICU_adm_list
tmp_pred_ICU_occ_list <- list_res_simul_regions$pred_ICU_occ_list
tmp_pred_daily_hosp_adm_list <- list_res_simul_regions$pred_daily_hosp_adm_list
tmp_pred_hosp_occ_list <- list_res_simul_regions$pred_hosp_occ_list
tmp_pred_cum_inf_list <- list_res_simul_regions$pred_cum_inf_list
tmp_pred_daily_new_inf_list <- list_res_simul_regions$pred_daily_new_inf_list

dir_results_simul <- paste0('Results/', format(date_file, '%Y%m%d'), '/Simul_regions')
ifelse(dir.exists(dir_results_simul), '', dir.create(dir_results_simul))
name_res_simul <- paste0(dir_results_simul, '/simul_baseline_')
saveRDS(tmp_pred_daily_ICU_adm_list, paste0(name_res_simul, 'pred_daily_ICU_adm_list.rds'))
saveRDS(tmp_pred_ICU_occ_list, paste0(name_res_simul, 'pred_ICU_occ_list.rds'))
saveRDS(tmp_pred_daily_hosp_adm_list, paste0(name_res_simul, 'pred_daily_hosp_adm_list.rds'))
saveRDS(tmp_pred_hosp_occ_list, paste0(name_res_simul,'pred_hosp_occ_list.rds'))
saveRDS(tmp_pred_daily_new_inf_list, paste0(name_res_simul,'pred_daily_new_inf_list.rds'))
saveRDS(tmp_pred_cum_inf_list, paste0(name_res_simul,'pred_cum_inf_list.rds'))



##################################
## 12) Plotting simulation (hospital trajectories and infections) for the french regions
##################################
par(mfrow = c(2,3))
par(mar = c(3,5,2,1))
for(iRegion in 1:13){
  plot_ICU_hosp_inf(tmp_pred_daily_ICU_adm_list, tmp_pred_ICU_occ_list,
                    tmp_pred_daily_hosp_adm_list, tmp_pred_hosp_occ_list,
                    tmp_pred_cum_inf_list,
                    tmp_pred_daily_new_inf_list,
                    iRegion)
  
}
