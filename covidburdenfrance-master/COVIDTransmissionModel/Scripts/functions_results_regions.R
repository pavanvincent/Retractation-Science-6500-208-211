## Trace plots of the chains
plot_chains <- function(list_res_1, list_res_2, list_res_3, list_res_4, BI, title = ''){
  par(mar = c(4,5,2,1))
  
  parameters_others_1 <- list_res_1$parameters_others
  parameters_others_2 <- list_res_2$parameters_others
  parameters_others_3 <- list_res_3$parameters_others
  parameters_others_4 <- list_res_4$parameters_others
  
  parameters_log_inf_1 <- list_res_1$parameters_log_inf
  parameters_log_inf_2 <- list_res_2$parameters_log_inf
  parameters_log_inf_3 <- list_res_3$parameters_log_inf
  parameters_log_inf_4 <- list_res_4$parameters_log_inf
  
  parameters_g_out_ICU_1 <- list_res_1$parameters_g_out_ICU
  parameters_g_out_ICU_2 <- list_res_2$parameters_g_out_ICU
  parameters_g_out_ICU_3 <- list_res_3$parameters_g_out_ICU
  parameters_g_out_ICU_4 <- list_res_4$parameters_g_out_ICU
  
  parameters_g_out_hosp_1 <- list_res_1$parameters_g_out_hosp
  parameters_g_out_hosp_2 <- list_res_2$parameters_g_out_hosp
  parameters_g_out_hosp_3 <- list_res_3$parameters_g_out_hosp
  parameters_g_out_hosp_4 <- list_res_4$parameters_g_out_hosp
  
  
  logLik_1 <- list_res_1$logLik
  logLik_2 <- list_res_2$logLik
  logLik_3 <- list_res_3$logLik
  logLik_4 <- list_res_4$logLik
  
  logLik_all_1 <- apply(logLik_1, 1, sum)
  logLik_all_2 <- apply(logLik_2, 1, sum)
  logLik_all_3 <- apply(logLik_3, 1, sum)
  logLik_all_4 <- apply(logLik_4, 1, sum)
  
  n_regions <- ncol(parameters_log_inf_1)
  burnIn <- BI
  ## Convergence of log_inf_regions param
  par(mfrow = c(3,5))
  param_names <- paste0('log_inf_', vect_name_region)
  for(parID in 1:n_regions) {
    y_max <- max(c(parameters_log_inf_1[-(1:burnIn),parID], parameters_log_inf_2[-(1:burnIn),parID], parameters_log_inf_3[-(1:burnIn),parID], parameters_log_inf_4[-(1:burnIn),parID]))
    y_min <- min(c(parameters_log_inf_1[-(1:burnIn),parID], parameters_log_inf_2[-(1:burnIn),parID], parameters_log_inf_3[-(1:burnIn),parID], parameters_log_inf_4[-(1:burnIn),parID]))
    plot(parameters_log_inf_1[-(1:burnIn),parID], type = 'l', ylab = param_names[parID],
         xlab = expression(n[iter]), main = param_names[parID],
         ylim = c(y_min, y_max), col = MyPal[1])
    lines(parameters_log_inf_2[-(1:burnIn),parID], col = MyPal[2])
    lines(parameters_log_inf_3[-(1:burnIn),parID], col = MyPal[3])
    lines(parameters_log_inf_4[-(1:burnIn),parID], col = MyPal[4])
    
  }
  
  par(mfrow = c(3,5))
  param_names <- paste0('g_out_ICU_', vect_name_region)
  for(parID in 1:n_regions) {
    y_max <- max(c(parameters_g_out_ICU_1[-(1:burnIn),parID], parameters_g_out_ICU_2[-(1:burnIn),parID], parameters_g_out_ICU_3[-(1:burnIn),parID], parameters_g_out_ICU_4[-(1:burnIn),parID]))
    y_min <- min(c(parameters_g_out_ICU_1[-(1:burnIn),parID], parameters_g_out_ICU_2[-(1:burnIn),parID], parameters_g_out_ICU_3[-(1:burnIn),parID], parameters_g_out_ICU_4[-(1:burnIn),parID]))
    plot(parameters_g_out_ICU_1[-(1:burnIn),parID], type = 'l', ylab = param_names[parID],
         xlab = expression(n[iter]), main = param_names[parID],
         ylim = c(y_min, y_max), col = MyPal[1])
    lines(parameters_g_out_ICU_2[-(1:burnIn),parID], col = MyPal[2])
    lines(parameters_g_out_ICU_3[-(1:burnIn),parID], col = MyPal[3])
    lines(parameters_g_out_ICU_4[-(1:burnIn),parID], col = MyPal[4])
    
  }
  
  par(mfrow = c(3,5))
  param_names <- paste0('g_out_hosp_', vect_name_region)
  for(parID in 1:n_regions) {
    y_max <- max(c(parameters_g_out_hosp_1[-(1:burnIn),parID], parameters_g_out_hosp_2[-(1:burnIn),parID], parameters_g_out_hosp_3[-(1:burnIn),parID], parameters_g_out_hosp_4[-(1:burnIn),parID]))
    y_min <- min(c(parameters_g_out_hosp_1[-(1:burnIn),parID], parameters_g_out_hosp_2[-(1:burnIn),parID], parameters_g_out_hosp_3[-(1:burnIn),parID], parameters_g_out_hosp_4[-(1:burnIn),parID]))
    plot(parameters_g_out_hosp_1[-(1:burnIn),parID], type = 'l', ylab = param_names[parID],
         xlab = expression(n[iter]), main = param_names[parID],
         ylim = c(y_min, y_max), col = MyPal[1])
    lines(parameters_g_out_hosp_2[-(1:burnIn),parID], col = MyPal[2])
    lines(parameters_g_out_hosp_3[-(1:burnIn),parID], col = MyPal[3])
    lines(parameters_g_out_hosp_4[-(1:burnIn),parID], col = MyPal[4])
    
  }
  
  
  par(mfrow = c(3,5))
  param_names <- paste0('Log_Lik_', seq(1, 13))
  for(parID in 1:n_regions) {
    y_max <- max(c(logLik_1[-(1:burnIn),parID], logLik_2[-(1:burnIn),parID], logLik_3[-(1:burnIn),parID], logLik_4[-(1:burnIn),parID]))
    y_min <- min(c(logLik_1[-(1:burnIn),parID], logLik_2[-(1:burnIn),parID], logLik_3[-(1:burnIn),parID], logLik_4[-(1:burnIn),parID]))
    plot(logLik_1[-(1:burnIn),parID], type = 'l', ylab = param_names[parID],
         xlab = expression(n[iter]), main = param_names[parID],
         ylim = c(y_min, y_max), col = MyPal[1])
    lines(logLik_2[-(1:burnIn),parID], col = MyPal[2])
    lines(logLik_3[-(1:burnIn),parID], col = MyPal[3])
    lines(logLik_4[-(1:burnIn),parID], col = MyPal[4])
    
  }
  
  par(mfrow = c(2,2))
  param_names <- c('R0', 'alpha', 'R_conf', 'delta')
  for(parID in 1:ncol(parameters_others_1)) {
    y_max <- max(c(parameters_others_1[-(1:burnIn),parID], parameters_others_2[-(1:burnIn),parID], parameters_others_3[-(1:burnIn),parID], parameters_others_4[-(1:burnIn),parID]))
    y_min <- min(c(parameters_others_1[-(1:burnIn),parID], parameters_others_2[-(1:burnIn),parID], parameters_others_3[-(1:burnIn),parID], parameters_others_4[-(1:burnIn),parID]))
    plot(parameters_others_1[-(1:burnIn),parID], type = 'l', ylab = param_names[parID],
         xlab = expression(n[iter]), main = param_names[parID],
         ylim = c(y_min, y_max), col = MyPal[1])
    lines(parameters_others_2[-(1:burnIn),parID], col = MyPal[2])
    lines(parameters_others_3[-(1:burnIn),parID], col = MyPal[3])
    lines(parameters_others_4[-(1:burnIn),parID], col = MyPal[4])
    
  }
  
  y_max <- max(c(logLik_all_1[-(1:burnIn)], logLik_all_2[-(1:burnIn)], logLik_all_3[-(1:burnIn)], logLik_all_4[-(1:burnIn)]))
  y_min <- min(c(logLik_all_1[-(1:burnIn)], logLik_all_2[-(1:burnIn)], logLik_all_3[-(1:burnIn)], logLik_all_4[-(1:burnIn)]))
  plot(logLik_all_1[-(1:burnIn)],main="Log-Lik", type = 'l', 
       ylab = 'Log-Lik',
       col = MyPal[1], ylim = c(y_min, y_max))
  lines(logLik_all_2[-(1:burnIn)], col = MyPal[2])
  lines(logLik_all_3[-(1:burnIn)], col = MyPal[3])
  lines(logLik_all_3[-(1:burnIn)], col = MyPal[4])
  legend('bottomright', col = NA, legend = title, bty = 'n')
  
}

## Acceptance rates
acc_rates <- function(list_res_1, list_res_2, list_res_3, list_res_4, BI){
  acc_rate_others_1 <- list_res_1$accept_others[-(1:BI),]
  acc_rate_others_2 <- list_res_2$accept_others[-(1:BI),]
  acc_rate_others_3 <- list_res_3$accept_others[-(1:BI),]
  acc_rate_others_4 <- list_res_4$accept_others[-(1:BI),]
  
  acc_rate_log_inf_1 <- list_res_1$accept_log_inf[-(1:BI),]
  acc_rate_log_inf_2 <- list_res_2$accept_log_inf[-(1:BI),]
  acc_rate_log_inf_3 <- list_res_3$accept_log_inf[-(1:BI),]
  acc_rate_log_inf_4 <- list_res_4$accept_log_inf[-(1:BI),]
  
  
  acc_rate_g_out_ICU_1 <- list_res_1$accept_g_out_ICU[-(1:BI),]
  acc_rate_g_out_ICU_2 <- list_res_2$accept_g_out_ICU[-(1:BI),]
  acc_rate_g_out_ICU_3 <- list_res_3$accept_g_out_ICU[-(1:BI),]
  acc_rate_g_out_ICU_4 <- list_res_4$accept_g_out_ICU[-(1:BI),]
  
  acc_rate_g_out_hosp_1 <- list_res_1$accept_g_out_hosp[-(1:BI),]
  acc_rate_g_out_hosp_2 <- list_res_2$accept_g_out_hosp[-(1:BI),]
  acc_rate_g_out_hosp_3 <- list_res_3$accept_g_out_hosp[-(1:BI),]
  acc_rate_g_out_hosp_4 <- list_res_4$accept_g_out_hosp[-(1:BI),]
  
  
  n_regions <- ncol(list_res_1$accept_log_inf)
  
  
  AR_others_1 <- apply(acc_rate_others_1, 2, sum)/nrow(acc_rate_others_1)
  AR_others_2 <- apply(acc_rate_others_2, 2, sum)/nrow(acc_rate_others_2)
  AR_others_3 <- apply(acc_rate_others_3, 2, sum)/nrow(acc_rate_others_3)
  AR_others_4 <- apply(acc_rate_others_4, 2, sum)/nrow(acc_rate_others_4)
  
  AR_log_inf_1 <- apply(acc_rate_log_inf_1, 2, sum)/nrow(acc_rate_log_inf_1)
  AR_log_inf_2 <- apply(acc_rate_log_inf_2, 2, sum)/nrow(acc_rate_log_inf_2)
  AR_log_inf_3 <- apply(acc_rate_log_inf_3, 2, sum)/nrow(acc_rate_log_inf_3)
  AR_log_inf_4 <- apply(acc_rate_log_inf_4, 2, sum)/nrow(acc_rate_log_inf_4)
  
  
  AR_g_out_ICU_1 <- apply(acc_rate_g_out_ICU_1, 2, sum)/nrow(acc_rate_g_out_ICU_1)
  AR_g_out_ICU_2 <- apply(acc_rate_g_out_ICU_2, 2, sum)/nrow(acc_rate_g_out_ICU_2)
  AR_g_out_ICU_3 <- apply(acc_rate_g_out_ICU_3, 2, sum)/nrow(acc_rate_g_out_ICU_3)
  AR_g_out_ICU_4 <- apply(acc_rate_g_out_ICU_4, 2, sum)/nrow(acc_rate_g_out_ICU_4)
  
  AR_g_out_hosp_1 <- apply(acc_rate_g_out_hosp_1, 2, sum)/nrow(acc_rate_g_out_hosp_1)
  AR_g_out_hosp_2 <- apply(acc_rate_g_out_hosp_2, 2, sum)/nrow(acc_rate_g_out_hosp_2)
  AR_g_out_hosp_3 <- apply(acc_rate_g_out_hosp_3, 2, sum)/nrow(acc_rate_g_out_hosp_3)
  AR_g_out_hosp_4 <- apply(acc_rate_g_out_hosp_4, 2, sum)/nrow(acc_rate_g_out_hosp_4)
  
  acc_rate_tot <- rbind(c(AR_others_1, AR_log_inf_1, AR_g_out_ICU_1, AR_g_out_hosp_1),
                        c(AR_others_2, AR_log_inf_2, AR_g_out_ICU_2, AR_g_out_hosp_2),
                        c(AR_others_3, AR_log_inf_3, AR_g_out_ICU_3, AR_g_out_hosp_3),
                        c(AR_others_4, AR_log_inf_4, AR_g_out_ICU_4, AR_g_out_hosp_4))
  
  rownames(acc_rate_tot) <- seq(1,4)
  colnames(acc_rate_tot) <- c('R0', 'alpha', 'R_after_int', 'delta',
                              paste0('log_inf_', vect_name_region),
                              paste0('g_out_ICU_', vect_name_region),
                              paste0('g_out_hosp_', vect_name_region))
  return(acc_rate_tot)
}

## Posterior densities
posterior_densities <- function(list_res_1, list_res_2, list_res_3, list_res_4, BI){
  
  parameters_others_1 <- list_res_1$parameters_others
  parameters_others_2 <- list_res_2$parameters_others
  parameters_others_3 <- list_res_3$parameters_others
  parameters_others_4 <- list_res_4$parameters_others
  
  parameters_log_inf_1 <- list_res_1$parameters_log_inf
  parameters_log_inf_2 <- list_res_2$parameters_log_inf
  parameters_log_inf_3 <- list_res_3$parameters_log_inf
  parameters_log_inf_4 <- list_res_4$parameters_log_inf
  
  parameters_g_out_ICU_1 <- list_res_1$parameters_g_out_ICU
  parameters_g_out_ICU_2 <- list_res_2$parameters_g_out_ICU
  parameters_g_out_ICU_3 <- list_res_3$parameters_g_out_ICU
  parameters_g_out_ICU_4 <- list_res_4$parameters_g_out_ICU
  
  parameters_g_out_hosp_1 <- list_res_1$parameters_g_out_hosp
  parameters_g_out_hosp_2 <- list_res_2$parameters_g_out_hosp
  parameters_g_out_hosp_3 <- list_res_3$parameters_g_out_hosp
  parameters_g_out_hosp_4 <- list_res_4$parameters_g_out_hosp
  
  
  logLik_1 <- list_res_1$logLik
  logLik_2 <- list_res_2$logLik
  logLik_3 <- list_res_3$logLik
  logLik_4 <- list_res_4$logLik
  
  
  
  n_regions <- ncol(parameters_log_inf_1)
  burnIn <- BI
  
  ## Convergence of log_inf_regions param
  par(mfrow = c(3,5))
  param_names <- paste0('log_inf_', seq(1, 13))
  for(parID in 1:n_regions) {
    density_1 <- density(parameters_log_inf_1[-(1:burnIn),parID])
    density_2 <- density(parameters_log_inf_2[-(1:burnIn),parID])
    density_3 <- density(parameters_log_inf_3[-(1:burnIn),parID])
    density_4 <- density(parameters_log_inf_4[-(1:burnIn),parID])
    x_min <- min(density_1$x, density_2$x, density_3$x, density_4$x)
    x_max <- max(density_1$x, density_2$x, density_3$x, density_4$x)
    y_min <- min(density_1$y, density_2$y, density_3$y, density_4$y)
    y_max <- max(density_1$y, density_2$y, density_3$y, density_4$y)
    plot(density_1, type = 'l', ylab = 'Density',
         xlab = param_names[parID], main = param_names[parID],
         ylim = c(y_min, y_max), xlim = c(x_min, x_max),col = MyPal[1])
    lines(density_2, col = MyPal[2])
    lines(density_3, col = MyPal[3])
    lines(density_4, col = MyPal[4])
  }
  
  par(mfrow = c(3,5))
  param_names <- paste0('g_out_ICU_', seq(1, 13))
  for(parID in 1:n_regions) {
    density_1 <- density(parameters_g_out_ICU_1[-(1:burnIn),parID])
    density_2 <- density(parameters_g_out_ICU_2[-(1:burnIn),parID])
    density_3 <- density(parameters_g_out_ICU_3[-(1:burnIn),parID])
    density_4 <- density(parameters_g_out_ICU_4[-(1:burnIn),parID])
    x_min <- min(density(parameters_g_out_ICU_1[-(1:burnIn),parID])$x, density(parameters_g_out_ICU_2[-(1:burnIn),parID])$x, density(parameters_g_out_ICU_3[-(1:burnIn),parID])$x, density(parameters_g_out_ICU_4[-(1:burnIn),parID])$x)
    x_max <- max(density(parameters_g_out_ICU_1[-(1:burnIn),parID])$x, density(parameters_g_out_ICU_2[-(1:burnIn),parID])$x, density(parameters_g_out_ICU_3[-(1:burnIn),parID])$x, density(parameters_g_out_ICU_4[-(1:burnIn),parID])$x)
    y_min <- min(density(parameters_g_out_ICU_1[-(1:burnIn),parID])$y, density(parameters_g_out_ICU_2[-(1:burnIn),parID])$y, density(parameters_g_out_ICU_3[-(1:burnIn),parID])$y, density(parameters_g_out_ICU_4[-(1:burnIn),parID])$y)
    y_max <- max(density(parameters_g_out_ICU_1[-(1:burnIn),parID])$y, density(parameters_g_out_ICU_2[-(1:burnIn),parID])$y, density(parameters_g_out_ICU_3[-(1:burnIn),parID])$y, density(parameters_g_out_ICU_4[-(1:burnIn),parID])$y)
    
    plot(density_1, type = 'l', ylab = 'Density',
         xlab = param_names[parID], main = param_names[parID],
         ylim = c(y_min, y_max), xlim = c(x_min, x_max),col = MyPal[1])
    lines(density_2, col = MyPal[2])
    lines(density_3, col = MyPal[3])
    lines(density_4, col = MyPal[4])
  }
  
  par(mfrow = c(3,5))
  param_names <- paste0('g_out_hosp_', seq(1, 13))
  for(parID in 1:n_regions) {
    density_1 <- density(parameters_g_out_hosp_1[-(1:burnIn),parID])
    density_2 <- density(parameters_g_out_hosp_2[-(1:burnIn),parID])
    density_3 <- density(parameters_g_out_hosp_3[-(1:burnIn),parID])
    density_4 <- density(parameters_g_out_hosp_4[-(1:burnIn),parID])
    x_min <- min(density_1$x, density_2$x, density_3$x, density_4$x)
    x_max <- max(density_1$x, density_2$x, density_3$x, density_4$x)
    y_min <- min(density_1$y, density_2$y, density_3$y, density_4$y)
    y_max <- max(density_1$y, density_2$y, density_3$y, density_4$y)
    plot(density_1, type = 'l', ylab = 'Density',
         xlab = param_names[parID], main = param_names[parID],
         ylim = c(y_min, y_max), xlim = c(x_min, x_max),col = MyPal[1])
    lines(density_2, col = MyPal[2])
    lines(density_3, col = MyPal[3])
    lines(density_4, col = MyPal[4])
  }
  
  par(mfrow = c(1,2))
  param_names <- c('R0', 'alpha', 'R_after_int', 'delta')
  for(parID in 1:ncol(parameters_others_1)) {
    density_1 <- density(parameters_others_1[-(1:burnIn),parID])
    density_2 <- density(parameters_others_2[-(1:burnIn),parID])
    density_3 <- density(parameters_others_3[-(1:burnIn),parID])
    density_4 <- density(parameters_others_4[-(1:burnIn),parID])
    x_min <- min(density_1$x, density_2$x, density_3$x, density_4$x)
    x_max <- max(density_1$x, density_2$x, density_3$x, density_4$x)
    y_min <- min(density_1$y, density_2$y, density_3$y, density_4$y)
    y_max <- max(density_1$y, density_2$y, density_3$y, density_4$y)
    plot(density_1, type = 'l', ylab = 'Density',
         xlab = param_names[parID], main = param_names[parID],
         ylim = c(y_min, y_max), xlim = c(x_min, x_max),col = MyPal[1])
    lines(density_2, col = MyPal[2])
    lines(density_3, col = MyPal[3])
    lines(density_4, col = MyPal[4])
    
  }
  
  
}

## Simulation from posterior with scenarios
simul_from_posterior <- function(list_res, BI, n_simul_1, n_simul_2, 
                                 seed = 1){
  ## Defining burnIn
  burnIn <- BI
  ## Getting posterior distribution with BI
  parameters_others <- list_res$parameters_others[-(1:burnIn),]
  parameters_log_inf <- list_res$parameters_log_inf[-(1:burnIn),]
  parameters_g_out_ICU <- list_res$parameters_g_out_ICU[-(1:burnIn),]
  parameters_g_out_hosp <- list_res$parameters_g_out_hosp[-(1:burnIn),]
  
  ## Number of regions
  n_regions <- ncol(parameters_log_inf)
  
  ## Seeding the RNG
  set.seed(seed)
  ## Sampling n_simul_1 indices
  index_sample <- sample.int(n = nrow(parameters_others), size = n_simul_1)
  
  ## Sampling for each region n_simul_1 'baselines' that will be used
  ## as average of the negative binomial
  list_all <- lapply(1:n_regions, FUN = function(iRegion){
    name_region <- vect_name_region[iRegion]
    
    list_reg <- lapply(1:n_simul_1, FUN = function(iIndex){
      index <- index_sample[iIndex]
      tmp_param_others <- parameters_others[index,]
      tmp_g_out_ICU <- parameters_g_out_ICU[index, iRegion]
      tmp_g_out_hosp <- parameters_g_out_hosp[index, iRegion]
      tmp_log_inf <- parameters_log_inf[index, iRegion]
      tmp_out <- output_from_param(tmp_param_others[1], tmp_g_out_ICU, tmp_g_out_hosp,
                                   tmp_param_others[2], tmp_param_others[3],
                                   tmp_log_inf, name_region)
      tmp_out
    })
    list_reg
  })
  names(list_all) <- vect_name_region
  
  ## For each of these n_simul_1 baselines, compute daily ICU adm
  pred_daily_ICU_adm_list <- lapply(1:n_regions, FUN = function(iRegion){
    out_list <- list_all[[iRegion]]
    lapply(1:length(out_list), FUN = function(i){
      tmp_out <- out_list[[i]]
      tmp_ICU_adm <- get_daily_ICU_admission_from_output(tmp_out)
      tmp_delta <- parameters_others[index_sample[i], 4]
      sapply(tmp_ICU_adm, FUN = function(X){
        if(X == 0){
          rep(0, n_simul_2)
        } else{
          rnbinom(n = n_simul_2, size = X^tmp_delta, mu = X)
        }
      })
    })
  })
  names(pred_daily_ICU_adm_list) <- vect_name_region
  
  ## For each of these n_simul_1 baselines, compute ICU beds
  pred_ICU_occ_list <- lapply(1:n_regions, FUN = function(iRegion){
    out_list <- list_all[[iRegion]]
    lapply(1:length(out_list), FUN = function(i){
      tmp_out <- out_list[[i]]
      tmp_ICU <- get_ICU_occ_from_output(tmp_out)
      tmp_delta <- parameters_others[index_sample[i], 4]
      sapply(tmp_ICU, FUN = function(X){
        if(X == 0){
          rep(0, n_simul_2)
        } else{
          rnbinom(n = n_simul_2, size = X^tmp_delta, mu = X)
        }
      })
    })
  })
  names(pred_ICU_occ_list) <- vect_name_region
  
  ## For each of these n_simul_1 baselines, compute daily hosp adm
  pred_daily_hosp_adm_list <- lapply(1:n_regions, FUN = function(iRegion){
    out_list <- list_all[[iRegion]]
    lapply(1:length(out_list), FUN = function(i){
      tmp_out <- out_list[[i]]
      tmp_ICU_adm <- get_daily_hosp_admission_from_output(tmp_out)
      tmp_delta <- parameters_others[index_sample[i], 4]
      sapply(tmp_ICU_adm, FUN = function(X){
        if(X == 0){
          rep(0, n_simul_2)
        } else{
          rnbinom(n = n_simul_2, size = X^tmp_delta, mu = X)
        }
      })
    })
  })
  names(pred_daily_hosp_adm_list) <- vect_name_region
  
  ## For each of these n_simul_1 baselines, compute ICU beds
  pred_hosp_occ_list <- lapply(1:n_regions, FUN = function(iRegion){
    out_list <- list_all[[iRegion]]
    lapply(1:length(out_list), FUN = function(i){
      tmp_out <- out_list[[i]]
      tmp_ICU <- get_hosp_occ_from_output(tmp_out)
      tmp_delta <- parameters_others[index_sample[i], 4]
      sapply(tmp_ICU, FUN = function(X){
        if(X == 0){
          rep(0, n_simul_2)
        } else{
          rnbinom(n = n_simul_2, size = X^tmp_delta, mu = X)
        }
      })
    })
  })
  names(pred_hosp_occ_list) <- vect_name_region
  
  ## For each of these n_simul_1, compute daily_new_infections
  pred_daily_new_inf_list <- lapply(1:n_regions, FUN = function(iRegion){
    out_list <- list_all[[iRegion]]
    lapply(1:length(out_list), FUN = function(i){
      tmp_out <- out_list[[i]]
      tmp_ICU <- get_daily_new_infections_from_output(tmp_out)
    })
  })
  names(pred_daily_new_inf_list) <- vect_name_region
  
  ## For each of these n_simul_1, compute cumulative number of infections
  pred_cum_inf_list <- lapply(1:n_regions, FUN = function(iRegion){
    out_list <- list_all[[iRegion]]
    lapply(1:length(out_list), FUN = function(i){
      tmp_out <- out_list[[i]]
      tmp_ICU <- get_cum_infections_from_output(tmp_out)
    })
  })
  names(pred_cum_inf_list) <- vect_name_region
  
  
  
  list_res_to_save <- list(pred_daily_ICU_adm_list = pred_daily_ICU_adm_list,
                           pred_ICU_occ_list = pred_ICU_occ_list,
                           pred_daily_new_inf_list = pred_daily_new_inf_list,
                           pred_cum_inf_list = pred_cum_inf_list,
                           pred_daily_hosp_adm_list = pred_daily_hosp_adm_list,
                           pred_hosp_occ_list = pred_hosp_occ_list)
  
  
  return(list_res_to_save)
}

## Plot results of the simulation
plot_ICU_hosp_inf <- function(list_res_pred_ICU_adm_2, list_res_pred_ICU_occ_2,
                              list_res_pred_hosp_adm_2, list_res_pred_hosp_occ_2,
                              list_res_pred_cum_inf_2, list_res_pred_new_inf_2,
                              iRegion){
  ## Dates for plot
  date_min <- date_intervention - time_intervention
  date_max <- date_min + 110
  seq_date <- seq(1, date_max - date_min + 1)
  
  index_begin_calibration <- as.numeric(date_beginning_calibration - date_min) + 1
  labels_for_plot <- format(as.Date(seq(0, length(seq_date) - 1), origin = date_min), '%d/%m')
  
  name_region <- vect_name_region[iRegion]
  popSize <- sum(pop_region[,colnames(pop_region) == name_region])
  
  ## Get predictions for the region iRegion
  pred_daily_ICU_adm <- do.call(rbind, list_res_pred_ICU_adm_2[[iRegion]])
  pred_ICU_occ <- do.call(rbind, list_res_pred_ICU_occ_2[[iRegion]])
  pred_daily_hosp_adm <- do.call(rbind, list_res_pred_hosp_adm_2[[iRegion]])
  pred_hosp_occ <- do.call(rbind, list_res_pred_hosp_occ_2[[iRegion]])
  pred_cum_inf <- do.call(rbind, list_res_pred_cum_inf_2[[iRegion]])
  pred_new_inf <- do.call(rbind, list_res_pred_new_inf_2[[iRegion]])
  
  
  ## Compute CI
  CI_pred_ICU_adm <- sapply(seq_date, FUN = function(i){
    quantile(pred_daily_ICU_adm[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  CI_pred_ICU_occ <- sapply(seq_date, FUN = function(i){
    quantile(pred_ICU_occ[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  CI_pred_hosp_adm <- sapply(seq_date, FUN = function(i){
    quantile(pred_daily_hosp_adm[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  CI_pred_hosp_occ <- sapply(seq_date, FUN = function(i){
    quantile(pred_hosp_occ[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  CI_pred_cum_infections <- sapply(seq_date, FUN = function(i){
    quantile(pred_cum_inf[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  CI_pred_new_infections <- sapply(seq_date, FUN = function(i){
    quantile(pred_new_inf[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  
  
  ## getting observed_data
  daily_ICU_admissions <- mat_daily_ICU[,colnames(mat_daily_ICU) == name_region]
  total_ICU_occ <- mat_ICU_occ[,colnames(mat_ICU_occ) == name_region]
  daily_ICU_admissions_before_beginning <- mat_daily_ICU_before_beginning[,colnames(mat_daily_ICU_before_beginning) == name_region]
  total_ICU_occ_before_beginning <- mat_ICU_occ_before_beginning[,colnames(mat_ICU_occ_before_beginning) == name_region]
  
  
  daily_hosp_admissions <- mat_daily_hosp[,colnames(mat_daily_hosp) == name_region]
  total_hosp_occ <- mat_hosp_occ[,colnames(mat_hosp_occ) == name_region]
  daily_hosp_admissions_before_beginning <- mat_daily_hosp_before_beginning[,colnames(mat_daily_hosp_before_beginning) == name_region]
  total_hosp_occ_before_beginning <- mat_hosp_occ_before_beginning[,colnames(mat_hosp_occ_before_beginning) == name_region]
  
  
  color_pred <- MyPal[length(MyPal)]
  date_beginning_plot <- as.Date("2020-03-01")
  
  ## Plot daily ICU admission
  y_max <- max(CI_pred_ICU_adm, daily_ICU_admissions)
  plot(seq(1, length(seq_date)), CI_pred_ICU_adm[1,], type = 'l', col = color_pred,
       main = paste0(name_region, ' - Daily ICU admissions'), ylim = c( 0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Daily admissions',
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)))
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_ICU_adm[2,], rev(CI_pred_ICU_adm[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_ICU_adm[4,], rev(CI_pred_ICU_adm[5,])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(12, length(seq_date), 7), labels = labels_for_plot[seq(12, length(seq_date), 7)])
  axis(side = 2, las = 1)
  points(seq(index_begin_calibration, index_begin_calibration + length(daily_ICU_admissions) - 1),
         daily_ICU_admissions, pch = 16, cex = 0.8)
  points(seq(index_begin_calibration -length(daily_ICU_admissions_before_beginning), index_begin_calibration - 1),
         daily_ICU_admissions_before_beginning, pch = 1, cex = 0.8)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  legend('topright',
         fill = c(color_pred, NA), border = NA,
         pch = c(NA, 16),
         col = c('black', 'black'),
         legend = c('Model predictions', 'Hospitalisation data'),
         bty = 'n', cex = 1)
 
  ## Plot daily hosp admission
  y_max <- max(CI_pred_hosp_adm, daily_hosp_admissions)
  plot(seq(1, length(seq_date)), CI_pred_hosp_adm[1,], type = 'l', col = color_pred,
       main = paste0(name_region, ' - Daily hospital admissions'), ylim = c( 0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Daily admissions',
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)))
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_hosp_adm[2,], rev(CI_pred_hosp_adm[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_hosp_adm[4,], rev(CI_pred_hosp_adm[5,])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(12, length(seq_date), 7), labels = labels_for_plot[seq(12, length(seq_date), 7)])
  axis(side = 2, las = 1)
  points(seq(index_begin_calibration, index_begin_calibration + length(daily_hosp_admissions) - 1),
         daily_hosp_admissions, pch = 16, cex = 0.8)
  points(seq(index_begin_calibration -length(daily_hosp_admissions_before_beginning), index_begin_calibration - 1),
         daily_hosp_admissions_before_beginning, pch = 1, cex = 0.8)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  
  
  ## Daily new infections
  y_min <- 10
  y_max <- max(CI_pred_new_infections)
  plot(seq(2, length(seq_date)), CI_pred_new_infections[1,2:length(seq_date)], type = 'l', col = color_pred,
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)),
       main = paste0(name_region, ' - Daily new infections'),
       ylim = c(y_min, y_max),
       log = 'y',
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
  title(ylab = 'Daily infections', line = 4)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_new_infections[2,], rev(CI_pred_new_infections[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_new_infections[4,1:length(seq_date)], rev(CI_pred_new_infections[5,1:length(seq_date)])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(12, length(seq_date), 7), labels = labels_for_plot[seq(12, length(seq_date), 7)])
  axis(side = 2, las = 1, at = c(10, 100, 1000, 5000, 1e4, 5e4, 1e5, 5e5),
       labels = c('10', '100', '1,000','5,000',  '10,000', '50,000', '100,000', '500,000'))
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  
  
  ## ICU beds
  y_max <- max(CI_pred_ICU_occ, total_ICU_occ)
  plot(seq(1, length(seq_date)), CI_pred_ICU_occ[1,], type = 'l', col = color_pred,
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)),
       main = paste0(name_region, ' - Number of ICU beds'), ylim = c( 0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Number of beds')
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_ICU_occ[2,], rev(CI_pred_ICU_occ[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_ICU_occ[4,], rev(CI_pred_ICU_occ[5,])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(12, length(seq_date), 7), labels = labels_for_plot[seq(12, length(seq_date), 7)])
  axis(side = 2, las = 1)
  points(seq(index_begin_calibration, index_begin_calibration + length(total_ICU_occ) - 1),
         total_ICU_occ, pch = 16, cex = 0.8)
  points(seq(index_begin_calibration -length(total_ICU_occ_before_beginning), index_begin_calibration - 1),
         total_ICU_occ_before_beginning, pch = 1, cex = 0.8)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
 
  ## Hosp beds
  y_max <- max(CI_pred_hosp_occ, total_hosp_occ)
  plot(seq(1, length(seq_date)), CI_pred_hosp_occ[1,], type = 'l', col = color_pred,
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)),
       main = paste0(name_region, ' - Number of general ward beds'), ylim = c( 0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Number of beds')
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_hosp_occ[2,], rev(CI_pred_hosp_occ[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_hosp_occ[4,], rev(CI_pred_hosp_occ[5,])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(12, length(seq_date), 7), labels = labels_for_plot[seq(12, length(seq_date), 7)])
  points(seq(index_begin_calibration, index_begin_calibration + length(total_hosp_occ) - 1),
         total_hosp_occ, pch = 16, cex = 0.8)
  points(seq(index_begin_calibration -length(total_hosp_occ_before_beginning), index_begin_calibration - 1),
         total_hosp_occ_before_beginning, pch = 1, cex = 0.8)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  axis(side = 2, las = 1)
  
  
  ## Proportion of the population infected
  y_min <- 0
  y_max <- ceiling(max(CI_pred_cum_infections)/popSize*100)
  plot(seq(2, length(seq_date)), CI_pred_cum_infections[1,2:length(seq_date)]/popSize*100, type = 'l', col = color_pred,
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)),
       main = paste0(name_region, ' - Proprotion infected (%)'),
       ylim = c(y_min, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Proportion infected (%)')
  polygon(x = c(seq(2, length(seq_date)), rev(seq(2, length(seq_date)))),
          y = c(CI_pred_cum_infections[2,2:length(seq_date)], rev(CI_pred_cum_infections[3,2:length(seq_date)]))/popSize*100,
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(2, length(seq_date)), rev(seq(2, length(seq_date)))),
          y = c(CI_pred_cum_infections[4,2:length(seq_date)], rev(CI_pred_cum_infections[5,2:length(seq_date)]))/popSize*100,
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(12, length(seq_date), 7), labels = labels_for_plot[seq(12, length(seq_date), 7)])
  axis(side = 2, las = 1)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  
}
