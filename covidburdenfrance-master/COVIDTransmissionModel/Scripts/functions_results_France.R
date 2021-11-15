## Trace plots of the chains
plot_chains <- function(list_res_1, list_res_2, list_res_3, list_res_4, BI, title = '',
                        param_names = c('R0',
                                        'g_out_ICU',
                                        'g_out_hosp',
                                        'alpha',
                                        'R0_after_intervention',
                                        'log_Inf_lockdown',
                                        'delta')){
  par(mar = c(4,5,2,1), mfrow = c(4,2))
  parameters_1 <- list_res_1$parameters
  parameters_2 <- list_res_2$parameters
  parameters_3 <- list_res_3$parameters
  parameters_4 <- list_res_4$parameters
  
  logLik_all_1 <- list_res_1$logLik
  logLik_all_2 <- list_res_2$logLik
  logLik_all_3 <- list_res_3$logLik
  logLik_all_4 <- list_res_4$logLik
  
  n_regions <- ncol(parameters_1)
  burnIn <- BI
  
  for(parID in 1:ncol(parameters_1)) {
    y_max <- max(c(parameters_1[-(1:burnIn),parID], parameters_2[-(1:burnIn),parID], parameters_3[-(1:burnIn),parID], parameters_4[-(1:burnIn),parID]))
    y_min <- min(c(parameters_1[-(1:burnIn),parID], parameters_2[-(1:burnIn),parID], parameters_3[-(1:burnIn),parID], parameters_4[-(1:burnIn),parID]))
    plot(parameters_1[-(1:burnIn),parID], type = 'l', ylab = param_names[parID],
         xlab = expression(n[iter]), main = param_names[parID],
         ylim = c(y_min, y_max), col = MyPal[1])
    lines(parameters_2[-(1:burnIn),parID], col = MyPal[2])
    lines(parameters_3[-(1:burnIn),parID], col = MyPal[3])
    lines(parameters_4[-(1:burnIn),parID], col = MyPal[4])
    
  }
  
  
  y_max <- max(c(logLik_all_1[-(1:burnIn)], logLik_all_2[-(1:burnIn)], logLik_all_3[-(1:burnIn)], logLik_all_4[-(1:burnIn)]))
  y_min <- min(c(logLik_all_1[-(1:burnIn)], logLik_all_2[-(1:burnIn)], logLik_all_3[-(1:burnIn)], logLik_all_4[-(1:burnIn)]))
  plot(logLik_all_1[-(1:burnIn)],main="Log-Lik", type = 'l', 
       ylab = 'Log-Lik',
       col = MyPal[1], ylim = c(y_min, y_max))
  lines(logLik_all_2[-(1:burnIn)], col = MyPal[2])
  lines(logLik_all_3[-(1:burnIn)], col = MyPal[3])
  lines(logLik_all_4[-(1:burnIn)], col = MyPal[4])
  legend('bottomright', col = NA, legend = title, bty = 'n')
  
  
}

## Acceptance rates
acc_rates <- function(list_res_1, list_res_2, list_res_3, list_res_4, BI,
                      param_names = c('R0', 'g_out_ICU', 'g_out_hosp', 'alpha', 'R0_after_intervention', 'log_Inf_lockdown', 'delta')){
  
  acc_rate_1 <- list_res_1$accept[-(1:BI),]
  acc_rate_2 <- list_res_2$accept[-(1:BI),]
  acc_rate_3 <- list_res_3$accept[-(1:BI),]
  acc_rate_4 <- list_res_4$accept[-(1:BI),]
  
  
  AR_1 <- apply(acc_rate_1, 2, sum)/nrow(acc_rate_1)
  AR_2 <- apply(acc_rate_2, 2, sum)/nrow(acc_rate_2)
  AR_3 <- apply(acc_rate_3, 2, sum)/nrow(acc_rate_3)
  AR_4 <- apply(acc_rate_4, 2, sum)/nrow(acc_rate_4)
  
  
  acc_rate_tot <- rbind(AR_1, AR_2, AR_3, AR_4)
  
  rownames(acc_rate_tot) <- seq(1,4)
  colnames(acc_rate_tot) <- param_names
  return(acc_rate_tot)
}

## Parameter estimates
param_estim <- function(list_res_1, list_res_2, list_res_3, list_res_4, BI,
                        param_names = c('R0', 'g_out_ICU', 'g_out_hosp', 'alpha', 'R0_after_intervention', 'log_Inf_lockdown', 'delta')){
  parameters_1 <- list_res_1$parameters
  parameters_2 <- list_res_2$parameters
  parameters_3 <- list_res_3$parameters
  parameters_4 <- list_res_4$parameters
  
  logLik_all_1 <- list_res_1$logLik
  logLik_all_2 <- list_res_2$logLik
  logLik_all_3 <- list_res_3$logLik
  logLik_all_4 <- list_res_4$logLik
  
  burnIn <- BI
  
  parameters_1 <- parameters_1[(-(1:burnIn)),]
  parameters_2 <- parameters_2[(-(1:burnIn)),]
  parameters_3 <- parameters_3[(-(1:burnIn)),]
  parameters_4 <- parameters_4[(-(1:burnIn)),]
  
  
  estim_1 <- apply(parameters_1, 2, FUN = function(X){
    tmp_quantile <- quantile(X, probs = c(0.5, 0.025, 0.975))
    paste0(round(tmp_quantile[1], 2), ' [', round(tmp_quantile[2], 2), ' - ', round(tmp_quantile[3], 2), ']')
  })
  estim_2 <- apply(parameters_2, 2, FUN = function(X){
    tmp_quantile <- quantile(X, probs = c(0.5, 0.025, 0.975))
    paste0(round(tmp_quantile[1], 2), ' [', round(tmp_quantile[2], 2), ' - ', round(tmp_quantile[3], 2), ']')
  })
  estim_3 <- apply(parameters_3, 2, FUN = function(X){
    tmp_quantile <- quantile(X, probs = c(0.5, 0.025, 0.975))
    paste0(round(tmp_quantile[1], 2), ' [', round(tmp_quantile[2], 2), ' - ', round(tmp_quantile[3], 2), ']')
  })
  estim_4 <- apply(parameters_4, 2, FUN = function(X){
    tmp_quantile <- quantile(X, probs = c(0.5, 0.025, 0.975))
    paste0(round(tmp_quantile[1], 2), ' [', round(tmp_quantile[2], 2), ' - ', round(tmp_quantile[3], 2), ']')
  })
  
  mat_estim <- rbind(estim_1, estim_2, estim_3, estim_4)
  mat_estim <- t(mat_estim)
  rownames(mat_estim) <- param_names
  return(mat_estim)
}

## Median of the posterior distribution
median_param <- function(list_res_1, BI, param_names = c('R0', 'g_out_ICU', 'g_out_hosp', 'alpha', 'R0_after_intervention', 'log_Inf_lockdown', 'delta')){
  
  parameters_1 <- list_res_1$parameters
  burnIn <- BI
  parameters_1 <- parameters_1[(-(1:burnIn)),]
  median_1 <- apply(parameters_1, 2, median)
  names(median_1) <- param_names
  
  return(median_1)
}

## Simulation from posterior with scenarios
simul_from_posterior<- function(list_res, BI, n_simul_1, n_simul_2, seed = 1){
  
  ## Defining burnIn
  burnIn <- BI
  ## Getting posterior distribution
  parameters <- list_res$parameters
  
  ## Removing BI
  parameters <- parameters[-(1:burnIn),]
  
  ## Seeding the RNG
  set.seed(seed)
  ## Sampling n_simul_1 indices
  index_sample <- sample.int(n = nrow(parameters), size = n_simul_1)
  
  ## Sampling for each region n_simul_1 baselines that will be used
  ## as average of the negative binomial
  list_res <- lapply(1:n_simul_1, FUN = function(iIndex){
    index <- index_sample[iIndex]
    tmp_param <- parameters[index,]
    tmp_out <- output_from_param(tmp_param[1], tmp_param[2], tmp_param[3], tmp_param[4], tmp_param[5], tmp_param[6])
    tmp_out
  })
  
  
  ## For each of these n_simul_1 baselines, compute daily ICU admissions
  pred_daily_ICU_adm <- lapply(1:length(list_res), FUN = function(i){
    tmp_out <- list_res[[i]]
    tmp_ICU_adm <- get_daily_ICU_admission_from_output(tmp_out)
    tmp_delta <- parameters[index_sample[i], 7]
    sapply(tmp_ICU_adm, FUN = function(X){
      if(X == 0){
        rep(0, n_simul_2)
      } else{
        rnbinom(n = n_simul_2, size = X^tmp_delta, mu = X)
      }
    })
  })
  
  ## For each of these n_simul_1 baselines, compute daily hospital admissions
  pred_daily_hosp_adm <- lapply(1:length(list_res), FUN = function(i){
    tmp_out <- list_res[[i]]
    tmp_hosp_adm <- get_daily_hosp_admission_from_output(tmp_out)
    tmp_delta <- parameters[index_sample[i], 7]
    sapply(tmp_hosp_adm, FUN = function(X){
      if(X == 0){
        rep(0, n_simul_2)
      } else{
        rnbinom(n = n_simul_2, size = X^tmp_delta, mu = X)
      }
    })
  })
  
  ## For each of these n_simul_1 baselines, compute ICU beds
  pred_ICU_occ <- lapply(1:length(list_res), FUN = function(i){
    tmp_out <- list_res[[i]]
    tmp_ICU_occ <- get_ICU_occ_from_output(tmp_out)
    tmp_delta <- parameters[index_sample[i], 7]
    sapply(tmp_ICU_occ, FUN = function(X){
      if(X == 0){
        rep(0, n_simul_2)
      } else{
        rnbinom(n = n_simul_2, size = X^tmp_delta, mu = X)
      }
    })
  })
  
  ## For each of these n_simul_1 baselines, compute general ward beds
  pred_hosp_occ <- lapply(1:length(list_res), FUN = function(i){
    tmp_out <- list_res[[i]]
    tmp_hosp_occ <- get_hosp_occ_from_output(tmp_out)
    tmp_delta <- parameters[index_sample[i], 7]
    sapply(tmp_hosp_occ, FUN = function(X){
      if(X == 0){
        rep(0, n_simul_2)
      } else{
        rnbinom(n = n_simul_2, size = X^tmp_delta, mu = X)
      }
    })
  })
  
  ## Compute expected cumulative number of infections
  pred_cum_infections <- t(sapply(1:length(list_res), FUN = function(i){
    tmp_out <- list_res[[i]]
    tmp_cum_inf <- get_cum_infections_from_output(tmp_out)
    tmp_cum_inf
  }))
  
  ## Compute daily new infections
  pred_daily_new_infections <- t(sapply(1:length(list_res), FUN = function(i){
    tmp_out <- list_res[[i]]
    tmp_new_inf <- get_daily_new_infections_from_output(tmp_out)
    tmp_new_inf
  }))
  
  list_res_to_save <- list(pred_daily_ICU_adm = pred_daily_ICU_adm,
                           pred_daily_hosp_adm = pred_daily_hosp_adm,
                           pred_ICU_occ = pred_ICU_occ,
                           pred_hosp_occ = pred_hosp_occ,
                           pred_cum_infections = pred_cum_infections,
                           pred_daily_new_infections = pred_daily_new_infections)
  
  return(list_res_to_save)
}

## Plot results of the simulation - Hospital trajectories
panel_ICU_hosp_France <- function(pred_daily_ICU_adm, pred_daily_hosp_adm, pred_ICU_occ, pred_hosp_occ){
  
  
  pred_daily_ICU_adm_2 <- do.call(rbind, pred_daily_ICU_adm)
  pred_daily_hosp_adm_2 <- do.call(rbind, pred_daily_hosp_adm)
  pred_ICU_occ_2 <- do.call(rbind, pred_ICU_occ)
  pred_hosp_occ_2 <- do.call(rbind, pred_hosp_occ)
  
  ## Dates for plot
  date_min <- date_intervention - time_intervention
  date_max <- date_min + 110
  seq_date <- seq(1, date_max - date_min + 1)
  index_begin_calibration <- as.numeric(date_beginning_calibration - date_min) + 1
  labels_for_plot <- format(as.Date(seq(0, length(seq_date) - 1), origin = date_min), '%d/%m')
  
  
  CI_pred_ICU_adm_2 <- sapply(seq_date, FUN = function(i){
    quantile(pred_daily_ICU_adm_2[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  CI_pred_ICU_occ_2 <- sapply(seq_date, FUN = function(i){
    quantile(pred_ICU_occ_2[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  
  CI_pred_hosp_adm_2 <- sapply(seq_date, FUN = function(i){
    quantile(pred_daily_hosp_adm_2[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  CI_pred_hosp_occ_2 <- sapply(seq_date, FUN = function(i){
    quantile(pred_hosp_occ_2[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  
  daily_ICU_admissions <- apply(mat_daily_ICU[,colnames(mat_daily_ICU) %in% vect_name_region], 1, sum)
  total_ICU_occ <- apply(mat_ICU_occ[,colnames(mat_ICU_occ) %in% vect_name_region], 1, sum)
  daily_ICU_admissions_before_beginning <- apply(mat_daily_ICU_before_beginning[,colnames(mat_daily_ICU_before_beginning) %in% vect_name_region], 1, sum)
  total_ICU_occ_before_beginning <- apply(mat_ICU_occ_before_beginning[,colnames(mat_ICU_occ_before_beginning) %in% vect_name_region], 1, sum)
  
  daily_hosp_admissions <- apply(mat_daily_hosp[,colnames(mat_daily_hosp) %in% vect_name_region], 1, sum)
  total_hosp_occ <- apply(mat_hosp_occ[,colnames(mat_hosp_occ) %in% vect_name_region], 1, sum)
  daily_hosp_admissions_before_beginning <- apply(mat_daily_hosp_before_beginning[,colnames(mat_daily_hosp_before_beginning) %in% vect_name_region], 1, sum)
  total_hosp_occ_before_beginning <- apply(mat_hosp_occ_before_beginning[,colnames(mat_hosp_occ_before_beginning) %in% vect_name_region], 1, sum)
  
  color_pred <- MyPal[length(MyPal)]
  
  date_beginning_plot <- as.Date("2020-03-01")
  
  y_max <- max(CI_pred_ICU_adm_2, daily_ICU_admissions)
  
  plot(seq(1, length(seq_date)), CI_pred_ICU_adm_2[1,], type = 'l', col = color_pred,
       main = paste0("Daily ICU admissions"), ylim = c(0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Daily admissions',
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)))
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_ICU_adm_2[2,], rev(CI_pred_ICU_adm_2[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_ICU_adm_2[4,], rev(CI_pred_ICU_adm_2[5,])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(5, length(seq_date), 7),
       labels = labels_for_plot[seq(5, length(seq_date), 7)])
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
  
  y_max <- max(CI_pred_ICU_occ_2, total_ICU_occ)
  plot(seq(1, length(seq_date)), CI_pred_ICU_occ_2[1,], type = 'l', col = color_pred,
       main = paste0("Number of ICU beds"), ylim = c( 0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Number of beds',
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)))
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_ICU_occ_2[2,], rev(CI_pred_ICU_occ_2[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_ICU_occ_2[4,], rev(CI_pred_ICU_occ_2[5,])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(5, length(seq_date), 7),
       labels = labels_for_plot[seq(5, length(seq_date), 7)])
  axis(side = 2, las = 1)
  points(seq(index_begin_calibration, index_begin_calibration + length(total_ICU_occ) - 1),
         total_ICU_occ, pch = 16, cex = 0.8)
  points(seq(index_begin_calibration -length(total_ICU_occ_before_beginning), index_begin_calibration - 1),
         total_ICU_occ_before_beginning, pch = 1, cex = 0.8)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  
  y_max <- max(CI_pred_hosp_adm_2, daily_hosp_admissions)
  plot(seq(1, length(seq_date)), CI_pred_hosp_adm_2[1,], type = 'l', col = color_pred,
       main = paste0("Daily hospital admissions"), ylim = c(0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Daily admissions',
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)))
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_hosp_adm_2[2,], rev(CI_pred_hosp_adm_2[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_hosp_adm_2[4,], rev(CI_pred_hosp_adm_2[5,])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(5, length(seq_date), 7),
       labels = labels_for_plot[seq(5, length(seq_date), 7)])
  axis(side = 2, las = 1)
  points(seq(index_begin_calibration, index_begin_calibration + length(daily_ICU_admissions) - 1),
         daily_hosp_admissions, pch = 16, cex = 0.8)
  points(seq(index_begin_calibration -length(daily_hosp_admissions_before_beginning), index_begin_calibration - 1),
         daily_hosp_admissions_before_beginning, pch = 1, cex = 0.8)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  
  
  y_max <- max(CI_pred_hosp_occ_2, total_hosp_occ)/1000
  plot(seq(1, length(seq_date)), CI_pred_hosp_occ_2[1,]/1000, type = 'l', col = color_pred,
       main = paste0("Number of general ward beds (k)"), ylim = c( 0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Number of beds (k)',
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)))
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_hosp_occ_2[2,], rev(CI_pred_hosp_occ_2[3,]))/1000,
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_hosp_occ_2[4,], rev(CI_pred_hosp_occ_2[5,]))/1000,
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(5, length(seq_date), 7),
       labels = labels_for_plot[seq(5, length(seq_date), 7)])
  axis(side = 2, las = 1)
  points(seq(index_begin_calibration, index_begin_calibration + length(total_hosp_occ) - 1),
         total_hosp_occ/1000, pch = 16, cex = 0.8)
  points(seq(index_begin_calibration -length(total_hosp_occ_before_beginning), index_begin_calibration - 1),
         total_hosp_occ_before_beginning/1000, pch = 1, cex = 0.8)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
}

## Plot results of the simulations - Proportion infected
plot_proportion_infected <- function(pred_cum_infections){
  
  
  ## Dates for plot
  date_min <- date_intervention - time_intervention
  date_max <- date_min + 110
  seq_date <- seq(1, date_max - date_min + 1)
  index_begin_calibration <- as.numeric(date_beginning_calibration - date_min) + 1
  labels_for_plot <- format(as.Date(seq(0, length(seq_date) - 1), origin = date_min), '%d/%m')
  
  CI_pred_cum_infections <- sapply(seq_date, FUN = function(i){
    quantile(pred_cum_infections[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  
  color_pred <- MyPal[length(MyPal)]
  
  date_beginning_plot <- as.Date("2020-03-01")
  ## Plot cumulative number of infection
  y_max <- ceiling(max(CI_pred_cum_infections)/popSize*100)
  plot(seq(1, length(seq_date)), CI_pred_cum_infections[1,]/popSize*100, type = 'l', col = color_pred,
       main = paste0("Proportion infected (%)"), ylim = c(0, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = 'Proportion (%)',
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)))
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_cum_infections[2,], rev(CI_pred_cum_infections[3,]))/popSize*100,
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_cum_infections[4,], rev(CI_pred_cum_infections[5,]))/popSize*100,
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(5, length(seq_date), 7),
       labels = labels_for_plot[seq(5, length(seq_date), 7)])
  axis(side = 2, las = 1, line = 0.5)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  
}

## Plot results of the simulations - Daily new infections
plot_new_infections <- function(pred_new_infections){
  
  ## Dates for plot
  date_min <- date_intervention - time_intervention
  date_max <- date_min + 110
  seq_date <- seq(1, date_max - date_min + 1)
  index_begin_calibration <- as.numeric(date_beginning_calibration - date_min) + 1
  labels_for_plot <- format(as.Date(seq(0, length(seq_date) - 1), origin = date_min), '%d/%m')
  
  CI_pred_new_infections <- sapply(seq_date, FUN = function(i){
    quantile(pred_new_infections[,i], probs = c(0.5, 0.025, 0.975, 0.25, 0.75))
  })
  color_pred <- MyPal[length(MyPal)]
  
  date_beginning_plot <- as.Date("2020-03-01")
  ## Plot cumulative number of infection
  y_max <- max(CI_pred_new_infections)
  plot(seq(1, length(seq_date)), CI_pred_new_infections[1,], type = 'l', col = color_pred,
       main = paste0("Daily new infections"), ylim = c(1000, y_max),
       xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '',
       xlim = c(time_intervention - as.numeric(date_intervention - date_beginning_plot) + 2,
                length(seq_date)),
       log = 'y')
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_new_infections[2,], rev(CI_pred_new_infections[3,])),
          col = adjustcolor(color_pred, alpha.f = 0.1), border = NA)
  polygon(x = c(seq(1, length(seq_date)), rev(seq(1, length(seq_date)))),
          y = c(CI_pred_new_infections[4,], rev(CI_pred_new_infections[5,])),
          col = adjustcolor(color_pred, alpha.f = 0.3), border = NA)
  axis(side = 1, at = seq(5, length(seq_date), 7),
       labels = labels_for_plot[seq(5, length(seq_date), 7)])
  title(ylab = 'Daily infections', line = 5)
  axis(side = 2, las = 1, line = 0.5)
  abline(v = time_intervention + 1, col = 'darkgreen', lwd = 2)
  
}
