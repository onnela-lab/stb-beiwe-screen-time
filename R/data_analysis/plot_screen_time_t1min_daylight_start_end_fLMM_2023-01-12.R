
#' The plotting code reuses plotting code published with Cui et al. (2021).  
#' 
#' @references
#' Erjia Cui, Andrew Leroux, Ekaterina Smirnova, Ciprian M. Crainiceanu (2021) 
#' Fast Univariate Inference for Longitudinal Functional Models, 
#' Journal of Computational and Graphical Statistics, 
#' DOI: 10.1080/10618600.2021.1950006

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
library(ggsci)
library(cowplot)
library(ggpubr)
library(ggtext)
library(grid)
options(digits.secs = 3)
options(scipen = 999)
source(file.path(here(), "R", "config.R"))
source(file.path(here(), "R", "config_figures.R"))
source(file.path(here(), "R", "utils.R"))


# ------------------------------------------------------------------------------
# plot: 3x 2
# ------------------------------------------------------------------------------

plot_title_vec <- c(
  "a",
  "b",
  "c",
  "d",
  "e",
  "f"
)

DAYS_DIFF_VALUE_MAX_VEC <- c(7, 14, 28)
DAYLIGHT_TIME_CHANGE_VEC <- c("start", "end")
param_df <- expand_grid(DAYS_DIFF_VALUE_MAX = DAYS_DIFF_VALUE_MAX_VEC, DAYLIGHT_TIME_CHANGE = DAYLIGHT_TIME_CHANGE_VEC)

plt_list <- list()
for (i in 1 : nrow(param_df)){ # i <- 1
  print(i)
  DAYS_DIFF_VALUE_MAX <- param_df$DAYS_DIFF_VALUE_MAX[i]
  DAYLIGHT_TIME_CHANGE <- param_df$DAYLIGHT_TIME_CHANGE[i]
  # 2nd variable, i.e. 1st variable after the intercept
  r <- 2
  
  # pull precomputed data set specific to current param set 
  fit_fname <- paste0("fit_result_lfosr3s_daylight_", DAYLIGHT_TIME_CHANGE, "_", DAYS_DIFF_VALUE_MAX, ".rds")
  fit_fpath <- file.path(here(), "results_objects", fit_fname)
  fit_tmp <- readRDS(fit_fpath)
  # format data, append
  beta.hat.plt <- data.frame(s = seq(1, ncol(fit_tmp$betaHat), length.out = ncol(fit_tmp$betaHat)), 
                             beta = fit_tmp$betaHat[r,],
                             lower = fit_tmp$betaHat[r,] - 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             upper = fit_tmp$betaHat[r,] + 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             lower.joint = fit_tmp$betaHat[r,] - fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             upper.joint = fit_tmp$betaHat[r,] + fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])))
  beta.hat.plt$days_diff_value_max <- DAYS_DIFF_VALUE_MAX
  beta.hat.plt$daylight_time_change <- DAYLIGHT_TIME_CHANGE
  
  # other plot elements
  decimal <- c(1, 1)
  txt_color = "black"
  txt_size = 10
  geom_text_label = paste0("Daylight saving time ", DAYLIGHT_TIME_CHANGE, " +/-", DAYS_DIFF_VALUE_MAX, " days")
  y_vals <- c(beta.hat.plt$lower.joint, beta.hat.plt$upper.joint)
  geom_text_x <- 0
  geom_text_y <- min(y_vals) + 1.2 * diff(range(y_vals))
  
  plt <- 
    ggplot() +
    theme(
      panel.grid.major = element_line(size = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey50", linewidth = 0.5, fill = NA),
      axis.text.x = element_text(color = txt_color, size = txt_size),
      axis.text.y = element_text(color = txt_color, size = txt_size),  
      axis.title.x = element_text(color = txt_color, size = txt_size),
      axis.title.y = element_text(color = txt_color, size = txt_size),
      plot.title = ggtext::element_markdown(color = txt_color, size = txt_size),
      text = element_text(color = txt_color, size = txt_size) 
    ) + 
    geom_hline(yintercept = 0, size = 0.3, linetype = 2) + 
    geom_ribbon(aes(x = s, ymax = upper.joint, ymin = lower.joint), data = beta.hat.plt, fill = "gray30", alpha = 0.2) +
    geom_ribbon(aes(x = s, ymax = upper, ymin = lower), data = beta.hat.plt, fill = "gray10", alpha = 0.4) +
    geom_line(aes(x = s, y = beta, color = "Estimate"), data = beta.hat.plt, alpha = 1, linetype = 2) +
    scale_colour_manual(name="", values=c("Estimate"="blue3")) +
    scale_y_continuous(labels=function(x) sprintf(paste0("%.", decimal[r], "f"), x)) +
    scale_x_continuous(breaks = seq(0, 1440, by = 60 * 4)) + 
    annotate("text", x = geom_text_x, y = geom_text_y, label = geom_text_label, hjust = 0, size = 4) + 
    theme(legend.position="none") + 
    labs(x = "", y = expression(beta[1](s)), 
         title = paste0("**", plot_title_vec[i], "**")) 
  if (i %in% c(5, 6)){
    plt <- plt + labs(x = "Minute of a day [UTC time], s")
  }
  
  plt_list[[length(plt_list) + 1]] <- plt
}


# generate final plot
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "vh", byrow = TRUE)
# rel_heights = c(rep(1, 4), 0.25))
plt_fpath <- file.path(here(), "results_figures", "screen_time_daylight_start_end_fLMM.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10, dpi = 300)


# ------------------------------------------------------------------------------
# plot: 1x 2
# ------------------------------------------------------------------------------


DAYS_DIFF_VALUE_MAX_VEC <- c(14)
DAYLIGHT_TIME_CHANGE_VEC <- c("start", "end")
param_df <- expand_grid(DAYS_DIFF_VALUE_MAX = DAYS_DIFF_VALUE_MAX_VEC, DAYLIGHT_TIME_CHANGE = DAYLIGHT_TIME_CHANGE_VEC)

plt_list <- list()
for (i in 1 : nrow(param_df)){ # i <- 1
  print(i)
  DAYS_DIFF_VALUE_MAX <- param_df$DAYS_DIFF_VALUE_MAX[i]
  DAYLIGHT_TIME_CHANGE <- param_df$DAYLIGHT_TIME_CHANGE[i]
  # 2nd variable, i.e. 1st variable after the intercept
  r <- 2
  
  # pull precomputed data set specific to current param set 
  fit_fname <- paste0("fit_result_lfosr3s_daylight_", DAYLIGHT_TIME_CHANGE, "_", DAYS_DIFF_VALUE_MAX, ".rds")
  fit_fpath <- file.path(here(), "results_objects", fit_fname)
  fit_tmp <- readRDS(fit_fpath)
  # format data, append
  beta.hat.plt <- data.frame(s = seq(1, ncol(fit_tmp$betaHat), length.out = ncol(fit_tmp$betaHat)), 
                             beta = fit_tmp$betaHat[r,],
                             lower = fit_tmp$betaHat[r,] - 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             upper = fit_tmp$betaHat[r,] + 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             lower.joint = fit_tmp$betaHat[r,] - fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                             upper.joint = fit_tmp$betaHat[r,] + fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])))
  beta.hat.plt$days_diff_value_max <- DAYS_DIFF_VALUE_MAX
  beta.hat.plt$daylight_time_change <- DAYLIGHT_TIME_CHANGE
  
  # other plot elements
  decimal <- c(1, 1)
  txt_color = "black"
  txt_size = 10
  geom_text_label = paste0("Daylight saving time ", DAYLIGHT_TIME_CHANGE, " +/-", DAYS_DIFF_VALUE_MAX, " days")
  y_vals <- c(beta.hat.plt$lower.joint, beta.hat.plt$upper.joint)
  geom_text_x <- 0
  geom_text_y <- min(y_vals) + 1.2 * diff(range(y_vals))
  
  plt <- 
    ggplot() +
    theme(
      panel.grid.major = element_line(size = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey50", linewidth = 0.5, fill = NA),
      axis.text.x = element_text(color = txt_color, size = txt_size),
      axis.text.y = element_text(color = txt_color, size = txt_size),  
      axis.title.x = element_text(color = txt_color, size = txt_size),
      axis.title.y = element_text(color = txt_color, size = txt_size),
      plot.title = ggtext::element_markdown(color = txt_color, size = txt_size),
      text = element_text(color = txt_color, size = txt_size) 
    ) + 
    geom_hline(yintercept = 0, size = 0.3, linetype = 2) + 
    geom_ribbon(aes(x = s, ymax = upper.joint, ymin = lower.joint), data = beta.hat.plt, fill = "gray30", alpha = 0.2) +
    geom_ribbon(aes(x = s, ymax = upper, ymin = lower), data = beta.hat.plt, fill = "gray10", alpha = 0.4) +
    geom_line(aes(x = s, y = beta, color = "Estimate"), data = beta.hat.plt, alpha = 1, linetype = 2) +
    scale_colour_manual(name="", values=c("Estimate"="blue3")) +
    scale_y_continuous(labels=function(x) sprintf(paste0("%.", decimal[r], "f"), x)) +
    scale_x_continuous(breaks = seq(0, 1440, by = 60 * 4)) + 
    annotate("text", x = geom_text_x, y = geom_text_y, label = geom_text_label, hjust = 0, size = 4) + 
    theme(legend.position="none") + 
    labs(x = "Minute of a day [UTC time], s", y = expression(beta[1](s)), 
         title = paste0("**", plot_title_vec[i], "**")) 
  
  plt_list[[length(plt_list) + 1]] <- plt
}


# generate final plot
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "vh", byrow = TRUE)
# rel_heights = c(rep(1, 4), 0.25))
plt_fpath <- file.path(here(), "results_figures", "screen_time_daylight_start_end_fLMM_subset.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10 * 1/3, dpi = 300)



# ------------------------------------------------------------------------------
# pull hourly values for the paper text: START

# Eastern Daylight Time (EDT; Mar-Nov) is 4 hours behind Coordinated Universal Time
# Eastern Standard Time (EST; Nov-Mar) is 5 hours behind Coordinated Universal Time

DAYS_DIFF_VALUE_MAX <- 14
DAYLIGHT_TIME_CHANGE <- "start"
r <- 2

# pull precomputed data set specific to current param set 
fit_fname <- paste0("fit_result_lfosr3s_daylight_", DAYLIGHT_TIME_CHANGE, "_", DAYS_DIFF_VALUE_MAX, ".rds")
fit_fpath <- file.path(here(), "results_objects", fit_fname)
fit_tmp <- readRDS(fit_fpath)
# format data, append
beta.hat.plt <- data.frame(s = seq(1, ncol(fit_tmp$betaHat), length.out = ncol(fit_tmp$betaHat)), 
                           beta = fit_tmp$betaHat[r,],
                           lower = fit_tmp$betaHat[r,] - 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                           upper = fit_tmp$betaHat[r,] + 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                           lower.joint = fit_tmp$betaHat[r,] - fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                           upper.joint = fit_tmp$betaHat[r,] + fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])))
exclude0 <- (beta.hat.plt$lower.joint * beta.hat.plt$upper.joint > 0)
signif_mins_UTC <- which(exclude0) 
# minute to hour
signif_hrs_UTC <- signif_mins_UTC / 60
signif_hrs_UTC_range <- range(signif_hrs_UTC)
signif_hrs_UTC_range

# what does it mean relating "old time" (EST time)  
signif_hrs_EST_range <- signif_hrs_UTC_range - 5
signif_hrs_EST_range <- 24 + signif_hrs_EST_range
signif_hrs_EST_range
# minutes part
(signif_hrs_EST_range - floor(signif_hrs_EST_range)) * 60

# what does it mean relating "new time" (EDT time)  
signif_hrs_EDT_range <- signif_hrs_UTC_range - 4
signif_hrs_EDT_range <- 24 + signif_hrs_EDT_range
signif_hrs_EDT_range
# minutes part
(signif_hrs_EDT_range - floor(signif_hrs_EDT_range)) * 60



# ------------------------------------------------------------------------------
# pull hourly values for the paper text: END  

# Eastern Daylight Time (EDT; Mar-Nov) is 4 hours behind Coordinated Universal Time
# Eastern Standard Time (EST; Nov-Mar) is 5 hours behind Coordinated Universal Time

DAYS_DIFF_VALUE_MAX <- 14
DAYLIGHT_TIME_CHANGE <- "end"
r <- 2

# pull precomputed data set specific to current param set 
fit_fname <- paste0("fit_result_lfosr3s_daylight_", DAYLIGHT_TIME_CHANGE, "_", DAYS_DIFF_VALUE_MAX, ".rds")
fit_fpath <- file.path(here(), "results_objects", fit_fname)
fit_tmp <- readRDS(fit_fpath)
# format data, append
beta.hat.plt <- data.frame(s = seq(1, ncol(fit_tmp$betaHat), length.out = ncol(fit_tmp$betaHat)), 
                           beta = fit_tmp$betaHat[r,],
                           lower = fit_tmp$betaHat[r,] - 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                           upper = fit_tmp$betaHat[r,] + 2*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                           lower.joint = fit_tmp$betaHat[r,] - fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])),
                           upper.joint = fit_tmp$betaHat[r,] + fit_tmp$qn[r]*sqrt(diag(fit_tmp$betaHat.var[,,r])))
exclude0 <- (beta.hat.plt$lower.joint * beta.hat.plt$upper.joint > 0)
signif_mins_UTC <- which(exclude0) 
# minute to hour
signif_hrs_UTC <- signif_mins_UTC / 60
signif_hrs_UTC_range1 <- range(signif_hrs_UTC[signif_hrs_UTC < 7])
signif_hrs_UTC_range2 <- range(signif_hrs_UTC[signif_hrs_UTC > 7])
signif_hrs_UTC_range1
# [1] 2.366667 3.316667
signif_hrs_UTC_range2
# [1] 10.86667 11.73333
(signif_hrs_UTC_range1 - floor(signif_hrs_UTC_range1)) * 60
(signif_hrs_UTC_range2 - floor(signif_hrs_UTC_range2)) * 60


# what does it mean relating "old time" (EDT time)  
signif_hrs_EDT_range1 <- signif_hrs_UTC_range1 - 4
signif_hrs_EDT_range1 <- 24 + signif_hrs_EDT_range1
signif_hrs_EDT_range1
# minutes part
(signif_hrs_EDT_range1 - floor(signif_hrs_EDT_range1)) * 60

# what does it mean relating "new time" (EST time)  
signif_hrs_EST_range1 <- signif_hrs_UTC_range1 - 5
signif_hrs_EST_range1 <- 24 + signif_hrs_EST_range1
signif_hrs_EST_range1
# minutes part
(signif_hrs_EST_range1 - floor(signif_hrs_EST_range1)) * 60



# what does it mean relating "old time" (EDT time)  
signif_hrs_EDT_range2 <- signif_hrs_UTC_range2 - 4
signif_hrs_EDT_range2
# minutes part
(signif_hrs_EDT_range2 - floor(signif_hrs_EDT_range2)) * 60

# what does it mean relating "new time" (EST time)  
signif_hrs_EST_range2 <- signif_hrs_UTC_range2 - 5
signif_hrs_EST_range2 <- signif_hrs_EST_range2
signif_hrs_EST_range2
# minutes part
(signif_hrs_EST_range2 - floor(signif_hrs_EST_range2)) * 60


