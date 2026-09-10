## This file is to create the figures and documents for the synthesis paper.  Its based on the hierarchical modelling of Guilluame Dauphin.

# this code was originally writen in "Project Keith" but needed a cleaner version.

# the purpose of this file is to create publication quality graphics for the hierarchical models for the Synthesis paper for Rose Blanche.

# Objects used below are all extracted from the *rds file but sometimes, its necessary to extract values from the chains in order to get the probability of being a positive value  (imported chain summaries); 

# Objects used below are all extracted from the *rds file but sometimes, its necessary to extract values from the chains in order to get the probability of being a positive value  (imported chain summaries); 

# biomass are from csv files generated from pb_biomass.R

#  Labels (inherited from M11)

# libraries and source -----
library(tidyverse)
library(cowplot)


# Source 
#source("Scripts/data_prep_d.R")
source("functionsKL.R")

# ---- helpers (used by model scripts and this file) ----
if (!dir.exists("Data/PB_data")) {
  dir.create("Data/PB_data", recursive = TRUE)
}
if(!dir.exists("Figures/PB_figs")){
  dir.create("Figures/PB_figs", recursive = TRUE)
} #for publication quality only
if(!dir.exists("Tables/PB_tabs")){
  dir.create("Tables/PB_tabs", recursive = TRUE)
} #for publication quality only

outputs_path <- function(f) file.path("Figures/PB_figs", f)

# helper objects
## used for axis labels, changing 
y.axis <- expression("Density (fish/ m" ^2*")")
y.axis.bio <- expression("Biomass density (g/100 m" ^2*")")
y.axis.trt <- "Treatment Effect (ln scale)"

sl_levels <- c("AS", "ASYOY", "BT", "BTYOY")
#trt_colours <- c("Control" = "black", "Treatment" = "grey60")
#trt_labels <- c("Control", "Treatment")
dodge_w <- 0.5


# convert species codes to labels for plotting
species_code_map <- c(
  "AS"    = "AS-1+",
  "ASYOY" = "AS-0+",
  "BT"    = "BT-1+",
  "BTYOY" = "BT-0+"
)

# not sure what this is for - GD gave it to me
pb_params <- read.csv("Data/PB_data/results_hyper_d.csv")
pb_params |> filter(model == "M12_did_restoration")


# Load data ----
## calculate densities
# 1. Load the file and assign it to a variable name of your choice
pb_data <- readRDS("Data/PB_data/ef_removal_results_d_PB.rds")
#rm(pb_data)
str(pb_data, 1)
#View(my_data)

pb_chains_m12 <- do.call(rbind, pb_data$m12_samples)
colnames(pb_chains_m12)


# density- treatment ----
# filter data and recode
#pb_den <- pb_data$group_density |> filter(model == "mM12_did_restoration")


pb_den <- read.csv("Data/PB_data/M12_group_density.csv")
pb_den <- pb_den |> filter(!(species == "ASYOY" & year == 1992))
pb_den <- pb_den %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))

temp <- pb_den[17:18,]
temp[, c(5:9)] <- NA
temp[1:2, 2] <- "AS-0+"
pb_den <- rbind(pb_den, temp)

# set ylims
y_lims <- c(min(pb_den$q2.5, na.rm = T), max(pb_den$q97.5, na.rm = T))
dodge_w <- 0.5

pb_den_trt_split <- pb_den |> 
  split(pb_den$species)


## plot ----
plot_den <- map(names(pb_den_trt_split), function(species) {
  df <- pb_den_trt_split[[species]]  
  legend_theme <- if (unique(df$species) == "BT-1+") {
    theme(
      legend.position = c(0.7, 0.88),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.title = element_blank(),
      legend.key.size = unit(0.4, "cm")
    )
  } else {
    theme(legend.position = "none")
  }
  #df <- pb_den_stn_split$`AS-1+`
  ggplot(df, aes(x = factor(year), y = q50,
                 colour = factor(trt),
                 group  = factor(trt))) +
    pb_geoms() +
    theme_bw() + 
    ylab(y.axis) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    legend_theme +
    ylim(y_lims) + 
    pb_scales()
})

## combine ----
prefix <- "/pb_den_trt_"
plot_den_p <- pb_get_plots(pb_den_trt_split, plot_den)
save_plots(plot_den_p, prefix = prefix)
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y.axis, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 


## table ----
library(kableExtra)
library(tidyr)

pb_den$trt <- ifelse(pb_den$trt == "0", "above", "below")
pb_den$ci <- paste0("(", round(pb_den$q2.5, 2), ", ", round(pb_den$q97.5, 2), ")")
pb_den$mci <- paste0(round(pb_den$q50, 2), " ",  pb_den$ci)
pb_den_temp <- pb_den[, c(1:3, 10)]


pb_den_tab <- pivot_wider(pb_den,
                          id_cols = c(species, trt),
                          names_from = c(year),
                          values_from = c(mci)
)

write.csv(pb_den_tab, "Tables/PB_tabs/PB_density.csv", row.names = FALSE)
#ci_df <- read.csv("Figures/PB/PB_density.csv", check.names = FALSE)
#str(ci_df)

# make table
kbl(pb_den_tab,
    col.names = c('spp', 'trt',
                  '1990',
                  '1991',
                  '1992',
                  '1996',
                  '2016'),
    align = 'c', caption = "Density CrIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Year" = 5)) |>
  kable_paper()



# density by station -----
# filter data and recode
pb_den_stn <- pb_data$d |> filter(model == "M12_did_restoration")
pb_den_stn$species <- dplyr::recode(pb_den_stn$species, !!!species_code_map)
pb_den_stn <- pb_den_stn |> mutate(trt = if_else(station %in% c("6", "7"), "above", "below"))

pb_den_stn_split <- pb_den_stn |> 
  split(pb_den_stn$species)

# set ylims

plot_den_stn <- map(names(pb_den_stn_split), function(species) {
  df <- pb_den_stn_split[[species]]  
  #df <- pb_den_stn_split$`AS-1+`
  legend_theme <- if (unique(df$species) == "BT-1+") {
    theme(
      legend.position = c(0.3, 0.88),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.title = element_blank(),
      legend.key.size = unit(0.4, "cm")
    )
  } else {
    theme(legend.position = "none")
  }
  ggplot(df, aes(x = factor(year), y = q50,
                 colour = factor(trt),
                 group  = factor(trt))) +
    pb_geoms() +
    theme_bw() + 
    ylab(y.axis) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    geom_vline(xintercept = 3.5, linetype = "dashed", colour = "black") +
    ylim(min(pb_den_stn$q2.5), max(pb_den_stn$q97.5)) + 
    pb_scales_stn()
})

## combine ----
prefix <- "/pb_den_stn_"
plot_den_p <- pb_get_plots(pb_den_stn_split, plot_den_stn)
save_plots(plot_den_p, prefix = prefix)
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y.axis, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 



# beta-delta graphs -----
# file below is from One Drive, Project Keith/Pamehac/Outputs/no_asyoy_1992 and generated from Scripts/posthoc_did_effect_d.R  The file is an expansion of did_effect_m12...._d.csv
pb_trt_effect <- read.csv("Data/PB_data/delta_vs_did_M12_did_restoration_d.csv")
pb_trt_effect <- pb_trt_effect |> filter(quantity == "did_effect")

pb_trt_effect <- pb_trt_effect %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))

temp <- pb_trt_effect[5,]
temp[, c(1:8, 14)] <- NA
temp[1, 10:11] <- "AS-0+"
pb_trt_effect <- rbind(pb_trt_effect, temp)



# set ylims
y_lims <- c(min(pb_trt_effect$q2.5, na.rm = T), max(pb_trt_effect$q97.5, na.rm = T))
y_lab <- "Treatment Effect (ln scale)"

pb_trt_effect_split <- pb_trt_effect |> 
  split(pb_trt_effect$species)

# set ylims
## plot ----
plot_beta <- map(names(pb_trt_effect_split), function(species) {
  df <- pb_trt_effect_split[[species]]  
  legend_theme <- if (unique(df$species) == "BT-1+") {
    theme(
      legend.position = c(0.3, 0.88),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.title = element_blank(),
      legend.key.size = unit(0.4, "cm")
    )
  } else {
    theme(legend.position = "none")
  }
  #  df <- pb_trt_effect_split[pb_trt_effect_split$species == "AS-1+", ]
  ggplot(df, aes(x = factor(year), y = q50)) +
    pb_geoms1() +
    theme_bw() + 
    ylab(y.axis.trt) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "gray30") + 
    geom_text(aes(y = q97.5, label = round(prob_pos, 2)),
              vjust = -0.5, size = 2, colour = "black") +
    ylim(y_lims) + 
    legend_theme
})

## combine ----
prefix <- "/pb_beta_"
plot_den_p <- pb_get_plots(pb_trt_effect_split, plot_beta)
save_plots(plot_den_p, prefix = prefix)
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y_lab, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 


## percent change ----
## comparing difference between below and above
pb_trt_effect$per_chg <- (exp(pb_trt_effect$q50) - 1)*100
pb_trt_effect[, c(12, 11, 4, 16)] |> arrange(species, year)


# biomass ----
## from pb_biomass.R
#   biomass_density[i] = d[i] * w_bar[i] * 100   (g/100m^2)
#
#   where d[i] is the posterior density (fish/m^2) from M13c
#   and w_bar[i] is the mean weight (g) of fish caught at that
#   station x year x species/lifestage.

## trt ----
## pb_biomass.R in the Scripts folder - this code is clear and output is saved as csv files.  
# For each year x species x treatment group x method, compute
# the GROUP-MEAN biomass density at the MCMC iteration level:
#
#   At iteration iter:
#     group_mean_biomass[iter] = mean( biomass[i, iter] )
#                                for all stations i in that group
#

pb_bio_grp <- read.csv("Data/PB_data/pb_biomass_trt.csv")
temp <- pb_bio_grp[17:18,]
temp[, c(5:9)] <- NA
temp[1:2,3] <- "AS-0+"
temp[2, 4] <- "below"
pb_bio_grp <- rbind(pb_bio_grp, temp)

pb_bio_grp <- pb_bio_grp %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))

y.axis.bio <- expression("Biomass density (g/100 m" ^2*")")
y_lims <- c(min(pb_bio_grp$q2.5), max(pb_bio_grp$q97.5))
trt_colours <- c("above" = "black", "below" = "grey60")


pb_bio_grp_split <- pb_bio_grp |> 
  split(pb_bio_grp$species)


## plot ----
plot_bio <- map(names(pb_bio_grp_split), function(species) {
  df <- pb_bio_grp_split[[species]]  
  #df <- df_b_split$BTYOY
  legend_theme <- if (unique(df$species) == "BT-1+") {
    theme(
      legend.position = c(0.3, 0.88),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.title = element_blank(),
      legend.key.size = unit(0.4, "cm")
    )
  } else {
    theme(legend.position = "none")
  }
  #  df <- pb_bio_grp_split$`AS-1+`
  ggplot(df, aes(x = factor(year), y = q50,
                 colour = factor(trt),
                 group  = factor(trt))) +
    pb_geoms() +
    theme_bw() + 
    ylab(y.axis.bio) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    geom_vline(xintercept = 1.5, linetype = "dashed", colour = "black") +
    pb_scales_stn()
})

## combine ----
prefix <- "/pb_bio_trt_"
plot_den_p <- pb_get_plots(pb_bio_grp_split, plot_bio)
save_plots(plot_den_p, prefix = prefix)
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y.axis.bio, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 


## table ----
#pb_bio_grp$trt <- ifelse(pb_bio_grp$trt == "0", "above", "below")
pb_bio_grp$ci <- paste0("(", round(pb_bio_grp$q2.5, 1), ",", round(pb_bio_grp$q97.5, 1), ")")
pb_bio_grp$mci <- paste0(round(pb_bio_grp$q50, 1), " ",  pb_bio_grp$ci)
pb_bio_grp_temp <- pb_bio_grp[, c(2:4, 11)]


pb_bio_grp_tab <- pivot_wider(pb_bio_grp_temp,
                              id_cols = c(species, trt),
                              names_from = c(year),
                              values_from = c(mci)
)

write.csv(pb_bio_grp_tab, "Tables/PB_tabs/PB_biomass.csv", row.names = FALSE)

# make table
kbl(pb_bio_grp_tab,
    col.names = c('spp', 'trt',
                  '1990',
                  '1991',
                  '1992',
                  '1996',
                  '2016'),
    align = 'c', caption = "Biomass density CrIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Year" = 5)) |>
  kable_paper()



# contrast ----
# Deprecated - not needed bc comparison with beta-delta graphs is just a scalar
# For each year x species x method, compute the DIFFERENCE in
# group-mean biomass: treatment minus control, at each MCMC
# iteration:
#
#   diff[iter] = mean(biomass_trt[iter]) - mean(biomass_ctrl[iter])

# pb_bio_con <- read.csv("Figures/pb_data/pb_biomass_trt_diff.csv")
# temp <- pb_bio_con[9,]
# temp[, c(4:8)] <- NA
# temp[1,3] <- "AS-0+"
# pb_bio_con <- rbind(pb_bio_con, temp)
# 
# pb_bio_con <- pb_bio_con %>%
#   mutate(species = recode(species,
#                           "AS" = "AS-1+",
#                           "ASYOY" = "AS-0+",
#                           "BT" = "BT-1+",
#                           "BTYOY" = "BT-0+"))
# 
# 
# y_lims <- c(min(pb_bio_con$q2.5), max(pb_bio_con$q97.5))
# 
# pb_bio_con_split <- pb_bio_con|> 
#   split(pb_bio_con$species)
# 
# ## plot ----
# plot_bio <- map(names(pb_bio_con_split), function(species) {
#   df <- pb_bio_con_split[[species]]  
#   #df <- df_b_split$BTYOY
#   legend_theme <- if (unique(df$species) == "BT-1+") {
#     theme(
#       legend.position = c(0.3, 0.88),
#       legend.background = element_rect(fill = "transparent", color = NA),
#       legend.title = element_blank(),
#       legend.key.size = unit(0.4, "cm")
#     )
#   } else {
#     theme(legend.position = "none")
#   }
#   #  df <- pb_bio_grp_split$`AS-1+`
#   ggplot(df, aes(x = factor(year), y = q50)) +
#     pb_geoms() +
#     theme_bw() + 
#     ylab(y.axis.trt) +
#     xlab("Year") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     #theme(legend.position= "none") +
#     legend_theme +
#     #geom_hline(yintercept = 0, linetype = "dashed", colour = "gray30") +
#     pb_scales()
# })
# 
# ## combine ----
# prefix <- "/pb_bio_con_"
# plot_den_p <- pb_get_plots(pb_bio_grp_split, plot_bio)
# save_plots(plot_den_p, prefix = prefix)
# final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y_lab, prefix = paste0(prefix, "comb"))
# final_plot_den


# Param Tables ----

# this does log_mu and sigma_d
m12_mu_cri <- extract_cri(pb_data$m12_samples, "^log_mu_d\\[|^sigma_d\\[") |>
  mutate(
    model   = "M12_did_restoration",
    sp_idx  = as.integer(str_extract(param, "\\d+")),
    species = sl_levels[sp_idx]
  )

mu_p_cri <- quantile(pb_chains_m12[, 'mu_p'], probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                     na.rm = TRUE)
sig_p_cri <- quantile(pb_chains_m12[, 'sigma_p'], probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                      na.rm = TRUE)

# make dataframes
m12_mu_cri[, -7]
mu_p_cri_df <- data.frame(
  param = "mu_p",
  q2.5  = mu_p_cri[1],
  q25   = mu_p_cri[2],
  q50   = mu_p_cri[3],
  q75   = mu_p_cri[4],
  q97.5 = mu_p_cri[5]
)
sig_p_cri_df <- data.frame(
  param = "sig_p",
  q2.5  = sig_p_cri[1],
  q25   = sig_p_cri[2],
  q50   = sig_p_cri[3],
  q75   = sig_p_cri[4],
  q97.5 = sig_p_cri[5]
)

# look ups
year_levels <- c(1990, 1991, 1992, 1996, 2016)
year_lookup_m12 <- tibble(year_idx = seq_along(year_levels),
                          year     = year_levels)
post_year_levels <- c(1991, 1992, 1996, 2016)
post_year_lookup <- tibble(
  post_year_idx = seq_along(post_year_levels),
  year          = post_year_levels
)

# Annual random effect on log-density (control sites only at PB, all sites at RB and GC)
year_re_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(pb_data$m12_samples, paste0("^alpha_year\\[.*,", sl, "\\]")) |>
    mutate(
      year_idx = as.integer(str_extract(param, "(?<=\\[)\\d+")),
      label    = species_code_map[sl],
      species  = sl_levels[sl]
    ) |>
    left_join(year_lookup_m12, by = "year_idx")
}) |> mutate(model = "M12_did_restoration")
year_re_cri <- year_re_cri |> filter(!(year == 1992 & species == "ASYOY"))

sigma_year_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(pb_data$m12_samples, paste0("^sigma_year\\[", sl, "\\]")) |>
    mutate(label = species_code_map[sl], species = sl_levels[sl])
}) |> mutate(model = "M12_did_restoration")


# Annual effect for treatment sites in post restoration years
m12_delta <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(pb_data$m12_samples, paste0("^delta_year\\[.*,", sl, "\\]")) |>
    mutate(
      post_year_idx = as.integer(str_extract(param, "(?<=\\[)\\d+")),
      label         = species_code_map[sl],
      species       = sl_levels[sl]
    ) |>
    left_join(post_year_lookup, by = "post_year_idx")
}) |>
  mutate(
    model        = "M12_did_restoration",
    prob_pos     = NA_real_,   # filled below from raw draws
    interpretation = NA_character_
  )
m12_delta <- m12_delta |> filter(!(year == 1992 & species == "ASYOY"))

sigma_delta_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(pb_data$m12_samples, paste0("^sigma_delta\\[", sl, "\\]")) |>
    mutate(label = species_code_map[sl], species = sl_levels[sl])
}) |> mutate(model = "M12_did_restoration")



## complie table ----
library(kableExtra)

# create dataframe
year_re_cri[, 1:10]
sigma_year_cri[, 1:8]
m12_delta[, 1:10]
sigma_delta_cri[, 1:8]


param_df <- bind_rows(m12_mu_cri[, -7],
                      mu_p_cri_df,
                      sig_p_cri_df,
                      year_re_cri[, 1:10],
                      sigma_year_cri[, 1:8],
                      m12_delta[, 1:10],
                      sigma_delta_cri[, 1:8]
)
rownames(param_df) <- NULL

write.csv(param_df, "Tables/PB_tabs/PB_param_tab.csv")

ft_df <- param_df |>
  select(param, species, year, q2.5, q25, q50, q75, q97.5) |>
  mutate(across(where(is.numeric), ~ round(.x, digits = 2)))
ft_df$year <- as.integer(ft_df$year)

names(ft_df) <- c("Parameters", "species", "year",
                  "2.5%", "25%", "50%", "75%", "97.5%")

# make table
kbl(param_df[, c(1, 8, 11, 2:6)],
    col.names = c('Parameters', 
                  "species",
                  "year",
                  '2.5%',
                  '25%',
                  '50%',
                  '75%',
                  '97.5%' 
    ),
    align = 'c', caption = "Parameter CrIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 1, "Quantiles" = 5, " " = 2)) |>
  kable_paper()


# END ----