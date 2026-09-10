## This file is to create the figures and documents for the synthesis paper.  Its based on the hierarchical modelling of Guilluame Dauphin.

# this code was originally writen in "Project Keith" but needed a cleaner version.

# the purpose of this file is to create publication quality graphics for the hierarchical models for the Synthesis paper for Rose Blanche.

# Objects used below are all extracted from the *rds file but sometimes, its necessary to extract values from the chains in order to get the probability of being a positive value  (imported chain summaries); 

# biomass are from csv files generated from RB_biomass.R

#  Labels (inherited from M11)

#put this file in the folder with the project and create the following subfolders
if (!dir.exists("Data/GC_data")) {
  dir.create("Data/GC_data", recursive = TRUE)
}
if(!dir.exists("Figures/GC_figs")){
  dir.create("Figures/GC_figs", recursive = TRUE)
} #for publication quality only
if(!dir.exists("Tables/GC_tabs")){
  dir.create("Tables/GC_tabs", recursive = TRUE)
} #for publication quality only



# libraries and source -----
library(tidyverse)
library(cowplot)


# Source 
#source("Scripts/data_prep_d.R")
source("functionsKL.R")

# ---- helpers (used by model scripts and this file) ----
outputs_path <- function(f) file.path("Figures/GC_figs", f)

# helper objects
## used for axis labels, changing 
y.axis <- expression("Density (fish/ m" ^2*")")
y.axis.bio <- expression("Biomass density (g/100 m" ^2*")")
y.axis.trt <- "Treatment Effect (ln scale)"
y.axis.bio.diff <- expression("Differences in biomass density (g/100 m" ^2*")")

sl_levels <- c("AS", "ASYOY", "BT", "BTYOY")
#trt_colours <- c("Control" = "black", "Treatment" = "grey60")
trt_labels <- c("Control", "Treatment")
group_colours <- c("MC-Riffle" = "black", "MC-Run" = "black", "SC-Run" = "gray")
group_shapes <- c("MC-Riffle" = 16, "MC-Run" = 17, "SC-Run" = 17)
group_labels <- c("MC-Riffle", "MC-Run", "SC-Run")

# convert species codes to labels for plotting
species_code_map <- c(
  "AS"    = "AS-1+",
  "ASYOY" = "AS-0+",
  "BT"    = "BT-1+",
  "BTYOY" = "BT-0+"
)


# Load data ----
## calculate densities
# 1. Load the file and assign it to a variable name of your choice
gc_data <- readRDS("Data/GC_data/ef_removal_results_GC.rds")
#rm(rb_data)
str(gc_data, 1)
#View(my_data)

gc_chains_m13c <- do.call(rbind, gc_data$m13c_samples)


# Graphs
# density- treatment ----
# filter data and recode
#gc_den <- gc_data$group_density |> filter(model == "m13c_GC_hier_hab"& year != 2006)
gc_den <- gc_data$group_density |> filter(model == "m13c_GC_hier_hab")
gc_den$species <- dplyr::recode(gc_den$species, !!!species_code_map)


# set ylims
y_lims <- c(min(gc_den$q2.5), max(gc_den$q97.5))
dodge_w <- 0.5

gc_den_split <- gc_den |> 
  split(gc_den$species)

levels(factor(gc_den$hab_type))

## plot ----
plot_den <- map(names(gc_den_split), function(species) {
  df <- gc_den_split[[species]]  
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
  #df <- rb_den_split$`AS-1+`
  ggplot(df, aes(x = factor(year), y = q50,
                 colour = factor(hab_type),
                 group  = factor(hab_type),
                 shape = factor(hab_type)
  )) +
    gc_geoms1() +
    theme_bw() + 
    ylab(y.axis) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    #ylim(y_lims) + 
    gc_scales()
})

## combine  ----
prefix <- "/gc_den_trt_"
plot_den_p <- pb_get_plots(gc_den_split, plot_den)
save_plots(plot_den_p, prefix = prefix, out_dir = "Figures/GC_figs")
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y.axis, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 




## table ----
library(kableExtra)
library(tidyr)

##### CHANGE THIS TO GC, NOT RB
gc_den$ci <- paste0("(", round(gc_den$q2.5, 3), ", ", round(gc_den$q97.5, 3), ")")
gc_den$mci <- paste0(round(gc_den$q50, 3), " ",  gc_den$ci)
gc_den_temp <- gc_den[, c(1:3, 12)]


gc_den_tab <- pivot_wider(gc_den_temp,
                          id_cols = c(species, hab_type),
                          names_from = c(year),
                          values_from = c(mci)
)

write.csv(gc_den_tab, "Tables/GC_tabs/GC_density.csv", row.names = FALSE)

# make table
# this works but have created tables in tab_GC_parameter.Rmd
kbl(gc_den_tab,
    col.names = c('spp', 'trt',
                  '2004',
                  '2005',
                  '2007',
                  '2009',
                  '2015'),
    align = 'c', caption = "Density CrIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Year" = 5)) |>
  kable_paper()



# density by station -----
# filter data and recode
gc_den_stn <- gc_data$d |> filter(model == "m13c_GC_hier_hab" & year != 2006)
gc_den_stn$species <- dplyr::recode(gc_den_stn$species, !!!species_code_map)

gc_den_stn_split <- gc_den_stn |> 
  split(gc_den_stn$species)

# set ylims
y_lims <- c(min(gc_den$q2.5), max(gc_den$q97.5))

plot_den_stn <- map(names(gc_den_stn_split), function(species) {
  df <- gc_den_stn_split[[species]]  
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
  #df <- gc_den_stn_split$`AS-1+`
  ggplot(df, aes(x = factor(year), y = q50,
                 colour = factor(hab_type),
                 group  = factor(hab_type),
                 shape  = factor(hab_type)
  )) +
    gc_geoms1() +
    theme_bw() + 
    ylab(y.axis) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    #ylim(y_lims) + 
    gc_scales()
  
})

## combine  ----
prefix <- "/gc_den_stn_"
plot_den_p <- pb_get_plots(gc_den_stn_split, plot_den_stn)
save_plots(plot_den_p, prefix = prefix, out_dir = "Figures/GC_figs")
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y.axis, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 



# beta-delta graphs -----
# Had to use the chains to calculate the probability of positive value; this may have been saved in GD's code but I couldn't find it.

# this is just a test to see that I get the same beta values as the beta_summary below
gc_beta_test <- gc_data$hab_effects |> filter(model == "m13c_GC_hier_hab" & year != 2006) # this lacks MC-runs - is it a contrast?? But its based on M13 beta....so this should be deviations from MC-Run which is what we wanted.  Yes.
gc_beta_test$species <- dplyr::recode(gc_beta_test$species, !!!species_code_map)


# set years and compute quantiles
beta_years <- c(2004, 2005, 2006, 2007, 2009, 2015) 
beta_summary <- summarize_chains_by_index1(
  chains         = gc_chains_m13c,
  year_levels    = beta_years,
  hab_levels     = c("MC-Run", "MC-Riffle", "SC-Run"),   # or however you index habitat 1:3
  var            = "beta_hab_year",
  species_code_map = species_code_map
)
beta_summary <- beta_summary |> arrange(species, habitat, year) |> filter(year != 2006 & habitat != "MC-Run") |> print(n = Inf)


# compute probability > 0 and combine
prob_pos_summary <- calc_prob_positive1(
  chains         = gc_chains_m13c,
  year_levels    = beta_years,
  hab_levels     = c("MC-Run", "MC-Riffle", "SC-Run"),   # or however you index habitat 1:3
  var            = "beta_hab_year",
  species_code_map = species_code_map
)
prob_pos_summary <- prob_pos_summary |> arrange(species, habitat, year) |> filter(year != 2006 & habitat != "MC-Run") |> print(n = Inf)
gc_beta <- dplyr::left_join(beta_summary, prob_pos_summary,
                            by = c("year", "species", 'habitat'))

# set ylims
y_lims <- c(min(gc_beta$q2.5), max(gc_beta$q97.5))
y_lab <- "Treatment Effect (ln scale)"

gc_beta_split <- gc_beta |> 
  split(gc_beta$species)

# set ylims
## plot ----
plot_beta <- map(names(gc_beta_split), function(species) {
  df <- gc_beta_split[[species]]  
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
  #  df <- gc_beta_split$`AS-1+`
  ggplot(df, aes(x = factor(year), y = q50,
                 colour = factor(habitat),
                 group  = factor(habitat),
                 shape = factor(habitat)
  )) +
    gc_geoms1() + 
    theme_bw() + 
    ylab(y.axis.trt) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "gray30") + 
    geom_text(aes(y = q97.5, label = round(prob_pos, 2)),
              vjust = -0.5, size = 2, colour = "black", position = position_dodge2(width = dodge_w)) +
    legend_theme + 
    gc_scales()
})


## combine  ----
prefix <- "/gc_beta_"
plot_den_p <- pb_get_plots(gc_beta_split, plot_beta)
save_plots(plot_den_p, prefix = prefix, out_dir = "Figures/GC_figs")
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y.axis.trt, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 


## percent change ----
gc_beta_test$per_chg <- (exp(gc_beta_test$q50) - 1)*100
gc_beta_test[, -c(11:13)] |> print(n=Inf)


# biomass ----
## from RB_biomass.R
#   biomass_density[i] = d[i] * w_bar[i] * 100   (g/100m^2)
#
#   where d[i] is the posterior density (fish/m^2) from M13c
#   and w_bar[i] is the mean weight (g) of fish caught at that
#   station x year x species/lifestage.

## trt ----
## RB_biomass.R in the Scripts folder - this code is clear and output is saved as csv files.  
# For each year x species x treatment group x method, compute
# the GROUP-MEAN biomass density at the MCMC iteration level:
#
#   At iteration iter:
#     group_mean_biomass[iter] = mean( biomass[i, iter] )
#                                for all stations i in that group
#

gc_bio_grp <- read.csv("Data/GC_data/biomass_group_gc.csv")

gc_bio_grp <- gc_bio_grp |>
  #filter(method == "point" & year != 2006)
  filter(method == "point")

gc_bio_grp <- gc_bio_grp %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))


y_lims <- c(min(gc_bio_grp$q2.5), max(gc_bio_grp$q97.5))

#trt_colours <- c("Control" = "black", "Treatment" = "grey60")
#rb_bio_grp$group_label <- ifelse(rb_bio_grp$trt == 0, "Control", "Treatment")

gc_bio_grp_split <- gc_bio_grp |> 
  split(gc_bio_grp$species)


## plot ----
plot_bio <- map(names(gc_bio_grp_split), function(species) {
  df <- gc_bio_grp_split[[species]]  
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
  #  df <- rb_bio_grp_split$`AS-1+`
  ggplot(df, aes(x = factor(year), y = q50,
                 colour = factor(hab_type),
                 group  = factor(hab_type),
                 shape = factor(hab_type)
  )) +
    gc_geoms1() +
    theme_bw() + 
    ylab(y.axis.bio) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    #ylim(y_lims) + 
    gc_scales()
})


## combine  ----
prefix <- "/gc_bio_trt_"
plot_den_p <- pb_get_plots(gc_bio_grp_split, plot_bio)
save_plots(plot_den_p, prefix = prefix, out_dir = "Figures/GC_figs")
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y.axis.bio, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 


## percent change ----
# 1. Get q50 wide by hab_type
q50_wide <- gc_bio_grp %>%
  select(year, species, label, hab_type, q50) %>%
  pivot_wider(names_from = hab_type, values_from = q50)

gc_bio_grp <- gc_bio_grp %>%
  left_join(q50_wide, by = c("year", "species", "label")) %>%
  mutate(
    pctdiff_q50 = case_when(
      hab_type == "MC-Riffle" ~ (`MC-Riffle` - `MC-Run`) / `MC-Run` * 100,
      hab_type == "SC-Run" ~ (`SC-Run` - `MC-Run`) / `MC-Run` * 100,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-`MC-Run`, -`MC-Riffle`, -`SC-Run`)  # drop the helper columns
gc_bio_grp[, -c(4, 6, 8, 12, 13)]
gc_bio_grp[50:60, -c(3, 5, 7, 9, 12, 13)]
(336.868767 -21.884733)/21.884733*100


### Table ----
library(kableExtra)
library(tidyr)

gc_bio_grp <- read.csv("Data/GC_data/biomass_group_gc.csv")

gc_bio_grp <- gc_bio_grp |>
  # filter(method == "point" & year != 2006)
  filter(method == "point")

gc_bio_grp <- gc_bio_grp %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))


gc_bio_grp$ci <- paste0("(", round(gc_bio_grp$q2.5, 1), ", ", round(gc_bio_grp$q97.5, 1), ")")
gc_bio_grp$mci <- paste0(round(gc_bio_grp$q50, 1), " ",  gc_bio_grp$ci)
gc_bio_grp <- gc_bio_grp[, c(1, 3:4, 15)]


gc_bio_grp_tab <- pivot_wider(gc_bio_grp,
                              id_cols = c(label, hab_type),
                              names_from = c(year),
                              values_from = c(mci)
)

write.csv(gc_bio_grp_tab, "Tables/GC_tabs/GC_biomass.csv", row.names = FALSE)

# make table
# this works but have created tables in tab_GC_parameter.Rmd
kbl(gc_bio_grp_tab,
    col.names = c('spp', 'hab_type',
                  '2004',
                  '2005',
                  '2007',
                  '2009',
                  '2015'),
    align = 'c', caption = "Density CrIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Year" = 5)) |>
  kable_paper()


# contrast ----
# For each year x species x method, compute the DIFFERENCE in
# group-mean biomass: treatment minus control, at each MCMC
# iteration:
#
#   diff[iter] = mean(biomass_trt[iter]) - mean(biomass_ctrl[iter])
gc_bio_con <- read.csv("Data/GC_data/biomass_contrast_gc.csv")

gc_bio_con <- gc_bio_con |>
  filter(method == "point" & year != 2006)

gc_bio_con <- gc_bio_con %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))

y_lims <- c(min(gc_bio_con$q2.5), max(gc_bio_con$q97.5))


gc_bio_con_split <- gc_bio_con|> 
  split(gc_bio_con$species)

## plot ----
plot_bio <- map(names(gc_bio_con_split), function(species) {
  df <- gc_bio_con_split[[species]]  
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
  #  df <- rb_bio_grp_split$`AS-1+`
  ggplot(df, aes(x = factor(year), y = q50,
                 colour = factor(hab_contrast),
                 group  = factor(hab_contrast),
                 shape = factor(hab_contrast)
  )) +
    gc_geoms1() +
    theme_bw() + 
    ylab(y.axis.bio.diff) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    geom_text(aes(y = q97.5, label = round(p_hab_gt_ref, 2)),
              vjust = -0.5, size = 2, colour = "black", position = position_dodge2(width = dodge_w)) +
    legend_theme +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "gray30") +
    gc_scales()
})

## combine  ----
prefix <- "/gc_bio_con_"
plot_den_p <- pb_get_plots(gc_bio_con_split, plot_bio)
save_plots(plot_den_p, prefix = prefix, out_dir = "Figures/GC_figs")
final_plot_den <- make_grid_plot(plot_den_p, y_axis_label = y.axis.bio.diff, prefix = paste0(prefix, "comb"))
final_plot_den  # prints it, 

## percent change ----
# don't use exp percent chnage because these are differences, not parameter estimates!!!!
# use gc_bio_grp


# Param Tables ----
## cri ----
#gc_data$m13c_samples
#gc_chains_m13c

# this does log_mu and sigma_d
m13_mu_cri <- extract_cri(gc_data$m13c_samples, "^log_mu_d\\[|^sigma_d\\[") |>
  mutate(
    model   = "M13c_gc",
    sp_idx  = as.integer(str_extract(param, "\\d+")),
    species = sl_levels[sp_idx]
  )

mu_p_cri <- quantile(gc_chains_m13c[, 'mu_p'], probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                     na.rm = TRUE)
sig_p_cri <- quantile(gc_chains_m13c[, 'sigma_p'], probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                      na.rm = TRUE)

year_levels <- c(2004, 2005, 2006, 2007, 2009, 2016)
year_lookup_m13 <- tibble(year_idx = seq_along(year_levels),
                          year     = year_levels)

hab_levels <- c("MC-Riffle", "MC-Run", "SC-Run")
hab_lookup_m13 <- tibble(hab_idx = seq_along(hab_levels),
                         hab     =hab_levels)

# Annual random effect on log-density (control sites only at PB, all sites at RB and GC)
year_re_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(gc_data$m13c_samples, paste0("^alpha_year\\[.*,", sl, "\\]")) |>
    mutate(
      year_idx = as.integer(str_extract(param, "(?<=\\[)\\d+")),
      label    = species_code_map[sl],
      species  = sl_levels[sl]
    ) |>
    left_join(year_lookup_m13, by = "year_idx")
}) |> mutate(model = "M13c_gc")
year_re_cri <- year_re_cri |> filter(!(year == 2006))

sigma_year_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(gc_data$m13c_samples, paste0("^sigma_year\\[", sl, "\\]")) |>
    mutate(label = species_code_map[sl], species = sl_levels[sl])
}) |> mutate(model = "M13c_gc")


# Annual effect for treatment sites in post restoration years
m13_beta_hab <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(gc_data$m13c_samples, paste0("^beta_hab_year\\[\\d+,\\d+,", sl, "\\]")) |>
    mutate(
      year_idx = as.integer(str_extract(param, "(?<=\\[)\\d+")),
      hab_idx  = as.integer(str_extract(param, "(?<=,)\\d+(?=,\\d+\\])")),
      label         = species_code_map[sl],
      species       = sl_levels[sl]
    ) |>
    left_join(year_lookup_m13, by = "year_idx")
}) |>
  mutate(
    model        = "M13c_gc",
    prob_pos     = NA_real_,   # filled below from raw draws
    interpretation = NA_character_
  ) |> 
  left_join(hab_lookup_m13, by = "hab_idx")
m13_beta_hab <- m13_beta_hab |> filter(!(year == 2006))

prob_pos_summary <- calc_prob_positive_gc(
  chains = gc_chains_m13c, 
  year_levels = year_levels,
  species_levels = sl_levels, 
  hab_levels     = c("MC-Run", "MC-Riffle", "SC-Run"),
  var = "beta_hab_year",
  species_code_map = species_code_map
)
prob_pos_summary <- prob_pos_summary |> arrange(species, hab_type, year) |> filter(year != 2006 & hab_type != "MC-Run") |> print(n = Inf)
m13_beta_hab <- dplyr::left_join(m13_beta_hab[, -13], prob_pos_summary,
                                 by = c("year" = "year", "hab" = "hab_type", "label" = "species"))


sigma_beta_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(gc_data$m13c_samples, paste0("^sigma_beta\\[", sl, "\\]")) |>
    mutate(label = species_code_map[sl], species = sl_levels[sl])
}) |> mutate(model = "M13c_gc")


# convert matrix to df for binding
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

year_re_cri[, 1:10]
sigma_year_cri[, 1:8]
m13_beta_hab[, c(1:6, 9, 11)]
sigma_beta_cri[, 1:8]


param_df <- bind_rows(m13_mu_cri[, -7],
                      mu_p_cri_df,
                      sig_p_cri_df,
                      year_re_cri[, 1:10],
                      sigma_year_cri[, 1:8],
                      m13_beta_hab[, c(1:6, 10:11, 15)],
                      sigma_beta_cri[, 1:8]
)
rownames(param_df) <- NULL
write.csv(param_df, "Tables/GC_tabs/GC_param_tab.csv")
ft_df <- param_df[, c(1, 8, 11, 2:6)]
ft_df <- ft_df |>
  mutate(across(where(is.numeric), ~ round(.x, digits = 2)))
names(ft_df) <- c("Parameters", "species", "year",
                  "2.5%", "25%", "50%", "75%", "97.5%")

## table ----
# this works but have created tables in tab_GC_parameter.Rmd
library(kableExtra)

# make table
kbl(param_df[, c(1, 8, 11, 12, 2:6)],
    col.names = c('Parameters', 
                  "species",
                  "year",
                  "hab",
                  '2.5%',
                  '25%',
                  '50%',
                  '75%',
                  '97.5%' 
    ),
    align = 'c', caption = "Parameter CrIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 4, "Quantiles" = 5)) |>
  kable_paper()




# END ----