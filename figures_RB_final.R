## This file is to create the figures and documents for the synthesis paper.  Its based on the hierarchical modelling of Guilluame Dauphin.

# this code was originally writen in "Project Keith" but needed a cleaner version.

# the purpose of this file is to create publication quality graphics for the hierarchical models for the Synthesis paper for Rose Blanche.

# Objects used below are all extracted from the *rds file but sometimes, its necessary to extract values from the chains in order to get the probability of being a positive value  (imported chain summaries); 

# biomass are from csv files generated from RB_biomass.R

#  Labels (inherited from M11)

#put this file in the folder with the project and create the following subfolders
if (!dir.exists("Data/RB_data")) {
  dir.create("Data/RB_data", recursive = TRUE)
}
if(!dir.exists("Figures/RB_figs")){
  dir.create("Figures/RB_figs", recursive = TRUE)
} #for publication quality only
if(!dir.exists("Tables/RB_tabs")){
  dir.create("Tables/RB_tabs", recursive = TRUE)
} #for publication quality only



# libraries and source -----
library(tidyverse)
library(cowplot)
source("functionsKL.R")

# ---- helpers (used by model scripts and this file) ----
outputs_path <- function(f) file.path("Figures/RB_figs", f)

# helper objects
## used for axis labels, changing 
y.axis <- expression("Density (fish/ m" ^2*")")
y.axis.bio <- expression("Biomass density (g/100 m" ^2*")")
y.axis.trt <- "Treatment Effect (ln scale)"

sl_levels <- c("AS", "ASYOY", "BT", "BTYOY")
#trt_colours <- c("Control" = "black", "Treatment" = "grey60")
trt_labels <- c("Control", "Treatment")

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
rb_data <- readRDS("Data/RB_data/ef_removal_results_RB.rds")
#rm(rb_data)
str(rb_data, 1)
#View(my_data)

rb_chains_m13c <- do.call(rbind, rb_data$m13c_samples)


# density- treatment ----
# filter data and recode
rb_den <- rb_data$group_density |> filter(model == "m13c_CI_hier_trt")
rb_den$species <- dplyr::recode(rb_den$species, !!!species_code_map)
rb_den$group_label <- ifelse(rb_den$trt == 0, "Control", "Treatment")

# set ylims
y_lims <- c(min(rb_den$q2.5), max(rb_den$q97.5))
dodge_w <- 0.5

rb_den_split <- rb_den |> 
  split(rb_den$species)

## plot ----
plot_den <- map(names(rb_den_split), function(species) {
  df <- rb_den_split[[species]]  
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
                 colour = factor(trt),
                 group  = factor(trt))) +
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_point(size = 2.25, alpha = 0.6, position = position_dodge2(width = dodge_w)) +
    theme_bw() + 
    ylab(y.axis) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    geom_vline(xintercept = 3.5, linetype = "dashed", colour = "black") +
    
    ylim(min(rb_den$q2.5), max(rb_den$q97.5)) + 
    scale_fill_discrete(name="",
                        breaks=c(0, 1),
                        labels=c(c)) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c(0, 1),
                        labels=c("Control", "Treatment")) +
    scale_shape_manual(values = c(16, 16),
                       name="",
                       breaks=c(0, 1),
                       labels=c("Control", "Treatment"))
  
})


names(plot_den) <- paste0(names(rb_den_split))
list2env(plot_den, envir = .GlobalEnv)
p1 <- plot_den$`AS-1+`
p2 <- plot_den$`BT-1+`
p3 <- plot_den$`BT-0+`
p4 <- plot_den$`AS-0+`

# save iteratively
plots <- list(
  `AS-1+` = p1,
  `BT-1+` = p2,
  `BT-0+` = p3,
  `AS-0+` = p4
)

prefix <- "/rb_den_trt_"

for (nm in names(plots)) {
  ggsave(
    filename = outputs_path(paste0(prefix, nm, ".png")),
    plot = plots[[nm]],
    width = 6, height = 4, dpi = 300
  )
}

#### combine ----
p1_clean <- p1 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p2_clean <- p2 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p3_clean <- p3 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p4_clean <- p4 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))

grid_den <- plot_grid(p4_clean, 
                      p1_clean,
                      p3_clean, 
                      p2_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      #labels = c("AS", "ASY","BT", "BTY"),
                      labels = c("AS-0+",
                                 "AS-1+",
                                 "BT-0+",
                                 "BT-1+"),
                      # hjust = -3, 
                      hjust = -1.5, 
                      vjust = 1.25)
grid_den
# Add shared axis labels
final_plot_den <- ggdraw(grid_den) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(expression("Density Estimate (#/100 m" ^2*")"), x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14) +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
final_plot_den

save_plot(outputs_path(paste0(prefix, ".png")), 
          final_plot_den, 
          base_height = 6, 
          base_width = 10,
          bg = "white")


## table ----
library(kableExtra)
library(tidyr)

rb_den$trt <- ifelse(rb_den$trt == "0", "Control", "Treatment")
rb_den$ci <- paste0("(", round(rb_den$q2.5, 3), ", ", round(rb_den$q97.5, 3), ")")
#rb_den$ci <- paste0(round(rb_den$q2.5, 3), ", ", round(rb_den$q97.5, 3))
rb_den$mci <- paste0(round(rb_den$q50, 3), " ",  rb_den$ci)
rb_den_temp <- rb_den[, c(1:3, 13)]


rb_den_tab <- pivot_wider(rb_den_temp,
                          id_cols = c(species, trt),
                          names_from = c(year),
                          values_from = c(mci)
)

write.csv(rb_den_tab, "Tables/RB_tabs/RB_density.csv", row.names = FALSE)

# make table
kbl(rb_den_tab,
    col.names = c('spp', 'trt',
                  '2000',
                  '2001',
                  '2002',
                  '2016'),
    align = 'c', caption = "Density CrIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Year" = 4)) |>
  kable_paper()


# density by station -----
# filter data and recode
rb_den_stn <- rb_data$d |> filter(model == "m13c_CI_hier_trt")
rb_den_stn$species <- dplyr::recode(rb_den_stn$species, !!!species_code_map)
rb_den_stn <- rb_den_stn |> mutate(trt = if_else(station %in% c("8", "9", "10"), "cntrl", "trt"))

rb_den_stn_split <- rb_den_stn |> 
  split(rb_den_stn$species)

# set ylims

plot_den_stn <- map(names(rb_den_stn_split), function(species) {
  df <- rb_den_stn_split[[species]]  
  #df <- rb_beta_split$`AS-1+`
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
                 colour = factor(trt, labels = trt_labels),
                 group  = factor(trt))) +
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_point(size = 2.25, alpha = 0.6, position = position_dodge2(width = dodge_w)) +
    theme_bw() + 
    ylab(y.axis) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    geom_vline(xintercept = 3.5, linetype = "dashed", colour = "black") +
    ylim(min(rb_den_stn$q2.5), max(rb_den_stn$q97.5)) + 
    scale_fill_discrete(name="",
                        breaks=c("Control", "Treatment"),
                        labels=c(c)) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("Control", "Treatment"),
                        labels=c("Control", "Treatment")) +
    scale_shape_manual(values = c(16, 16),
                       name="",
                       breaks=c("Control", "Treatment"),
                       labels=c("Control", "Treatment"))
  
})


names(plot_den_stn) <- paste0(names(rb_den_split))
list2env(plot_den_stn, envir = .GlobalEnv)
p1 <- plot_den_stn$`AS-1+`
p2 <- plot_den_stn$`BT-1+`
p3 <- plot_den_stn$`BT-0+`
p4 <- plot_den_stn$`AS-0+`

# save iteratively
plots <- list(
  `AS-1+` = p1,
  `BT-1+` = p2,
  `BT-0+` = p3,
  `AS-0+` = p4
)

prefix <- "/rb_den_stn_"

for (nm in names(plots)) {
  ggsave(
    filename = outputs_path(paste0(prefix, nm, ".png")),
    plot = plots[[nm]],
    width = 6, height = 4, dpi = 300
  )
}

#### combine ----
p1_clean <- p1 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p2_clean <- p2 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p3_clean <- p3 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p4_clean <- p4 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))

grid_den <- plot_grid(p4_clean, 
                      p1_clean,
                      p3_clean, 
                      p2_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      #labels = c("AS", "ASY","BT", "BTY"),
                      labels = c("AS-0+",
                                 "AS-1+",
                                 "BT-0+",
                                 "BT-1+"),
                      # hjust = -3, 
                      hjust = -1.5, 
                      vjust = 1.25)
grid_den
# Add shared axis labels
final_plot_den <- ggdraw(grid_den) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(expression("Density Estimate (#/100 m" ^2*")"), x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14) +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
final_plot_den

save_plot(outputs_path(paste0(prefix, ".png")), 
          final_plot_den, 
          base_height = 6, 
          base_width = 10,
          bg = "white")


# beta-delta graphs -----
# Had to use the chains to calculate the probability of positive value; this may have been saved in GD's code but I couldn't find it.

# this is just a test to see that I get the same beta values as the beta_summary below
rb_beta_test <- rb_data$trt_effects |> filter(model == "m13c_CI_hier_trt")
rb_beta_test$species <- dplyr::recode(rb_beta_test$species, !!!species_code_map)

str(rb_chains_m13c)
colnames(rb_chains_m13c)

# set years and compute quantiles
beta_years <- c(2000, 2001, 2002, 2015) 
beta_summary <- summarize_chains_by_index(
  chains         = rb_chains_m13c,
  year_levels    = beta_years,
  species_levels = sl_levels,
  var            = "beta_year",
  species_code_map = species_code_map
)

# compute probability > 0 and combine
prob_pos_summary <- calc_prob_positive(
  chains = rb_chains_m13c, 
  year_levels = beta_years,
  species_levels = sl_levels, 
  var = "beta_year",
  species_code_map = species_code_map
)

rb_beta <- dplyr::left_join(beta_summary, prob_pos_summary,
                            by = c("year", "species"))

# set ylims
y_lims <- c(min(rb_beta$q2.5), max(rb_beta$q97.5))
y_lab <- "Treatment Effect (ln scale)"
rb_beta_split <- rb_beta |> 
  split(rb_beta$species)

# set ylims
## plot ----
plot_beta <- map(names(rb_beta_split), function(species) {
  df <- rb_beta_split[[species]]  
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
  #  df <- rb_beta_split$`AS-1+`
  ggplot(df, aes(x = factor(year), y = q50)) +
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6) +
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6) +
    geom_point(size = 2.25, alpha = 0.6) +
    theme_bw() + 
    ylab(y.axis.trt) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "gray30") + 
    geom_text(aes(y = q97.5, label = round(prob_pos, 2)),
              vjust = -0.5, size = 2, colour = "black") +
    legend_theme
})

## names ----
names(plot_beta) <- paste0(names(rb_beta_split))
list2env(plot_beta, envir = .GlobalEnv)
p1 <- plot_beta$`AS-1+`
p2 <- plot_beta$`BT-1+`
p3 <- plot_beta$`BT-0+`
p4 <- plot_beta$`AS-0+`

# save iteratively
plots <- list(
  `AS-1+` = p1,
  `BT-1+` = p2,
  `BT-0+` = p3,
  `AS-0+` = p4
)

prefix <- "/rb_beta_"

for (nm in names(plots)) {
  ggsave(
    filename = outputs_path(paste0(prefix, nm, ".png")),
    plot = plots[[nm]],
    width = 6, height = 4, dpi = 300
  )
}

#### combine ----
p1_clean <- p1 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p2_clean <- p2 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p3_clean <- p3 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p4_clean <- p4 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))

grid_den <- plot_grid(p4_clean, 
                      p1_clean,
                      p3_clean, 
                      p2_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      #labels = c("AS", "ASY","BT", "BTY"),
                      labels = c("AS-0+",
                                 "AS-1+",
                                 "BT-0+",
                                 "BT-1+"),
                      # hjust = -3, 
                      hjust = -1.5, 
                      vjust = 1.25)
grid_den
# Add shared axis labels
final_plot_den <- ggdraw(grid_den) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(y_lab, x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14) +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
final_plot_den

save_plot(outputs_path(paste0(prefix, ".png")), 
          final_plot_den, 
          base_height = 6, 
          base_width = 10,
          bg = "white")

## percent change ----
rb_beta_test$per_chg <- (exp(rb_beta_test$q50) - 1)*100



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

rb_bio_grp <- read.csv("Data/RB_data/biomass_group_m13c_rb.csv")  # this used to recreate Scruton, Table 3s

rb_bio_grp <- rb_bio_grp |>
  filter(method == "point")

rb_bio_grp <- rb_bio_grp %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))
rb_bio_grp |> dplyr::filter(trt == "1")

y.axis.bio <- expression("Biomass density (g/100 m" ^2*")")
y_lims <- c(min(rb_bio_grp$q2.5), max(rb_bio_grp$q97.5))
trt_colours <- c("Control" = "black", "Treatment" = "grey60")
rb_bio_grp$group_label <- ifelse(rb_bio_grp$trt == 0, "Control", "Treatment")

rb_bio_grp_split <- rb_bio_grp |> 
  split(rb_bio_grp$species)


## plot ----
plot_bio <- map(names(rb_bio_grp_split), function(species) {
  df <- rb_bio_grp_split[[species]]  
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
                 colour = factor(trt),
                 group  = factor(trt))) +
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_point(size = 2.25, alpha = 0.6, position = position_dodge2(width = dodge_w)) +
    theme_bw() + 
    ylab(y.axis.bio) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    geom_vline(xintercept = 3.5, linetype = "dashed", colour = "black") +
    scale_fill_discrete(name="",
                        breaks=c("Control", "Treatment"),
                        labels=c(c)) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c(0, 1),
                        labels=c("Control", "Treatment")) +
    scale_shape_manual(values = c(16, 16),
                       name="",
                       breaks=c(0, 1),
                       labels=c("Control", "Treatment"))
  
})


names(plot_bio) <- paste0(names(rb_bio_grp_split))
list2env(plot_bio, envir = .GlobalEnv)
p1 <- plot_bio$`AS-1+`
p2 <- plot_bio$`BT-1+`
p3 <- plot_bio$`BT-0+`
p4 <- plot_bio$`AS-0+`

# save iteratively
plots <- list(
  `AS-1+` = p1,
  `BT-1+` = p2,
  `BT-0+` = p3,
  `AS-0+` = p4
)

prefix <- "/rb_bio_trt_"

for (nm in names(plots)) {
  ggsave(
    filename = outputs_path(paste0(prefix, nm, ".png")),
    plot = plots[[nm]],
    width = 6, height = 4, dpi = 300
  )
}

#### combine ----
p1_clean <- p1 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p2_clean <- p2 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p3_clean <- p3 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p4_clean <- p4 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))

grid_den <- plot_grid(p4_clean, 
                      p1_clean,
                      p3_clean, 
                      p2_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      #labels = c("AS", "ASY","BT", "BTY"),
                      labels = c("AS-0+",
                                 "AS-1+",
                                 "BT-0+",
                                 "BT-1+"),
                      # hjust = -3, 
                      hjust = -1.5, 
                      vjust = 1.25)
grid_den
# Add shared axis labels
final_plot_den <- ggdraw(grid_den) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(y.axis.bio, x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14) +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
final_plot_den

save_plot(outputs_path(paste0(prefix, ".png")), 
          final_plot_den, 
          base_height = 6, 
          base_width = 10,
          bg = "white")

## table ----
rb_bio_grp$trt <- ifelse(rb_bio_grp$trt == "0", "Control", "Treatment")
rb_bio_grp$ci <- paste0("(", round(rb_bio_grp$q2.5, 1), ", ", round(rb_bio_grp$q97.5, 1), ")")
#rb_bio_grp$ci <- paste0(round(rb_bio_grp$q2.5, 1), ", ", round(rb_bio_grp$q97.5, 1))
rb_bio_grp$mci <- paste0(round(rb_bio_grp$q50, 1), " ",  rb_bio_grp$ci)
rb_bio_grp_temp <- rb_bio_grp[, c(1:2, 4, 17)]


rb_bio_grp_tab <- pivot_wider(rb_bio_grp_temp,
                              id_cols = c(species, trt),
                              names_from = c(year),
                              values_from = c(mci)
)

write.csv(rb_bio_grp_tab, "Tables/RB_tabs/RB_biomass.csv", row.names = FALSE)

# make table
kbl(rb_bio_grp_tab,
    col.names = c('spp', 'trt',
                  '2000',
                  '2001',
                  '2002',
                  '2016'),
    align = 'c', caption = "Biomass density CrIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Year" = 4)) |>
  kable_paper()



## contrast ----
# For each year x species x method, compute the DIFFERENCE in
# group-mean biomass: treatment minus control, at each MCMC
# iteration:
#
#   diff[iter] = mean(biomass_trt[iter]) - mean(biomass_ctrl[iter])
rb_bio_con <- read.csv("Data/RB_data/biomass_contrast_m13c_rb.csv")

rb_bio_con <- rb_bio_con |>
  filter(method == "point")

rb_bio_con <- rb_bio_con %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))


y_lims <- c(min(rb_bio_con$q2.5), max(rb_bio_con$q97.5))

rb_bio_con_split <- rb_bio_con|> 
  split(rb_bio_con$species)

## plot ----
plot_bio <- map(names(rb_bio_con_split), function(species) {
  df <- rb_bio_con_split[[species]]  
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
  ggplot(df, aes(x = factor(year), y = q50)) +
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_point(size = 2.25, alpha = 0.6, position = position_dodge2(width = dodge_w)) +
    theme_bw() + 
    ylab(y.axis.trt) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #theme(legend.position= "none") +
    legend_theme +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "gray30") +
    scale_fill_discrete(name="",
                        breaks=c("Control", "Treatment"),
                        labels=c(c)) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c(0, 1),
                        labels=c("Control", "Treatment")) +
    scale_shape_manual(values = c(16, 16),
                       name="",
                       breaks=c(0, 1),
                       labels=c("Control", "Treatment"))
  
})


names(plot_bio) <- paste0(names(rb_bio_grp_split))
list2env(plot_bio, envir = .GlobalEnv)
p1 <- plot_bio$`AS-1+`
p2 <- plot_bio$`BT-1+`
p3 <- plot_bio$`BT-0+`
p4 <- plot_bio$`AS-0+`

# save iteratively
plots <- list(
  `AS-1+` = p1,
  `BT-1+` = p2,
  `BT-0+` = p3,
  `AS-0+` = p4
)

prefix <- "/rb_bio_con_"

for (nm in names(plots)) {
  ggsave(
    filename = outputs_path(paste0(prefix, nm, ".png")),
    plot = plots[[nm]],
    width = 6, height = 4, dpi = 300
  )
}

#### combine ----
p1_clean <- p1 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p2_clean <- p2 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p3_clean <- p3 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))
p4_clean <- p4 + theme(axis.title = element_blank()) + theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 5, "pt"))

grid_den <- plot_grid(p4_clean, 
                      p1_clean,
                      p3_clean, 
                      p2_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      #labels = c("AS", "ASY","BT", "BTY"),
                      labels = c("AS-0+",
                                 "AS-1+",
                                 "BT-0+",
                                 "BT-1+"),
                      # hjust = -3, 
                      hjust = -1.5, 
                      vjust = 1.25)
grid_den
# Add shared axis labels
final_plot_den <- ggdraw(grid_den) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(y.axis.bio, x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14) +
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
final_plot_den

save_plot(outputs_path(paste0(prefix, ".png")), 
          final_plot_den, 
          base_height = 6, 
          base_width = 10,
          bg = "white")


# replacement ----
## Recreate my figure ----
# For each year x treatment group, compute the GROUP-MEAN of
# total station biomass at each MCMC iteration:
#
#   1. For each station in the group: sum biomass across species
#   2. Average those station totals across stations in the group
#   3. Summarise across iterations -> median, CrIs, P > threshold
#

rb_bio_tot_grp <- read.csv("Data/RB_data/biomass_total_group_m13c_rb.csv")

rb_bio_tot_grp <- rb_bio_tot_grp |>
  filter(method == "point")


ggplot(rb_bio_tot_grp, 
       aes(as.factor(year), q50)) + 
  theme_bw(base_size = 20) + 
  geom_point(aes(colour=as.factor(trt)), position=position_dodge(0.5), size = 3) + 
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                 linewidth = 0.6, alpha = 0.6,
                 position = position_dodge2(width = 0.5)) +
  geom_linerange(aes(ymin = q25, ymax = q75),
                 linewidth = 1.6, alpha = 0.6,
                 position = position_dodge2(width = 0.5)) +
  ylab(expression("Biomass density (g/100 m" ^2*")")) + 
  xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.20, .85)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  geom_hline(yintercept = 239.4, colour = "red") +
  geom_text(aes(y = q97.5, label = round(p_above, 2)),
            vjust = -0.7, size = 4, colour = "black", position = position_dodge2(width = 0.5)) +
  # geom_hline(yintercept = 42, colour = "red", linetype = "dashed") + # density of dewatered area
  # geom_hline(yintercept = 42*5.7, colour = "red", linetype = "dashed") + # 
  scale_colour_manual(
    breaks = c("0", "1"),
    labels = c("Control", "Treatment"),
    values=c("black", "grey")) +
  scale_shape_manual(
    breaks = c("0", "1"), 
    labels = c("Control", "Treatment"),
    values=c(16, 16))

ggsave("Figures/RB_figs/rb_bio_replacement_simple.png", width=10, height=8, units="in")

## GD's version ----
rb_bio_stn <- read.csv("Data/RB_data/biomass_station_m13c_rb.csv")
rb_bio_total_stn <- read.csv("Data/RB_data/biomass_total_station_m13c_rb.csv")

rb_bio_stn <- rb_bio_stn |>
  filter(method == "point")

rb_bio_stn <- rb_bio_stn %>%
  mutate(species = recode(species,
                          "AS" = "AS-1+",
                          "ASYOY" = "AS-0+",
                          "BT" = "BT-1+",
                          "BTYOY" = "BT-0+"))

rb_bio_total_stn <- rb_bio_total_stn |>
  filter(method == "point")


biomass_threshold <- 239.4    # g/100m^2  

## GD's fig
ggplot2::ggplot() +
  # Stacked bars: per-species median biomass
  geom_col(data = rb_bio_stn,
           aes(x = factor(station), y = q50, fill = label),
           position = "stack", width = 0.7) +
  # Total biomass 95% CrI (from proper posterior of the sum)
  geom_errorbar(data = rb_bio_total_stn,
                aes(x = factor(station), ymin = q2.5, ymax = q97.5),
                width = 0.25, linewidth = 0.5) +
  # Threshold line
  geom_hline(yintercept = biomass_threshold,
             linetype = "dashed", colour = "black", linewidth = 0.7) +
  # P(total > threshold) label above error bar
  geom_text(data = rb_bio_total_stn,
            aes(x = factor(station), y = q97.5, label = round(p_above,2)),
            size = 2.5, vjust = -0.5) +
  # n_species label inside bottom of bar
  #  geom_text(data = cri_data,
  #           aes(x = factor(station), y = 0,
  #              label = paste0(n_species, "/4")),
  #         size = 2, vjust = -0.3, colour = "grey30") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  facet_grid(group_label ~ year) +
  labs(title = paste0("Total biomass by station with species breakdown "),
       y = "Total biomass density (g/100m^2)",
       x = "Station",
       fill = "Species",
       caption = paste0("Note: Bar segments = median biomass per species (stacked). ",  # keep this for now but remove and put in figure captoin
                        "Error bars = 95% CrI of total biomass (summed at the iteration level).\n",
                        "Bar height may not exactly equal the total median due to the ",
                        "non-additivity of medians across correlated posteriors.")) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 7, hjust = 0))

ggsave("Figures/RB_figs/rb_bio_replacement_stacked_stn.png", width=10, height=8, units="in")


##3  collapse the facet ----
ggplot2::ggplot() +
  # Stacked bars: per-species median biomass
  geom_col(data = rb_bio_stn,
           aes(x = factor(station), y = q50, fill = label),
           position = "stack", width = 0.7, linewidth = 0.3) +
  # Total biomass 95% CrI (from proper posterior of the sum)
  geom_errorbar(data = rb_bio_total_stn,
                aes(x = factor(station), ymin = q2.5, ymax = q97.5),
                width = 0.25, linewidth = 0.5) +
  # Threshold line
  geom_hline(yintercept = biomass_threshold,
             linetype = "dashed", colour = "black", linewidth = 0.7) +
  geom_vline(xintercept = 7.5,
             linetype = "dotted", colour = "black", linewidth = 0.7) +
  
  # P(total > threshold) label above error bar
  geom_text(data = rb_bio_total_stn,
            aes(x = factor(station), y = q97.5, label = round(p_above,2)),
            size = 2.5, vjust = -0.5) +
  # n_species label inside bottom of bar
  #  geom_text(data = cri_data,
  #           aes(x = factor(station), y = 0,
  #              label = paste0(n_species, "/4")),
  #         size = 2, vjust = -0.3, colour = "grey30") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  facet_grid(~ year) +
  labs(title = paste0("Total biomass by station with species breakdown "),
       y = "Total biomass density (g/100m^2)",
       x = "Station",
       fill = "Species",
       caption = paste0("Note: Bar segments = median biomass per species (stacked). ",  # keep this for now but remove and put in figure captoin
                        "Error bars = 95% CrI of total biomass (summed at the iteration level).\n",
                        "Bar height may not exactly equal the total median due to the ",
                        "non-additivity of medians across correlated posteriors.")) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 7, hjust = 0))

ggsave("Figures/RB_figs/rb_bio_replacement_unstacked_stn.png", width=10, height=8, units="in")

### change colours  ----
ggplot2::ggplot() +
  # Stacked bars: per-species median biomass
  geom_col(data = rb_bio_stn,
           aes(x = factor(station), y = q50, fill = label),
           colour = "black", position = "stack", width = 0.7, linewidth = 0.3) +
  # Total biomass 95% CrI (from proper posterior of the sum)
  geom_errorbar(data = rb_bio_total_stn,
                aes(x = factor(station), ymin = q2.5, ymax = q97.5),
                width = 0.25, linewidth = 0.5) +
  # Threshold line
  geom_hline(yintercept = biomass_threshold,
             linetype = "dashed", colour = "black", linewidth = 0.7) +
  geom_vline(xintercept = 7.5,
             linetype = "dotted", colour = "black", linewidth = 0.7) +
  # P(total > threshold) label above error bar
  geom_text(data = rb_bio_total_stn,
            aes(x = factor(station), y = q97.5, label = round(p_above,2)),
            size = 2.5, vjust = -0.5) +
  scale_fill_manual(
    values = c("grey85", "white", "gray40", "grey20"),
    name = "Species"
  ) +
  theme_bw() +
  facet_grid(~ year) +
  labs(y = "Total biomass density (g/100m^2)",
       x = "Station",
       fill = "Species",
  ) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 16)) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 7, hjust = 0))

ggsave("Figures/RB_figs/rb_bio_replacement_unstacked_BW_stn.png", width=10, height=8, units="in")



# Param Tables ----

rb_data$m13c_samples
rb_chains_m13c 


# this does log_mu and sigma_d
m13c_mu_cri <- extract_cri(rb_data$m13c_samples, "^log_mu_d\\[|^sigma_d\\[") |>
  mutate(
    model   = "M13c_anova_glmm_rb",
    sp_idx  = as.integer(str_extract(param, "\\d+")),
    species = sl_levels[sp_idx]
  )

mu_p_cri <- quantile(rb_chains_m13c [, 'mu_p'], probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                     na.rm = TRUE)
sig_p_cri <- quantile(rb_chains_m13c [, 'sigma_p'], probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                      na.rm = TRUE)

year_levels <- c(2000, 2001, 2002, 2015)
year_lookup_m13c  <- tibble(year_idx = seq_along(year_levels),
                            year     = year_levels)


# Annual random effect on log-density (control sites only at PB, all sites at RB and GC)
year_re_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(rb_data$m13c_samples, paste0("^alpha_year\\[.*,", sl, "\\]")) |>
    mutate(
      year_idx = as.integer(str_extract(param, "(?<=\\[)\\d+")),
      label    = species_code_map[sl],
      species  = sl_levels[sl]
    ) |>
    left_join(year_lookup_m13c , by = "year_idx")
}) |> mutate(model = "M13c_anova_glmm_rb")


sigma_year_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(rb_data$m13c_samples, paste0("^sigma_year\\[", sl, "\\]")) |>
    mutate(label = species_code_map[sl], species = sl_levels[sl])
}) |> mutate(model = "M13c_anova_glmm_rb")


# Annual effect for treatment sites in post restoration years
m13c_beta <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(rb_data$m13c_samples, paste0("^beta_year\\[.*,", sl, "\\]")) |>
    mutate(
      year_idx = as.integer(str_extract(param, "(?<=\\[)\\d+")),
      label         = species_code_map[sl],
      species       = sl_levels[sl]
    ) 
}) |>
  mutate(
    model        = "M13c_anova_glmm_rb",
    prob_pos     = NA_real_,   # filled below from raw draws
    interpretation = NA_character_
  ) |>
  left_join(year_lookup_m13c , by = "year_idx")


sigma_beta_cri <- purrr::map_dfr(1:4, function(sl) {
  extract_cri(rb_data$m13c_samples, paste0("^sigma_beta\\[", sl, "\\]")) |>
    mutate(label = species_code_map[sl], species = sl_levels[sl])
}) |> mutate(model = "M13c_anova_glmm_rb")



m13c_mu_cri[, -7]
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
m13c_beta[, 1:10]
sigma_beta_cri[, 1:8]


param_df <- bind_rows(m13c_mu_cri[, -7],
                      mu_p_cri_df,
                      sig_p_cri_df,
                      year_re_cri[, 1:10],
                      sigma_year_cri[, 1:8],
                      m13c_beta[, c(1:9, 13)],
                      sigma_beta_cri[, 1:8]
)
rownames(param_df) <- NULL


write.csv(param_df, "Tables/RB_tabs/RB_param_tab.csv")
ft_df <- param_df[, c(1, 8, 11, 2:6)]
ft_df <- ft_df |>
  mutate(across(where(is.numeric), ~ round(.x, digits = 2)))
names(ft_df) <- c("Parameters", "species", "year",
                  "2.5%", "25%", "50%", "75%", "97.5%")

## table ----
library(kableExtra)

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
  add_header_above(header = c(" " = 3, "Quantiles" = 5)) |>
  kable_paper()



# END ----