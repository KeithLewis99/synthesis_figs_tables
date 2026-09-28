# The purpose of this file is to determine if there is any increase in large salmonids at RB because that was one of the goals of the project.  

# HOWEVER, in creating this file, we discovered that there are some very small adult AS and BT on RB.  There are only a couple and this is likely just a data entry error.  Further, when we looked at GC and PB, the problem appears to be more systematic.  

# also, I started out doing ridge plots but we really don't have enough data for that in some cases so I changed to histograms based on discussions with GD.

# So, there this partly to create a publication quality graphic but the majority is to create diagnostic graphs to see WTF happened ito of classifying age-classes over the duration of the study.

# I inserted the publication quality graphic infront of all the diagnostic/detective work. This is 'RB data revised'

# The first set historgram type figures is to create a frequency distribution of 0+ and 1+ fish by study area and year and then by study area, year, and station.  The first is to show general trends and the latter is to show station by station differences which is useful becasue sometimes, there is overlap in the study area-year distribution that is hard to tease out. Teh vlines in these graphs were based on discussions with Keith Clarke.
# We discarded GC 2006 because 1) it did not look like we could differentiate the 0+ and 1+ and 2) only one habitat was sampled that year.

# Then, we made the 'datacheck' figures which was similar to above but only the really problematic years where it looked like there were systemic errors.  # Note that for RB, all the errors appear to be of the data entry type so I did not mke 'data checks' or 'salmonid' graphs.

# The 'gray' plots are plots where 0+ and 1+ are not distinguished from one another.  These were sent to Kristin Loughlin and Nick Kelly for an independent assessment of the divisions between the two age-classes by species - see 'study_area_species_data_decisions_KL.xlsx'.

# Finally, we plotted the values from 'study_area_species_data_decisions_KL.xlsx' in the 'salmonids' section.  

# These were further modified due to disagreements between KL/GD and what salmonids came up with.  These are recorded in 'study_area_species_data_decisions_KL.xlsx' under Threshold_AS (mm)_KL_GD,	Change_AS,	Threshold_BT (mm)_KL_GD,	Change_BT.  Note that no figures were reproduced from these new values.

# Spoke to Curtis Pennel about the above figures.  He agreed that there is an issue and mentioned that Loyd Cole had aged fish.  I checked the archive (electro_fish.xlsx or csv which is better) which runs to mid-late 90s.  Fish were aged for Seal Cove (river_code == 2216270) and Pamehac (0707798). However, linking these to the fish is going to be hard because the  Clarke was pretty sure fish were not aged for RB (3919460) or GC (no river code).  

# based on this work, GD will reclassify the age classes and re-run the models, after which, we need to remake all the figures


# library ----
# need this by treatment type
library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyr)
library(purrr)
library(cowplot)


# Source 
source("functionsKL.R")

# RB data revised ----
df_rb_LF <- read.csv("Data/RB_data/RB_individual_LF.csv")
str(df_rb_LF)
unique(df_rb_LF$Sweep)

df_rb_LF <- df_rb_LF |>
  mutate(trt = ifelse(Station <=7, "trt", "con")) %>%
  filter(Species %in% c("BT", "AS"))


length_hist <- hist_plot_fun(df_rb_LF)
length_hist
ggsave("Figures/RB_figs/length_histo_LF_pub.png", length_hist, width = 8, height = 6, dpi = 300, units = "in")


# RB data ----
# check to see if this needs to be filtered
df_all <- read.csv("../RoseBlanche/RB_depletion/data_derived/df_all.csv")
df_all <- df_all |>
  filter(Length.mm > 0 & Sweep <=3) |>
  mutate(trt = ifelse(Station <=7, "trt", "con"))

dat <- df_all %>%
  filter(Species %in% c("BT", "AS"))


## ridge plots ----
length_den <- ridge_plot_fun(dat)
length_den
ggsave("Figures/RB_figs/length_density_BT_AS.png", length_den, width = 8, height = 6, dpi = 300, units = "in")


# this for the publication
length_hist <- hist_plot_fun(dat)
length_hist
ggsave("Figures/RB_figs/length_histo_BT_AS.png", length_hist, width = 8, height = 6, dpi = 300, units = "in")


### vlines ----
# this dataset is just to set the vlines for the species - just a 
vlines <- data.frame(
  sp = c("AS", "BT"),
  xint = c(70, 80)
)


# RB ----
dat <- df_all %>%
  filter(Species %in% c("BT", "BTYOY", "AS", "ASYOY")) %>%
  mutate(
    sp = case_when(
      Species %in% c("BT", "BTYOY") ~ "BT",
      Species %in% c("AS", "ASYOY") ~ "AS"
    ),
    stage = case_when(
      Species %in% c("BTYOY", "ASYOY") ~ "YOY",
      TRUE ~ "Older"
    )
  )

# data summary 
df_all_summary <- df_all |>
  filter(Species == "AS" & Length.mm <= 65 |
           Species == "ASYOY" & Length.mm >= 70 |
           Species == "BT" & Length.mm <= 75 |
           Species == "BTYOY" & Length.mm >= 80
  ) |>
  tibble::as_tibble() 

df_all_summary |> print(n = Inf)

df_all_summary |>
  group_by(Year, Species, trt) |>
  summarise(count = n()) |>
  pivot_wider(
    names_from = trt,
    values_from = count,
    values_fill = 0
  )

nrow(df_all_summary |> filter(Species == "AS" & Length.mm <= 65))
nrow(df_all_summary |> filter(Species == "BT" & Length.mm <= 75))
nrow(df_all_summary |> filter(Species == "BT" & Length.mm <= 70))
nrow(df_all_summary |> filter(Species == "BT" & Length.mm <= 65))
nrow(df_all_summary |> filter(Species == "BT" & Length.mm <= 60))

### density ----
length_den <- ridge_plot_age_fun(dat, title = "Rose Blanche")
length_den

### histo -----
length_age_hist <- hist_plot_age_fun(dat, title = "Rose Blanche")
length_age_hist

#### histo-stn-loop ----
plot_length_hist <- function(yr, data, bin = 5) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun(
      title = paste0("Rose Blanche - ", yr, "; bin-size = ", bin),
      vlines = vlines,
      bin = bin
    )
}
years <- sort(unique(dat$Year))
plots <- map(years, ~ plot_length_hist(.x, dat))
names(plots) <- years


plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun(
      title = paste0("Rose Blanche - ", yr, "; bin-size = ", bin), 
      vlines = vlines, 
      bin = bin
    )
}

years <- sort(unique(dat$Year))
plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


## combine plots ----

combined_plots <- combine_plot_lists(plots_all, plots)

output_folder <- "Figures/RB_figs"
output_name <- "RB"
study_area_name <- "Rose Blanche"

print(paste0(output_folder, "/LengthFreq_combined_", output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_combined_", output_name, "_"),
  out_dir = output_folder,
)

## save as pdf
print(paste0(output_folder, "/LengthFreq_datacheck_", output_name, "_AllYears.pdf"))

save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, "/LengthFreq_combined_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, " Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste0("Data source: ", study_area_name, "EF data"),
    "",
    "Vlines: AS = 70 mm; BT = 80 mm",
    "",
    "ASYOY not >= 70 mm",
    "AS not <= 65 mm",
    "BTYOY not >= 80 mm",
    "BT not <= 75 mm",
    "",
    "Values are intended as markers only."
  )
)

## gray ----

plot_length_hist <- function(yr, data, bin = 5) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun1(
      title = paste0("Rose Blanche - ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin, 
      fill_var = "sp",
      gray_all = TRUE
    )
}
years <- sort(unique(dat$Year))
plots <- map(years, ~ plot_length_hist(.x, dat))
names(plots) <- years




plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun1(
      title = paste0("Rose Blanche - -  ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin, 
      fill_var = "sp",
      gray_all = TRUE
    )
}

plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


### combine ----
combined_plots <- combine_plot_lists(plots_all, plots)

print(paste0(output_folder, "/LengthFreq_gray_", output_name, "_"))

save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_gray_", output_name, "_"),
  out_dir = output_folder,
)

# save pdf
print(paste0(output_folder, "/LengthFreq_gray_", output_name, "_AllYears.pdf"))

save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, "/LengthFreq_gray_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, " Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste("Data source: ", study_area_name, " EF data"),
             "",
             "Use the length frequency graphs below to determine the threshold size separating 0+/1+ Atlantic salmon",
             "",
             "and Brook trout at each station x year.  ",
             "",
             "Please enter the threshold size for each year x station x species in the excel sheet provided along with the graphs.",
             "",
             "When returning you excel sheet please provide a description of the method you used to determine the threshold.",
             ""
      )
    )



# GC ----
## data ----
df_all <- read.csv("../Granite/analyses/restoration_Granite/data_derived/df_all0.csv")


str(df_all)

df_all <- df_all |> mutate(channel = if_else(Station == "Control1"|Station == "Control2", "con",
                     if_else(Station == "1" | 
                               Station == "2" |
                               Station == "3" |
                               Station == "4" |
                               Station == "5" |
                               Station == "6", "SC", "MC")
                     )
                  )

df_all <- df_all |> 
  filter(Sweep <=3) |>
  filter(channel != "con") |>
  filter(Year != "2006") |>
  filter(Month != "Sept") |>
  mutate(trt = paste(channel, type, sep = "-"))

# data summary 
df_all_summary <- df_all |>
  filter(Species == "AS" & Length.mm <= 65 |
           Species == "ASYOY" & Length.mm >= 70 |
           Species == "BT" & Length.mm <= 75 |
           Species == "BTYOY" & Length.mm >= 80
         ) |>
  tibble::as_tibble() 

df_all_summary |> print(n = Inf)

df_all_summary |>
  group_by(Year, Species, trt) |>
  summarise(count = n()) |>
  pivot_wider(
    names_from = trt,
    values_from = count,
    values_fill = 0
  )

nrow(df_all_summary |> filter(Species == "AS" & Length.mm <= 65))
nrow(df_all_summary |> filter(Species == "ASYOY" & Length.mm >= 70))
nrow(df_all_summary |> filter(Species == "ASYOY" & Length.mm >= 75))
nrow(df_all_summary |> filter(Species == "ASYOY" & Length.mm >= 80))
nrow(df_all_summary |> filter(Species == "BT" & Length.mm <= 75))
nrow(df_all_summary |> filter(Species == "BT" & Length.mm <= 70))
nrow(df_all_summary |> filter(Species == "BTYOY" & Length.mm >= 80))


## combine species
dat <- df_all %>%
  filter(Species %in% c("BT", "BTYOY", "AS", "ASYOY")) %>%
  mutate(
    sp = case_when(
      Species %in% c("BT", "BTYOY") ~ "BT",
      Species %in% c("AS", "ASYOY") ~ "AS"
    ),
    stage = case_when(
      Species %in% c("BTYOY", "ASYOY") ~ "YOY",
      TRUE ~ "Older"
    )
  )

## density ----
length_gc <- ridge_plot_age_fun(dat, title = "Granite Canal") 
length_gc

# scale_x_continuous(limits = c(0, 210), breaks = seq(0,200, by = 25)) +

ggsave("Figures/GC_figs/length_density_BT_AS.png", length_gc, width = 10, height = 8, dpi = 300, units = "in", bg = "white")


## histo-stn-loop ----
plot_length_hist <- function(yr, data, bin = 2) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun(
      title = paste0("Granite Canal - ", yr, "; bin-size = ", bin), 
      vlines = NULL,
      bin = bin
    )
}
years <- sort(unique(dat$Year))
plots <- map(years, ~ plot_length_hist(.x, dat))
names(plots) <- years


plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun(
      title = paste0("Granite Canal - ", yr, "; bin-size = ", bin),
      vlines = vlines,
      bin = bin
    )
}

years <- sort(unique(dat$Year))
plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


## combine plots ----

combined_plots <- combine_plot_lists(plots_all, plots)

output_folder <- "Figures/GC_figs"
output_name <- "GC"
study_area_name <- "Granite Canal"

print(paste0(output_folder, "/LengthFreq_combined_", output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_combined_", output_name, "_"),
  out_dir = output_folder,
)

print(paste0(output_folder, "/LengthFreq_combined_", output_name, "_AllYears.pdf"))
save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, "/LengthFreq_combined_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, " Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste0("Data source: ", study_area_name, "EF data"),
    "",
    "Vlines: AS = 70 mm; BT = 80 mm",
    "",
    "ASYOY not >= 70 mm",
    "AS not <= 65 mm",
    "BTYOY not >= 80 mm",
    "BT not <= 75 mm",
    "",
    "Values are intended as markers only."
  )
)

# Data checks ----
dat_gc_datacheck <- dat |> filter(Year == 2005 & Station %in%c(1, 4, 5, "M11Riffle", "M11Run",  "M3Run") |
                                    Year == 2007 & Station %in%c(1, 2, 4, 5, "M11Riffle", "M11Run",  "M12Riffle", "m12Run", "M3Run", "M5Riffle"))


### vlines ----
vlines2 <- data.frame(
  Year = c(rep("2005", 6), rep("2007", 9)),
  sp = c(sort(rep(c("AS"), 6)), sort(rep(c("AS") , 9))),
  Station = c(rep(c(1, 4, 5, "M11Riffle", "M11Run",  "M3Run"), 1), rep(c(1, 2, 4, 5, "M11Riffle", "M11Run",  "M12Riffle", "M3Run", "M5Riffle"), 1)),
  xint = c(rep(61, 6), 
           #rep(70, 6),
           58, 60, 57, 60, rep(61, 5)
           #rep(80, 9)
  )
)

plot_length_hist <- function(yr, data, bin = 2) {
  data %>%
    filter(Year == yr) %>%
    filter(Species == "AS"|Species == "ASYOY") |>
    hist_plot_age_stn_fun(
      title = paste0("Granite Canal Data Check - ", yr, "; bin-size = ", bin), 
      vlines = filter(vlines2, Year == yr),
      bin = bin
    )
}
years <- sort(unique(dat_gc_datacheck$Year))
plots <- map(years, ~ plot_length_hist(.x, dat_gc_datacheck))
names(plots) <- years


plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun(
      title = paste0("Granite Canal - Data check -  ", yr, "; bin-size = ", bin),
      vlines = vlines,
      bin = bin
    )
}

years <- sort(unique(dat_gc_datacheck$Year))
plots_all <- map(years, ~ plot_length_hist_all(.x, dat_gc_datacheck))
names(plots_all) <- years


## combine plots ----

combined_plots <- combine_plot_lists(plots_all, plots)

print(paste0("/LengthFreq_datacheck_", output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_datacheck_", output_name, "_"),
  out_dir = output_folder,
)

print(paste0(output_folder, "/LengthFreq_datacheck_", output_name, "_AllYears.pdf"))
save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, "/LengthFreq_datacheck_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, "Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste0("Data source: ", study_area_name, " EF data"),
    "",
    "Vlines: discussion with K. Clarke and G. Dauphin (2026-08-29) to suggest misclassifications of 0+ and 1+ fish",
    "",
    "vlines for the aggregated figure unchanged from previous report, vlines for stations refined based on discussions with KC and GD",
    "",
    "Revised version - no BT becauae its obvious where the divisions are and want more room for AS"
  )
)


### gray ----
plot_length_hist <- function(yr, data, bin = 5) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun1(
      title = paste0(study_area_name, " - ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin, 
      fill_var = "sp",
      gray_all = TRUE
    )
}
years <- sort(unique(dat$Year))
plots <- map(years, ~ plot_length_hist(.x, dat))
names(plots) <- years




plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun1(
      title = paste0(study_area_name, " - ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin, 
      fill_var = "sp",
      gray_all = TRUE
    )
}

plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


#### combine ----
combined_plots <- combine_plot_lists(plots_all, plots)

print(paste0(output_folder, "/LengthFreq_gray_", output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_gray_", output_name, "_"),
  out_dir = output_folder,
)


# save pdf
print(paste0(output_folder, "/LengthFreq_gray_", output_name, "_AllYears.pdf"))

save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, "/LengthFreq_gray_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, " Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste("Data source: ", study_area_name, " EF data"),
    "",
    "Use the length frequency graphs below to determine the threshold size separating 0+/1+ Atlantic salmon",
    "",
    "and Brook trout at each station x year.  ",
    "",
    "Please enter the threshold size for each year x station x species in the excel sheet provided along with the graphs.",
    "",
    "When returning you excel sheet please provide a description of the method you used to determine the threshold.",
    ""
  )
)


# salmonids ----

### vlines ----

# Stations sampled in each year
stations <- list(
  `2004` = c("1", "2", "4", "5",
             "M11Riffle", "M11Run",
             "M12Riffle", "M12Run",
             "M3Riffle", "M3Run",
             "M5Riffle", "M5Run"),
  
  `2005` = c("1", "2", "4", "5",
             "M11Riffle", "M11Run",
             "M12Riffle", "M12Run",
             "M3Riffle", "M3Run",
             "M5Riffle", "M5Run"),
  
  `2007` = c("1", "2", "4", "5",
             "M11Riffle", "M11Run",
             "M12Riffle", "M12Run",
             "M3Riffle", "M3Run",
             "M5Riffle", "M5Run"),
  
  `2009` = c("1", "2", "4", "5",
             "M11Riffle", "M11Run",
             "M12Riffle", "M12Run",
             "M3Riffle", "M3Run",
             "M5Riffle", "M5Run"),
  
  `2016` = c("1", "2", "3", "4", "5", "6",
             "M11Riffle", "M11Run",
             "M3Riffle", "M3Run")
)

thresholds <- bind_rows(
  lapply(names(stations), function(yr) {
    tibble(
      `Study Area` = "GC",
      Year = as.integer(yr),
      Station = stations[[yr]]
    )
  })
) %>%
  mutate(
    Threshold_AS = if_else(Year == 2009, 70, 63),
    Threshold_BT = 75
  )


vlines3 <- thresholds %>%
  pivot_longer(
    cols = c(Threshold_AS, Threshold_BT),
    names_to = "sp",
    values_to = "xint"
  ) %>%
  mutate(
    sp = recode(
      sp,
      Threshold_AS = "AS",
      Threshold_BT = "BT"
    )
  ) %>%
  arrange(Year, sp, Station)

vlines3


plot_length_hist <- function(yr, data, bin = 2) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun(
      title = paste0("Granite Canal - Salmonids - ", yr, "; bin-size = ", bin), 
      vlines = filter(vlines3, Year == yr),
      bin = bin
    )
}
years <- sort(unique(dat$Year))
plots <- map(years, ~ plot_length_hist(.x, dat))
names(plots) <- years


plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun(
      title = paste0("Granite Canal - Salmonids -  ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin
    )
}

years <- sort(unique(dat$Year))
plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


## combine plots ----

combined_plots <- combine_plot_lists(plots_all, plots)

print(paste0("/LengthFreq_salmonid_vlines_", output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_salmonid_vlines_", output_name, "_"),
  out_dir = output_folder,
)

print(paste0(output_folder, "/LengthFreq_salmonid_vlines_", output_name, "_AllYears.pdf"))
save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, "/LengthFreq_salmonid_vlines_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, "Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste0("Data source: ", study_area_name, " EF data"),
    "",
    "Vlines: based on review by K. Louglin and N. Kelly, ~ 2026-09-04",
    "",
    "This exercise was because we wanted an independent review of the LF data using the gray graphs, i.e., remove bias",
    "",
    "Revised version - no BT becauae its obvious where the divisions are and want more room for AS"
  )
)




# PB  ----
## data
df_all <- read.csv("../PamehacDepletion/data_derived/df_all.csv")
df_all <- df_all |>
  filter(Sweep <=3) |>
  filter(!Station %in% c("5B", "9")) |>
  mutate(trt = ifelse(Station == "7" | Station == "6", "trt", "con"))

dat <- df_all %>%
  filter(Species %in% c("BT", "BTYOY", "AS", "ASYOY")) %>%
  mutate(
    sp = case_when(
      Species %in% c("BT", "BTYOY") ~ "BT",
      Species %in% c("AS", "ASYOY") ~ "AS"
    ),
    stage = case_when(
      Species %in% c("BTYOY", "ASYOY") ~ "YOY",
      TRUE ~ "Older"
    )
  )


# data summary 
df_all_summary <- df_all |>
  filter(Species == "AS" & Length.mm <= 65 |
           Species == "ASYOY" & Length.mm >= 70 |
           Species == "BT" & Length.mm <= 75 |
           Species == "BTYOY" & Length.mm >= 80
  ) |>
  tibble::as_tibble() 

df_all_summary |> print(n = Inf)

df_all_summary |>
  group_by(Year, Species, trt) |>
  summarise(count = n()) |>
  pivot_wider(
    names_from = trt,
    values_from = count,
    values_fill = 0
  )

nrow(df_all_summary |> filter(Species == "AS" & Length.mm <= 65))
nrow(df_all_summary |> filter(Species == "ASYOY" & Length.mm >= 70))
nrow(df_all_summary |> filter(Species == "ASYOY" & Length.mm >= 75))
nrow(df_all_summary |> filter(Species == "BT" & Length.mm <= 75))
nrow(df_all_summary |> filter(Species == "BT" & Length.mm <= 70))


## density ----
length_pb <- ridge_plot_age_fun(dat, title = "Pamehac Brook") 
length_pb
ggsave("Figures/PB_figs/length_density_BT_AS.png", length_pb, width = 10, height = 8, dpi = 300, units = "in", bg = "white")
# scale_x_continuous(limits = c(0, 210), breaks = seq(0,200, by = 25)) +


## histo-stn-loop ----
plot_length_hist <- function(yr, data, bin = 5) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun(
      title = paste0("Pamehac Brook - ", yr, "; bin-size = ", bin), 
      vlines = vlines,
      bin = bin
    )
}
years <- sort(unique(dat$Year))
plots <- map(years, ~ plot_length_hist(.x, dat))
names(plots) <- years


plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun(
      title = paste0("Pamehac Brook - ", yr, "; bin-size = ", bin), 
      vlines = vlines,
      bin = bin
    )
}

years <- sort(unique(dat$Year))
plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


## combine plots ----

combined_plots <- combine_plot_lists(plots_all, plots)

output_folder <- "Figures/PB_figs"
output_name <- "PB"
study_area_name <- "Pamehac Brook"

print(paste0(output_folder,"/LengthFreq_combined_", output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_combined_", output_name, "_"),
  out_dir = output_folder,
)

# save as pdf
print(paste0(output_folder, "/LengthFreq_combined_", output_name, "_AllYears.pdf"))
save_report_pdf(
  file = paste0(output_folder, "/LengthFreq_combined_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, "Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste0("Data source: ", study_area_name, " EF data"),
    "",
    "Vlines: AS = 70 mm; BT = 80 mm",
    "",
    "ASYOY not >= 70 mm",
    "AS not <= 65 mm",
    "BTYOY not >= 80 mm",
    "BT not <= 75 mm",
    "",
    "Values are intended as markers only."
  )
)



## Data check ----
dat_pb_datacheck <- dat |> filter(Year == 2016 & Station %in%c(1, 2, 4, 5, 6, "5A", 6, 7, 8, "8A")
                                  )

### vlines ----

vlines2 <- data.frame(
  Year = c(rep("2016", 9)),
  sp = c(sort(rep(c("AS"), 9))),
  Station = c(rep(c(1, 2, 4, 5, "5A", 6, 7, 8, "8A"), 1)),
  xint = c(51, 55, 50, 50, 51, 53, 60, 60, 60
  )
)


plot_length_hist <- function(yr, data, bin = 2) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun(
      title = paste0("Pamehac Brook Data Check - ", yr, "; bin-size = ", bin), 
      vlines = filter(vlines2, Year == yr),
      bin = bin
    )
}
years <- sort(unique(dat_pb_datacheck$Year))
plots <- map(years, ~ plot_length_hist(.x, dat_pb_datacheck))
names(plots) <- years


plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun(
      title = paste0("Pamehac Brook Data Check -  ", yr, "; bin-size = ", bin),
      vlines = vlines,
      bin = bin
    )
}

years <- sort(unique(dat_pb_datacheck$Year))
plots_all <- map(years, ~ plot_length_hist_all(.x, dat_pb_datacheck))
names(plots_all) <- years


## combine plots ----

combined_plots <- combine_plot_lists(plots_all, plots)

print(paste0(output_folder, "/LengthFreq_datacheck_", output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_datacheck_", output_name, "_"),
  out_dir = output_folder,
)

# save as pdf

print(paste0(output_folder, "/LengthFreq_datacheck_", output_name, "_AllYears.pdf"))
save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, "/LengthFreq_datacheck_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, "Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste0("Data source: ", study_area_name, " EF data"),
    "",
    "Vlines: discussion with K. Clarke and G. Dauphin (2026-08-29) to suggest misclassifications of 0+ and 1+ fish",
    "",
    "vlines for the aggregated figure unchanged from previous report, vlines for stations refined based on discussions with KC and GD"
  )
)


### gray ----
plot_length_hist <- function(yr, data, bin = 5) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun1(
      title = paste0(study_area_name, " - ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin, 
      fill_var = "sp",
      gray_all = TRUE
    )
}
years <- sort(unique(dat$Year))
plots <- map(years, ~ plot_length_hist(.x, dat))
names(plots) <- years




plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun1(
      title = paste0(study_area_name, " - ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin, 
      fill_var = "sp",
      gray_all = TRUE
    )
}

plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


### combine ----
combined_plots <- combine_plot_lists(plots_all, plots)

print(paste0(output_folder, "/LengthFreq_gray_", output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0("/LengthFreq_gray_", output_name, "_"),
  out_dir = output_folder,
)


print(paste0(output_folder, "/LengthFreq_gray_", output_name, "_AllYears.pdf"))
save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, "/LengthFreq_gray_", output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, "Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste0("Data source: ", study_area_name, " EF data"),
    "",
    "Use the length frequency graphs below to determine the threshold size separating 0+/1+ Atlantic salmon",
    "",
    "and Brook trout at each station x year.  ",
    "",
    "Please enter the threshold size for each year x station x species in the excel sheet provided along with the graphs.",
    "",
    "When returning you excel sheet please provide a description of the method you used to determine the threshold.",
    ""
  )
)



# salmonids ----

### vlines ----

# Stations sampled in each year
vlines_pb <- dat %>%
  distinct(Year, Station, sp) %>%
  mutate(
    xint = case_when(
      sp == "AS" & Year %in% c(1990, 1991) ~ 63,
      sp == "AS" & Year == 1992 & Station %in% c("6", "7") ~ 75,
      sp == "AS" & Year == 1992 ~ 63,
      sp == "AS" & Year == 1996 ~ 63,
      sp == "AS" & Year == 2016 & Station %in% c("1", "2", "3", "4", "5", "5A") ~ 55,
      sp == "AS" & Year == 2016 ~ 63,
      
      sp == "BT" & Year %in% c(1990, 1991) ~ 75,
      sp == "BT" & Year == 1992 ~ 65,
      sp == "BT" & Year %in% c(1996, 2016) ~ 63
    )
  )

plot_length_hist <- function(yr, data, bin = 2) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_stn_fun(
      title = paste0("Pamehac Brook - Salmonids - ", yr, "; bin-size = ", bin), 
      vlines = filter(vlines_pb, Year == yr),
      bin = bin
    )
}
years <- sort(unique(dat$Year))
plots <- map(years, ~ plot_length_hist(.x, dat))
names(plots) <- years


plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun(
      title = paste0("Pamehac Brook - Salmonids -  ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin
    )
}

years <- sort(unique(dat$Year))
plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


## combine plots ----

combined_plots <- combine_plot_lists(plots_all, plots)
file_title <- "/LengthFreq_salmonid_vlines_"

print(paste0(file_title, output_name, "_"))
save_plot_list(
  combined_plots,
  file_prefix = paste0(file_title, output_name, "_"),
  out_dir = output_folder,
)

print(paste0(output_folder, file_title, output_name, "_AllYears.pdf"))
save_report_pdf(
  #file = "Figures/GC_figs/temp____LengthFreq_combined_GC_AllYears.pdf",
  file = paste0(output_folder, file_title, output_name, "_AllYears.pdf"),
  plots_top = plots_all,
  plots_bottom = plots,
  cover_title = paste0(study_area_name, "Length Frequency Report"),
  cover_text = c(
    paste("Generated:", Sys.Date()),
    paste0("Data source: ", study_area_name, " EF data"),
    "",
    "Vlines: based on review by K. Louglin and N. Kelly, ~ 2026-09-04",
    "",
    "This exercise was because we wanted an independent review of the LF data using the gray graphs, i.e., remove bias",
    "",
    "Revised version - no BT becauae its obvious where the divisions are and want more room for AS"
  )
)



# archive check ----
# this file is a subset of C:\Users\lewiske\Documents\CAFE\projects\restoration\archival_data\data\electro_fish.csv
## based on extracting the river code for Pamehac (see above)

# the below figure shows good seperation in 0+ and 1+ for 
## AS 1991 1992; AS 1990 has no fish
## BT 1990, 1991; BT 1992 has some overlap
df_age_length <- read.csv("Data/PB_data/pb_age_length_archive.csv")

# AS = 173, BT = 178 based on Akenhead and LeGrow 1981, river codes are Waldron codes
df_age_length |> 
  summarise(n_na = sum(is.na(Scale_Age)),
            n_age = sum(!is.na(Scale_Age)))

dat <- df_age_length %>%
  filter(!is.na(Scale_Age)) |>
  mutate(
    sp = case_when(
      Species %in% c(178) ~ "BT",
      Species %in% c(172, 173) ~ "AS"
    ),
    stage = case_when(
      Scale_Age %in% c(0) ~ "YOY",
      TRUE ~ "Older"
    )
  ) |>
  rename(Length.mm = Fork_Length)



plot_length_hist_all <- function(yr, data, bin = 1) {
  data %>%
    filter(Year == yr) %>%
    hist_plot_age_all_fun(
      title = paste0("Pamehac Brook - Salmonids -  ", yr, "; bin-size = ", bin),
      vlines = NULL,
      bin = bin
    )
}

years <- sort(unique(dat$Year))
plots_all <- map(years, ~ plot_length_hist_all(.x, dat))
names(plots_all) <- years


# END ----
