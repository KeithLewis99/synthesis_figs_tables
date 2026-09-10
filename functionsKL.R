# Helper functions -----
# These functions feed figures _RB_final.R, figures_GC_final.R, and figues_PB_final.R  feed figures_KL_ms_output_claude-v2.R

extract_cri <- function(samples, param_pattern,
                        probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  chains <- do.call(rbind, samples)
  cols   <- grep(param_pattern, colnames(chains), value = TRUE)
  if (length(cols) == 0) return(tibble(param = character(0)))
  mat <- chains[, cols, drop = FALSE]
  if (ncol(mat) == 1) {
    q <- quantile(mat[, 1], probs = probs)
    result <- as.data.frame(t(q))
    setNames(result, paste0("q", probs * 100)) |>
      mutate(param = cols) |>
      select(param, everything())
  } else {
    apply(mat, 2, quantile, probs = probs) |>
      t() |>
      as.data.frame() |>
      setNames(paste0("q", probs * 100)) |>
      rownames_to_column("param")
  }
}


filter_excluded_year_sl <- function(df) {
  if (!exists("excluded_year_sl") || nrow(excluded_year_sl) == 0) return(df)
  if ("species" %in% names(df) && "species" %in% names(excluded_year_sl)) {
    return(anti_join(df, excluded_year_sl, by = c("year", "species")))
  }
  if ("sl_idx" %in% names(df)) {
    return(anti_join(df, excluded_year_sl, by = c("year", "sl_idx")))
  }
  df
}

calc_prob_positive_gc <- function(chains,
                                  year_levels,
                                  species_levels = sl_levels,
                                  hab_levels = NULL,
                                  var = "d",
                                  species_code_map = NULL) {
  
  purrr::map_dfr(seq_along(year_levels), function(yr_idx) {
    yr <- year_levels[yr_idx]
    
    if (is.null(hab_levels)) {
      # original 2-index case: var[yr, species]
      purrr::map_dfr(species_levels, function(sp) {
        sp_idx <- match(sp, species_levels)
        col_name <- paste0(var, "[", yr_idx, ",", sp_idx, "]")
        
        if (!col_name %in% colnames(chains)) {
          warning("Column ", col_name, " not found in chains; skipping.")
          return(NULL)
        }
        
        draws <- chains[, col_name]
        
        tibble::tibble(
          year     = yr,
          species  = sp,
          prob_pos = mean(draws > 0)
        )
      })
    } else {
      # 3-index case: var[yr, hab, species]
      purrr::map_dfr(seq_along(hab_levels), function(hab_idx) {
        hab <- hab_levels[hab_idx]
        
        purrr::map_dfr(species_levels, function(sp) {
          sp_idx <- match(sp, species_levels)
          col_name <- paste0(var, "[", yr_idx, ",", hab_idx, ",", sp_idx, "]")
          
          if (!col_name %in% colnames(chains)) {
            warning("Column ", col_name, " not found in chains; skipping.")
            return(NULL)
          }
          
          draws <- chains[, col_name]
          
          tibble::tibble(
            year     = yr,
            hab_type = hab,
            species  = sp,
            prob_pos = mean(draws > 0)
          )
        })
      })
    }
  }) -> out
  
  if (!is.null(species_code_map)) {
    out$species <- dplyr::recode(out$species, !!!species_code_map)
  }
  
  out
}


# for pb, just in case
# calc_prob_positive <- function(chains,
#                                year_levels,
#                                species_levels = sl_levels,
#                                hab_levels = NULL,
#                                var = "d",
#                                species_code_map = NULL) {
#   
#   purrr::map_dfr(seq_along(year_levels), function(yr_idx) {
#     yr <- year_levels[yr_idx]
#     
#     if (is.null(hab_levels)) {
#       # original 2-index case: var[yr, species]
#       purrr::map_dfr(species_levels, function(sp) {
#         sp_idx <- match(sp, species_levels)
#         col_name <- paste0(var, "[", yr_idx, ",", sp_idx, "]")
#         
#         if (!col_name %in% colnames(chains)) {
#           warning("Column ", col_name, " not found in chains; skipping.")
#           return(NULL)
#         }
#         
#         draws <- chains[, col_name]
#         
#         tibble::tibble(
#           year     = yr,
#           species  = sp,
#           prob_pos = mean(draws > 0)
#         )
#       })
#     } else {
#       # 3-index case: var[yr, hab, species]
#       purrr::map_dfr(seq_along(hab_levels), function(hab_idx) {
#         hab <- hab_levels[hab_idx]
#         
#         purrr::map_dfr(species_levels, function(sp) {
#           sp_idx <- match(sp, species_levels)
#           col_name <- paste0(var, "[", yr_idx, ",", hab_idx, ",", sp_idx, "]")
#           
#           if (!col_name %in% colnames(chains)) {
#             warning("Column ", col_name, " not found in chains; skipping.")
#             return(NULL)
#           }
#           
#           draws <- chains[, col_name]
#           
#           tibble::tibble(
#             year     = yr,
#             hab_type = hab,
#             species  = sp,
#             prob_pos = mean(draws > 0)
#           )
#         })
#       })
#     }
#   }) -> out
#   
#   if (!is.null(species_code_map)) {
#     out$species <- dplyr::recode(out$species, !!!species_code_map)
#   }
#   
#   out
# }


pb_scales <- function(){
  list(
    scale_fill_discrete(name="",
                        breaks=c(0, 1),
                        labels=c(c)
    ), 
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c(0, 1),
                        labels=c("above", "below")
    )
  )
}

pb_scales_stn <- function(){
  list(
    scale_fill_discrete(name="",
                        breaks=c("above", "below"),
                        labels=c("above", "below")
    ), 
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("above", "below"),
                        labels=c("above", "below")
    )
  )
}


gc_scales <- function(){
  list(
    scale_fill_discrete(name="",
                        breaks=c("MC-Run", "MC-Riffle", "SC-Run"),
                        labels=c(c)
                        ),
    scale_colour_manual(values=c("black", "black", "dark grey"),
                        name="",
                        breaks=c("MC-Run", "MC-Riffle", "SC-Run"),
                        labels=c("MC-Run", "MC-Riffle", "SC-Run")
                        ),
    scale_shape_manual(values = c(16, 17, 17),
                       name="",
                       breaks=c("MC-Run", "MC-Riffle", "SC-Run"),
                       labels=c("MC-Run", "MC-Riffle", "SC-Run")
                       )
    )
  }

gc_scales <- function(){
  list(
    scale_fill_discrete(name="",
                        breaks=c("MC-Run", "MC-Riffle", "SC-Run"),
                        labels=c(c)
    ),
    scale_colour_manual(
      values = c(
        "MC-Run"    = "black",
        "MC-Riffle" = "black",
        "SC-Run"    = "darkgrey"
      ),
      name = ""
    ),
    scale_shape_manual(
      values = c(
        "MC-Run"    = 17,
        "MC-Riffle" = 16,
        "SC-Run"    = 17
      ),
      name = ""
    )
  )
}

  
pb_geoms <- function(){
  list(
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)),
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)), 
    geom_point(size = 2.25, alpha = 0.6, position = position_dodge2(width = dodge_w)),
    geom_vline(xintercept = 1.5, linetype = "dashed", colour = "black")
  )
}

pb_geoms1 <- function(){
  list(
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)),
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)), 
    geom_point(size = 2.25, alpha = 0.6, position = position_dodge2(width = dodge_w))
  )
}

gc_geoms1 <- function(){
  list(
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)),
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)), 
    geom_point(size = 2.25, alpha = 0.6, position = position_dodge2(width = dodge_w))
  )
}

pb_get_plots <- function(data_source, plot_name) {
  
  names(plot_name) <- names(data_source)
  
  named_plots <- list(
    p1 = plot_name$`AS-1+`,
    p2 = plot_name$`BT-1+`,
    p3 = plot_name$`BT-0+`,
    p4 = plot_name$`AS-0+`
  )
}


save_plots <- function(plot_obj, prefix = "", out_dir = "Figures/PB_figs"){
  
  # confirm the expected names exist
  needed <- c("p1", "p2", "p3", "p4")
  missing_nm <- setdiff(needed, names(plot_obj))
  if (length(missing_nm) > 0) {
    stop("plot_obj is missing: ", paste(missing_nm, collapse = ", "),
         "\nAvailable names: ", paste(dput(names(plot_obj)), collapse = ", "))
  }
  
  # make sure the output folder exists
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  plots <- list(
    `AS-1+` = plot_obj$p1,
    `BT-1+` = plot_obj$p2,
    `BT-0+` = plot_obj$p3,
    `AS-0+` = plot_obj$p4
  )
  
  for (nm in names(plots)) {
    outfile <- file.path(out_dir, paste0(prefix, nm, ".png"))
    
    tryCatch({
      ggsave(
        filename = outfile,
        plot = plots[[nm]],
        width = 6, height = 4, dpi = 300
      )
      message(nm, " saved -> ", outfile, " | exists: ", file.exists(outfile))
    }, error = function(e) {
      message("FAILED on ", nm, ": ", conditionMessage(e))
    })
  }
}

make_grid_plot <- function(plot_obj, 
                           y_axis_label,
                           prefix = "",
                           order = c("p4", "p1", "p3", "p2"),
                           labels = c("AS-0+", "AS-1+", "BT-0+", "BT-1+"),
                           ncol = 2,
                           margin_t = 5, margin_r = 0, margin_b = 0, margin_l = 5,
                           hjust = -1.5, vjust = 1.25,
                           scale = 0.9,
                           base_height = 6, base_width = 10) {
  
  # clean each plot: strip axis titles, standardize margins
  clean_plots <- lapply(plot_obj[order], function(p) {
    p + 
      theme(axis.title = element_blank()) +
      theme(plot.margin = margin(t = margin_t, r = margin_r, 
                                 b = margin_b, l = margin_l, "pt"))
  })
  
  grid_plot <- plot_grid(plotlist = clean_plots,
                         ncol = ncol, align = "hv", axis = "tblr",
                         scale = scale,
                         labels = labels,
                         hjust = hjust,
                         vjust = vjust)
  
  # add shared axis labels
  final_plot <- ggdraw(grid_plot) +
    draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
    draw_label(y_axis_label, x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14) +
    theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
  
  save_plot(outputs_path(paste0(prefix, ".png")),
            final_plot,
            base_height = base_height,
            base_width = base_width,
            bg = "white")
  
  final_plot
}













extract_cri <- function(samples, param_pattern,
                        probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  chains <- do.call(rbind, samples)
  cols   <- grep(param_pattern, colnames(chains), value = TRUE)
  if (length(cols) == 0) return(tibble(param = character(0)))
  mat <- chains[, cols, drop = FALSE]
  if (ncol(mat) == 1) {
    q <- quantile(mat[, 1], probs = probs)
    result <- as.data.frame(t(q))
    setNames(result, paste0("q", probs * 100)) |>
      mutate(param = cols) |>
      select(param, everything())
  } else {
    apply(mat, 2, quantile, probs = probs) |>
      t() |>
      as.data.frame() |>
      setNames(paste0("q", probs * 100)) |>
      rownames_to_column("param")
  }
}


filter_excluded_year_sl <- function(df) {
  if (!exists("excluded_year_sl") || nrow(excluded_year_sl) == 0) return(df)
  if ("species" %in% names(df) && "species" %in% names(excluded_year_sl)) {
    return(anti_join(df, excluded_year_sl, by = c("year", "species")))
  }
  if ("sl_idx" %in% names(df)) {
    return(anti_join(df, excluded_year_sl, by = c("year", "sl_idx")))
  }
  df
}





#########Virtually all of the below are deprecated except calc_prob_positive###########
# Helper functions -----

# remove axis.title from a list of plots (any length, not just 4)
clean_axis <- function(...) {
  plots <- list(...)
  purrr::map(plots, ~ .x + theme(axis.title = element_blank()))
}

# arrange cleaned plots into a 2x2 (or ncol-defined) grid with labels
order_plot <- function(plot_list, codes, order = seq_along(plot_list),
                       hjust = -1.55, vjust = 1.25, ncol = 2) {
  plot_grid(plotlist = plot_list[order],
            ncol = ncol, align = "hv", axis = "tblr",
            scale = 0.9,
            labels = codes[order],
            hjust = hjust,
            vjust = vjust)
}


summarize_chains_by_group <- function(chains, raw_df, trt_df,
                                      year_levels, species_levels,
                                      trt_levels = c(0L, 1L),
                                      station_levels = NULL,
                                      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                      species_code_map = NULL) {
  out <- purrr::map_dfr(year_levels, function(yr) {
    purrr::map_dfr(species_levels, function(sp) {
      purrr::map_dfr(trt_levels, function(t) {
        # station-level branch
        if (!is.null(station_levels)) {
          purrr::map_dfr(station_levels, function(stn) {
            idx <- which(raw_df$year == yr & raw_df$species == sp &
                           trt_df$treat == t & raw_df$station == stn)
            if (length(idx) == 0) return(NULL)
            
            d_chains <- sapply(idx, function(i) chains[, paste0("d[", i, "]")])
            group_mean <- if (is.matrix(d_chains)) rowMeans(d_chains) else d_chains
            q <- unname(quantile(group_mean, probs = probs))
            
            tibble(year = yr, species = sp, trt = t, station = stn,
                   n_sites = length(idx),
                   q2.5 = q[1], q25 = q[2], q50 = q[3], q75 = q[4], q97.5 = q[5])
          })  # closes purrr::map_dfr(station_levels, ...)
          
          # pooled (original) branch
        } else {
          idx <- which(raw_df$year == yr & raw_df$species == sp & trt_df$treat == t)
          if (length(idx) == 0) return(NULL)
          
          d_chains <- sapply(idx, function(i) chains[, paste0("d[", i, "]")])
          group_mean <- if (is.matrix(d_chains)) rowMeans(d_chains) else d_chains
          q <- unname(quantile(group_mean, probs = probs))
          
          tibble(year = yr, species = sp, trt = t,
                 n_sites = length(idx),
                 q2.5 = q[1], q25 = q[2], q50 = q[3], q75 = q[4], q97.5 = q[5])
        }
      })
    })
  })
  if (!is.null(species_code_map)) {
    out$species <- dplyr::recode(out$species, !!!species_code_map)
  }
  out
}




summarize_chains_by_index <- function(chains,
                                      year_levels,
                                      species_levels = sl_levels,
                                      var = "d",
                                      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                      species_code_map = NULL) {
  
  purrr::map_dfr(seq_along(year_levels), function(yr_idx) {
    yr <- year_levels[yr_idx]
    
    purrr::map_dfr(species_levels, function(sp) {
      sp_idx <- match(sp, species_levels)
      
      col_name <- paste0(var, "[", yr_idx, ",", sp_idx, "]")
      
      if (!col_name %in% colnames(chains)) {
        warning("Column ", col_name, " not found in chains; skipping.")
        return(NULL)
      }
      
      draws <- chains[, col_name]
      q <- unname(quantile(draws, probs = probs))
      
      tibble::tibble(
        year    = yr,
        species = sp,
        q2.5    = q[1], q25 = q[2], q50 = q[3], q75 = q[4], q97.5 = q[5]
      )
    })
  }) -> out
  
  if (!is.null(species_code_map)) {
    out$species <- dplyr::recode(out$species, !!!species_code_map)
  }
  
  out
}


# thius is supposed to be a generalized form of the above
summarize_chains_by_index1 <- function(chains,
                                      year_levels,
                                      species_levels = sl_levels,
                                      hab_levels = NULL,
                                      var = "d",
                                      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                      species_code_map = NULL) {
  
  if (is.null(hab_levels)) {
    
    # Original 2D version: [year,species]
    
    out <- purrr::map_dfr(seq_along(year_levels), function(yr_idx) {
      
      yr <- year_levels[yr_idx]
      
      purrr::map_dfr(species_levels, function(sp) {
        
        sp_idx <- match(sp, species_levels)
        
        col_name <- paste0(var, "[", yr_idx, ",", sp_idx, "]")
        
        if (!col_name %in% colnames(chains)) {
          warning("Column ", col_name, " not found in chains; skipping.")
          return(NULL)
        }
        
        draws <- chains[, col_name]
        q <- unname(quantile(draws, probs = probs))
        
        tibble::tibble(
          year = yr,
          species = sp,
          q2.5 = q[1],
          q25 = q[2],
          q50 = q[3],
          q75 = q[4],
          q97.5 = q[5]
        )
      })
    })
    
  } else {
    
    # New 3D version: [year,habitat,species]
    
    out <- purrr::map_dfr(seq_along(year_levels), function(yr_idx) {
      
      yr <- year_levels[yr_idx]
      
      purrr::map_dfr(seq_along(hab_levels), function(hab_idx) {
        
        hab <- hab_levels[hab_idx]
        
        purrr::map_dfr(species_levels, function(sp) {
          
          sp_idx <- match(sp, species_levels)
          
          col_name <- paste0(
            var, "[",
            yr_idx, ",",
            hab_idx, ",",
            sp_idx, "]"
          )
          
          if (!col_name %in% colnames(chains)) {
            warning("Column ", col_name, " not found in chains; skipping.")
            return(NULL)
          }
          
          draws <- chains[, col_name]
          q <- unname(quantile(draws, probs = probs))
          
          tibble::tibble(
            year = yr,
            habitat = hab,
            species = sp,
            q2.5 = q[1],
            q25 = q[2],
            q50 = q[3],
            q75 = q[4],
            q97.5 = q[5]
          )
        })
      })
    })
  }
  
  if (!is.null(species_code_map)) {
    out$species <- dplyr::recode(out$species, !!!species_code_map)
  }
  
  out
}

calc_prob_positive <- function(chains,
                               year_levels,
                               species_levels = sl_levels,
                               var = "d",
                               species_code_map = NULL) {
  
  purrr::map_dfr(seq_along(year_levels), function(yr_idx) {
    yr <- year_levels[yr_idx]
    
    purrr::map_dfr(species_levels, function(sp) {
      sp_idx <- match(sp, species_levels)
      
      col_name <- paste0(var, "[", yr_idx, ",", sp_idx, "]")
      
      if (!col_name %in% colnames(chains)) {
        warning("Column ", col_name, " not found in chains; skipping.")
        return(NULL)
      }
      
      draws <- chains[, col_name]
      
      tibble::tibble(
        year     = yr,
        species  = sp,
        prob_pos = mean(draws > 0)
      )
    })
  }) -> out
  
  if (!is.null(species_code_map)) {
    out$species <- dplyr::recode(out$species, !!!species_code_map)
  }
  
  out
}



calc_prob_positive1 <- function(chains,
                               year_levels,
                               species_levels = sl_levels,
                               hab_levels = NULL,
                               var = "d",
                               species_code_map = NULL) {
  
  if (is.null(hab_levels)) {
    
    # Original 2D case: [year,species]
    
    out <- purrr::map_dfr(seq_along(year_levels), function(yr_idx) {
      
      yr <- year_levels[yr_idx]
      
      purrr::map_dfr(species_levels, function(sp) {
        
        sp_idx <- match(sp, species_levels)
        
        col_name <- paste0(var, "[", yr_idx, ",", sp_idx, "]")
        
        if (!col_name %in% colnames(chains)) {
          warning("Column ", col_name, " not found in chains; skipping.")
          return(NULL)
        }
        
        draws <- chains[, col_name]
        
        tibble::tibble(
          year = yr,
          species = sp,
          prob_pos = mean(draws > 0)
        )
      })
    })
    
  } else {
    
    # New 3D case: [year,habitat,species]
    
    out <- purrr::map_dfr(seq_along(year_levels), function(yr_idx) {
      
      yr <- year_levels[yr_idx]
      
      purrr::map_dfr(seq_along(hab_levels), function(hab_idx) {
        
        hab <- hab_levels[hab_idx]
        
        purrr::map_dfr(species_levels, function(sp) {
          
          sp_idx <- match(sp, species_levels)
          
          col_name <- paste0(
            var, "[",
            yr_idx, ",",
            hab_idx, ",",
            sp_idx, "]"
          )
          
          if (!col_name %in% colnames(chains)) {
            warning("Column ", col_name, " not found in chains; skipping.")
            return(NULL)
          }
          
          draws <- chains[, col_name]
          
          tibble::tibble(
            year = yr,
            habitat = hab,
            species = sp,
            prob_pos = mean(draws > 0)
          )
        })
      })
    })
  }
  
  if (!is.null(species_code_map)) {
    out$species <- dplyr::recode(out$species, !!!species_code_map)
  }
  
  out
}


# shared internals: filter + optional NA-masking + legend theme
.panel_base <- function(df, species_code, na_year, show_legend, legend_pos) {
  d <- dplyr::filter(df, species == species_code)
  if (!is.null(na_year)) {
    d[d$year == na_year, c("q2.5", "q25", "q50", "q75", "q97.5")] <- NA
  }
  legend_theme <- if (show_legend) {
    theme(legend.position = legend_pos,
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.title = element_blank(),
          legend.key.size = grid::unit(0.4, "cm"))
  } else {
    theme(legend.position = "none")
  }
  list(data = d, legend_theme = legend_theme)
}

# original: two groups (above/below treatment)
make_ci_panel <- function(df, species_code = NULL, y_lab, x_lab = "Year",
                          dodge_w = 0.5,
                          na_year = NULL,
                          show_legend = FALSE,
                          legend_pos = c(0.45, 0.88),
                          vline_x = NULL,
                          hline_y = NULL,
                          trt_colors = c("above" = "black", "below" = "grey60"),
                          y_limits = NULL) {
  base <- .panel_base(df, species_code = NULL, na_year, show_legend, legend_pos)
  d    <- base$data
  
  trt_labels <- names(trt_colors)
  
  ggplot(d, aes(x = factor(year), y = q50,
                colour = factor(trt, levels = c(0, 1), labels = trt_labels),
                group  = factor(trt))) +
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, alpha = 0.6,
                   position = position_dodge2(width = dodge_w)) +
    geom_point(size = 2.25, alpha = 0.6, position = position_dodge2(width = dodge_w)) +
    (if (!is.null(vline_x)) geom_vline(xintercept = vline_x, linetype = "dashed", colour = "black")) +
    (if (!is.null(hline_y)) geom_hline(yintercept = hline_y, linetype = "dashed", colour = "gray30")) +
    scale_colour_manual(values = trt_colors, name = "Treatment") +
    scale_y_continuous(limits = y_limits,
                       expand = expansion(mult = c(0.05, 0.18))) +
    theme_bw() +
    base$legend_theme +
    ylab(y_lab) + xlab(x_lab)  
}

# new: single estimate per year, no treatment grouping
make_ci_panel_single <- function(df, species_code, y_lab, x_lab = "Year", 
                                 na_year = NULL,
                                 show_legend = FALSE,
                                 legend_pos = c(0.45, 0.88),
                                 point_colour = "black",
                                 y_limits = NULL,
                                 vline_x = NULL,
                                 hline_y = NULL, 
                                 show_prob_pos = FALSE,
                                 prob_pos_size = 3.2) {
  base <- .panel_base(df, species_code, na_year, show_legend, legend_pos)
  d    <- base$data
  
  ggplot(d, aes(x = factor(year), y = q50, group = 1)) +
    geom_linerange(aes(ymin = q2.5, ymax = q97.5),
                   linewidth = 0.6, alpha = 0.6, colour = point_colour) +
    geom_linerange(aes(ymin = q25, ymax = q75),
                   linewidth = 1.6, colour = point_colour) +
    geom_point(size = 3, colour = point_colour) +
    (if (!is.null(vline_x)) geom_vline(xintercept = vline_x, linetype = "dashed", colour = "black")) +
    (if (!is.null(hline_y)) geom_hline(yintercept = hline_y, linetype = "dashed", colour = "gray30")) +
    (if (show_prob_pos) geom_text(aes(y = q97.5, label = round(prob_pos, 2)),
                                  vjust = -0.5, size = prob_pos_size, colour = point_colour)) +
    scale_y_continuous(limits = y_limits,
                       expand = expansion(mult = c(0.05, 0.18))) +
    theme_bw() +
    base$legend_theme +
    ylab(y_lab) + xlab(x_lab) 
}

save_panels <- function(plots, codes, prefix, width = 10, height = 8) {
  walk2(plots, codes, ~ ggsave(
    filename = outputs_path(paste0(prefix, .y, "_cri.png")),
    plot = .x, width = width, height = height, units = "in"
  ))
}


combine_panel_grid <- function(plot_list, codes, order = c(4, 1, 3, 2),
                               x_lab, y_lab,
                               hjust = -1.55, vjust = 1.25,
                               out_file = NULL,
                               base_height = 6, base_width = 10) {
  
  cleaned <- purrr::map(plot_list, ~ .x + theme(axis.title = element_blank()))
  
  grid <- plot_grid(plotlist = cleaned[order],
                    ncol = 2, align = "hv", axis = "tblr",
                    scale = 0.9,
                    labels = codes[order],
                    hjust = hjust, vjust = vjust)
  
  final <- ggdraw(grid) +
    draw_label(x_lab, x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
    draw_label(y_lab, x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14)
  
  if (!is.null(out_file)) {
    save_plot(out_file, final, base_height = base_height, base_width = base_width, bg = "white")
  }
  
  final
}



ridge_plot_fun <- function(dat){
  ggplot(dat,
         aes(x = Length.mm,
             y = factor(Year),
             fill = trt,
             group = interaction(Year, trt))) +
    geom_density_ridges(
      jittered_points = TRUE,
      position = position_points_jitter(width = 0, height = 0),
      point_shape = '|',
      point_size = 3,
      alpha = 0.6
    ) + 
    geom_vline(
      xintercept = 150,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    facet_grid(trt ~ Species, 
               scales = "free_y",
               labeller = labeller(
                 trt = c(
                   con = "Control", 
                   trt = "Treatment"
                 )
               )
    ) +
    scale_fill_brewer(palette = "Set1") +
    theme_ridges() +
    labs(
      x = "Length (mm)",
      y = "Year",
      fill = "Treatment"
    ) +
    theme(
      axis.title.y = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.title.x = element_text(
        vjust = 0.5,
        hjust = 0.5
      )
    ) + 
    scale_x_continuous(breaks = seq(0,300, by = 25)) +
    theme(legend.position = "none")
  
}



hist_plot_fun <- function(dat){
  ggplot(dat,
                      aes(x = Length.mm,
                          fill = trt)) +
  geom_histogram(
    binwidth = 3,
    position = "identity",
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = 150,
    linetype = "dashed",
    color = "black",
    linewidth = 1
  ) +
  facet_grid(
    Year ~ Species,
    scales = "free_y"
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Length (mm)",
    y = "Count",
    fill = "Treatment"
  ) +
  scale_x_continuous(
    breaks = seq(0, 300, by = 25)
  ) +
  theme_bw()
}


ridge_plot_age_fun <- function(dat, title = NULL){
  ggplot(dat,
         aes(x = Length.mm,
             y = factor(Year),
             fill = stage,
             group = interaction(Year, stage))) +
    geom_density_ridges(
      jittered_points = TRUE,
      position = position_points_jitter(width = 0, height = 0),
      point_shape = '|',
      point_size = 3,
      alpha = 0.6
    ) +
    facet_grid(trt ~ sp,
               scales = "free_y",
               labeller = labeller(
                 trt = c(
                   con = "Control", 
                   trt = "Treatment"
                 )
               )
    ) +
    scale_fill_manual(
      values = c(
        "YOY" = "lightblue",
        "Older" = "darkblue"
      )
    ) +
    theme_ridges() +
    labs(
      x = "Length (mm)",
      y = "Year",
      fill = "Treatment",
      title = title
    ) +
    theme(
      axis.title.y = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.title.x = element_text(
        vjust = 0.5,
        hjust = 0.5
      )
    ) + 
    scale_x_continuous(breaks = seq(0,300, by = 25)) +
    theme(legend.position = "none")  
}


hist_plot_age_fun <- function(dat, title = NULL) {
  ggplot(dat,
         aes(x = Length.mm,
             fill = stage)) +
    geom_histogram(
      binwidth = 3,
      position = "identity",
      alpha = 0.6,
    ) +
    facet_grid(
      trt ~ sp,
      scales = "free_y",
      labeller = labeller(
        trt = c(
          con = "Control",
          trt = "Treatment"
        )
      )
    ) +
    scale_fill_manual(
      values = c(
        "YOY" = "lightblue",
        "Older" = "darkblue"
      )
    ) +
    geom_vline(
      xintercept = 150,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    labs(
      x = "Length (mm)",
      y = "Count",
      fill = "Stage",
      title = title
    ) +
    scale_x_continuous(
      breaks = seq(0, 300, by = 25)
    ) +
    theme_bw() +
    theme(
      axis.title.y = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.title.x = element_text(
        vjust = 0.5,
        hjust = 0.5
      )
    )  +
    theme(legend.position = "none")
}


hist_plot_age_stn_fun <- function(dat, fill = stage, title = NULL, vlines = NULL, bin = bin) {
  ggplot(dat, aes(x = Length.mm, fill = stage)) +
    geom_histogram(
      binwidth = bin,
      position = "identity",
      alpha = 0.6
    ) +
    {
      if(!is.null(vlines))
        geom_vline(
          data = vlines,
          aes(xintercept = xint),
          linetype = "dashed",
          color = "black",
          linewidth = 1,
          inherit.aes = FALSE
        ) 
    } +
    facet_grid(
      Station ~ sp,
      scales = "free_y"
    ) +
    scale_fill_manual(
      values = c(
        "YOY" = "lightblue",
        "Older" = "darkblue"
      )
    ) +
    labs(
      x = "Length (mm)",
      y = "Count",
      fill = "Stage",
      title = title
    ) +
    scale_x_continuous(
      breaks = seq(0, 300, by = 25)
    ) +
    theme_bw() +
    theme(
      axis.title.y = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.title.x = element_text(
        vjust = 0.5,
        hjust = 0.5
      ),
      legend.position = "none"
    )
}




hist_plot_age_all_fun <- function(dat, fill = stage, title = NULL, vlines = NULL, bin = bin){
  ggplot(dat, aes(x = Length.mm, fill = stage)) +
  geom_histogram(
    binwidth = bin,
    position = "identity",
    alpha = 0.6
  ) +
  {if(!is.null(vlines))
    geom_vline(
      data = vlines,
      aes(xintercept = xint),
      linetype = "dashed",
      color = "black",
      linewidth = 1,
      inherit.aes = FALSE
    ) 
   } +
  facet_grid(
    ~ sp,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "YOY" = "lightblue",
      "Older" = "darkblue"
    )
  ) +
  labs(
    x = "Length (mm)",
    y = "Count",
    fill = "Stage",
    title = title
  ) +
  scale_x_continuous(
    breaks = seq(0, 300, by = 25)
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 0.5
    ),
    axis.title.x = element_text(
      vjust = 0.5,
      hjust = 0.5
    ),
    legend.position = "none"
  )
}


combine_plot_lists <- function(plots_top,
                               plots_bottom,
                               ncol = 1,
                               rel_heights = c(1, 2),
                               labels = "AUTO") {
  
  map2(
    plots_top,
    plots_bottom,
    ~ plot_grid(
      .x, .y,
      ncol = ncol,
      rel_heights = rel_heights,
      labels = labels,
      align = "v",
      axis = "lr"
    )
  )
}

save_plot_list <- function(plot_list,
                           file_prefix,
                           out_dir = "Figures/GC_figs",
                           width = 12,
                           height = 10,
                           dpi = 300,
                           ext = "png") {
  
  walk2(
    plot_list,
    names(plot_list),
    ~ ggsave(
      filename = file.path(
        out_dir,
        paste0(file_prefix, .y, ".", ext)
      ),
      plot = .x,
      width = width,
      height = height,
      dpi = dpi
    )
  )
}




save_report_pdf <- function(
    file,
    plots = NULL,
    plots_top = NULL,
    plots_bottom = NULL,
    cover_title = NULL,
    cover_text = NULL,
    width = 12,
    height = 10,
    combine_args = list(
      ncol = 1,
      rel_heights = c(1, 2),
      labels = "AUTO",
      align = "v",
      axis = "lr"
    )
) {
  
  # Combine plots if needed
  if (is.null(plots)) {
    
    if (is.null(plots_top) || is.null(plots_bottom)) {
      stop(
        "Supply either 'plots' or both 'plots_top' and 'plots_bottom'."
      )
    }
    
    plots <- map2(
      plots_top,
      plots_bottom,
      ~ do.call(
        plot_grid,
        c(list(.x, .y), combine_args)
      )
    )
  }
  
  pdf(
    file = file,
    width = width,
    height = height,
    onefile = TRUE
  )
  
  on.exit(dev.off())
  
  # Cover page
  if (!is.null(cover_title)) {
    
    plot.new()
    
    text(
      x = 0.5,
      y = 0.8,
      labels = cover_title,
      cex = 2,
      font = 2
    )
    
    if (!is.null(cover_text)) {
      
      text(
        x = 0.5,
        y = 0.5,
        labels = paste(cover_text, collapse = "\n"),
        cex = 1.2
      )
    }
  }
  
  walk(plots, print)
  
  invisible(plots)
}




hist_plot_age_stn_fun1 <- function(dat, title = NULL, vlines = NULL, bin = bin,
                                  fill_var = "stage", gray_all = FALSE) {
  
  # Build the base aes — only map fill if we're not forcing gray
  if (gray_all) {
    p <- ggplot(dat, aes(x = Length.mm))
  } else {
    p <- ggplot(dat, aes(x = Length.mm, fill = .data[[fill_var]]))
  }
  
  p <- p +
    geom_histogram(
      binwidth = bin,
      position = "identity",
      alpha = 0.6,
      fill = if (gray_all) "gray50" else NULL
    )
  
  # Only add vlines if supplied
  if (!is.null(vlines)) {
    p <- p +
      geom_vline(
        data = vlines,
        aes(xintercept = xint),
        linetype = "dashed",
        color = "black",
        linewidth = 1,
        inherit.aes = FALSE
      )
  }
  
  p <- p +
    facet_grid(Station ~ sp,
               scales = "free_y") +
    labs(
      x = "Length (mm)",
      y = "Count",
      fill = fill_var,
      title = title
    ) +
    scale_x_continuous(breaks = seq(0, 300, by = 25)) +
    theme_bw() +
    theme(
      axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
      axis.title.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.position = "none"
    )
  
  # Only apply the manual stage-specific palette when fill_var == "stage"
  if (!gray_all && fill_var == "stage") {
    p <- p + scale_fill_manual(
      values = c("YOY" = "lightblue", "Older" = "darkblue")
    )
  }
  
  p
}



hist_plot_age_all_fun1 <- function(dat, fill_var = "stage", title = NULL, vlines = NULL,
                                  bin = bin, gray_all = FALSE) {
  
  # Build the base aes — only map fill if we're not forcing gray
  if (gray_all) {
    p <- ggplot(dat, aes(x = Length.mm))
  } else {
    p <- ggplot(dat, aes(x = Length.mm, fill = .data[[fill_var]]))
  }
  
  p <- p +
    geom_histogram(
      binwidth = bin,
      position = "identity",
      alpha = 0.6,
      fill = if (gray_all) "gray50" else NULL
    )
  
  # Only add vlines if supplied
  if (!is.null(vlines)) {
    p <- p +
      geom_vline(
        data = vlines,
        aes(xintercept = xint),
        linetype = "dashed",
        color = "black",
        linewidth = 1,
        inherit.aes = FALSE
      )
  }
  
  p <- p +
    facet_grid(~ sp,
               scales = "free_y") +
    labs(
      x = "Length (mm)",
      y = "Count",
      fill = fill_var,
      title = title
    ) +
    scale_x_continuous(breaks = seq(0, 300, by = 25)) +
    theme_bw() +
    theme(
      axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
      axis.title.x = element_text(vjust = 0.5, hjust = 0.5),
      legend.position = "none"
    )
  
  # Only apply the manual stage-specific palette when fill_var == "stage"
  if (!gray_all && fill_var == "stage") {
    p <- p + scale_fill_manual(
      values = c("YOY" = "lightblue", "Older" = "darkblue")
    )
  }
  
  p
}
# END ----
