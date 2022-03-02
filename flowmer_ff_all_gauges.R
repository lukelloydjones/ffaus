# =========================================================
# Take a look at the data for the gauges of the vegetation
# analysis
# Author: Luke Lloyd-Jones
# Date started: 08/06/2021
# Date updated: 17/06/2021
# =========================================================
# library(devtools)
# devtools::install("ffaus") # Install the current state of the package
# Source the library dependencies
library(ffaus)

# --------------------------------------------------------------------
# Test data read for comparison 
# --------------------------------------------------------------------
path_to_flw_dta <- "../func_flow_r_aus/data/"
mer_hydrology <- read.csv(path_to_flw_dta, "FlowMER_Streamflow_2014-20.csv")
prg_gg        <- unique(mer_hydrology[, c("Valley", "Site")]) # 100 Gauges
prg_gg$Valley_Long <- prg_gg$Valley

# --------------------------------------------------------------------
# Let's add in the longer valley name to make it 
# easier to match to mer_hydrology_sites.xls file
# THE KEY
# --------------------------------------------------------------------
prg_gg$Valley_Long[prg_gg$Valley == "BDL"] <- "Barwon Darling"
prg_gg$Valley_Long[prg_gg$Valley == "BRD"] <- "Border Rivers"
prg_gg$Valley_Long[prg_gg$Valley == "BRK"] <- "Broken"
prg_gg$Valley_Long[prg_gg$Valley == "CDB"] <- "Condamine Balonne"
prg_gg$Valley_Long[prg_gg$Valley == "CMP"] <- "Campaspe"
prg_gg$Valley_Long[prg_gg$Valley == "CNM"] <- "Central Murray"
prg_gg$Valley_Long[prg_gg$Valley == "EWK"] <- "Edward Wakool"
prg_gg$Valley_Long[prg_gg$Valley == "GLB"] <- "Goulburn"
prg_gg$Valley_Long[prg_gg$Valley == "GWY"] <- "Gwydir"
prg_gg$Valley_Long[prg_gg$Valley == "LCH"] <- "Lachlan"
prg_gg$Valley_Long[prg_gg$Valley == "LDL"] <- "Lower Darling"
prg_gg$Valley_Long[prg_gg$Valley == "LOD"] <- "Loddon"
prg_gg$Valley_Long[prg_gg$Valley == "LWM"] <- "Lower Murray"
prg_gg$Valley_Long[prg_gg$Valley == "MBG"] <- "Murrumbidgee"
prg_gg$Valley_Long[prg_gg$Valley == "MCQ"] <- "Macquarie"
prg_gg$Valley_Long[prg_gg$Valley == "NAM"] <- "Namoi"
prg_gg$Valley_Long[prg_gg$Valley == "OVN"] <- "Ovens"
prg_gg$Valley_Long[prg_gg$Valley == "WRG"] <- "Warrego"

table(prg_gg$Valley)
val_site <- prg_gg

# ----------------------------------------------------
# Cycle over all the gauges and generate the FF -
# hydro metrics
# ---------------------------------------------------- 
# Set up the special par lists
dflt_pars  <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, 
                   last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst <- rep(list(dflt_pars), 100)
names(is_special_lst) <- as.character(val_site$Site)
is_special_lst$YancoOfftake       <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 50, recess_peak_filter_percentage = 0.5)
is_special_lst$Eppalock           <- list(dry_max_peak_flow_date = 230, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 50, recess_peak_filter_percentage = 0.5)
is_special_lst$Rochester          <- list(dry_max_peak_flow_date = 230, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 50, recess_peak_filter_percentage = 0.5)
is_special_lst$'Mallan School'    <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 12, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 50, recess_peak_filter_percentage = 0.5)
is_special_lst$'Barham Moulamien' <- list(dry_max_peak_flow_date = 330, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$ColligenOfftake    <- list(dry_max_peak_flow_date = 330, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$Pinegrove          <- list(dry_max_peak_flow_date = 330, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$Pallamallawa       <- list(dry_max_peak_flow_date = 330, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Brageen            <- list(dry_max_peak_flow_date = 330, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.3)
is_special_lst$Allambie           <- list(dry_max_peak_flow_date = 330, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.3)
is_special_lst$Garah              <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 12, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.3)
is_special_lst$Condobolin         <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 12, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$Willandra          <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 500, min_dry_season_flow_percent = 0.125, init_broad_sigma = 12, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$Brewster           <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.2)
is_special_lst$Booligal           <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.2)
is_special_lst$Hillston           <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.2)
is_special_lst$Merrimajeel        <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 30, recess_peak_filter_percentage = 0.5)
is_special_lst$Whealbah           <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 500, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 30, recess_peak_filter_percentage = 0.5)
is_special_lst$CairnCurran        <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 50, recess_peak_filter_percentage = 0.5)
is_special_lst$Laanecoorie        <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.3)
is_special_lst$Serpentine         <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.3)
is_special_lst$Appin              <- list(dry_max_peak_flow_date = 220, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Baroona            <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Warren             <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Marebone           <- list(dry_max_peak_flow_date = 320, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.3)
is_special_lst$Wangaratta         <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.2)
is_special_lst$Wellington         <- list(dry_max_peak_flow_date = 220, dry_sensitivity = 500, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$StGeorge           <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$Flinton            <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Farnbro            <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Goondiwindi        <- list(dry_max_peak_flow_date = 310, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Louth              <- list(dry_max_peak_flow_date = 180, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Bourke             <- list(dry_max_peak_flow_date = 250, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Eildon             <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 500, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$Murchison          <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 500, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)
is_special_lst$Piallamore         <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Gunnedah           <- list(dry_max_peak_flow_date = 300, dry_sensitivity = 800, min_dry_season_flow_percent = 0.125, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 60, recess_peak_filter_percentage = 0.5)
is_special_lst$Bulpunga           <- list(dry_max_peak_flow_date = 320, dry_sensitivity = 500, min_dry_season_flow_percent = 0.200, init_broad_sigma = 15, last_date_cutoff_initiation = 152, wet_peak_detect_perc = 0.30, recess_init_timing_cutoff = 80, recess_peak_filter_percentage = 0.5)

# Where do you want the files written to?

out_file     <- "../func_flow_r_aus/flowmer_all_gauges_ffaus"

# --------------------------
# Now cycle over and compute
# --------------------------

for (i in seq(1, dim(val_site)[1]))
{
  rm(flow_matrix_obs, flow_fil_flw, flow_matrix_cf)
  
  valley <- val_site$Valley_Long[i]
  site   <- val_site$Site[i]
  
  out_file_val <- paste0(out_file, "/", valley)
  out_file_2   <- paste0(out_file_val, "/", site)
   
  if (!dir.exists(out_file)) dir.create(out_file)
  dir.create(out_file_val)
  dir.create(out_file_2)
  
  gg              <- prg_gg[which(prg_gg$Valley_Long  == valley & prg_gg$Site == site), ]
  flow_fil        <- dplyr::filter(mer_hydrology, Site == gg$Site & Category == "Flow")
  flow_fil_gg     <- flow_fil[, c("Date", "Flow")]
  str_dt          <- "07-01"
  flow_matrix_obs <- makeFlowMatrix(flow_fil_gg, str_dt, impute = 1)$flow_deep
  
  # ---------
  # CF values
  # ---------
  
  flow_fil_flw    <- dplyr::filter(mer_hydrology, Site == gg$Site & Category == "Flow")
  flow_fil_cew    <- dplyr::filter(mer_hydrology, Site == gg$Site & Category == "CEW")
  flow_fil_flw_cw <- dplyr::left_join(flow_fil_flw, flow_fil_cew, by = "Date")
  flow_fil_flw_cw$Flow.y[is.na(flow_fil_flw_cw$Flow.y)] <- 0
  flow_fil_flw_cw$Flow_CF <- as.numeric(flow_fil_flw_cw$Flow.x) - as.numeric(flow_fil_flw_cw$Flow.y)
  flow_fil_gg     <- flow_fil_flw_cw[, c("Date", "Flow_CF")]
  colnames(flow_fil_gg) <- c("Date", "Flow")
  flow_matrix_cf  <- makeFlowMatrix(flow_fil_gg, str_dt, impute = 1)$flow_deep
  
  # ------------------
  # Global hydrographs
  # ------------------
  
  plot_type <- "global_hydrograph"
  file <- paste0(out_file_2, "/", plot_type, ".png")
  png(filename = file, 
      bg = "white", width =  14, height = 7, units = 'in', res = 300)
  max_val  <- quantile(flow_matrix_obs$Flow, probs = 0.95) + quantile(flow_matrix_obs$Flow, probs = 0.95) / 2
  max_val2 <- quantile(flow_matrix_obs$Flow, probs = 0.90) + quantile(flow_matrix_obs$Flow, probs = 0.90) / 2
  plot(flow_matrix_obs$Date, flow_matrix_obs$Flow, 
       type = "l", lwd = 3,
       xlab = 'Date',
       ylab = 'Flow (ML/d)', col = "purple", ylim = c(0, max_val))
  lines(flow_matrix_cf$Date, flow_matrix_cf$Flow, col = "red", lwd = 2)
  flow_matrix_cf$cols <- flow_matrix_cf$Season
  flow_matrix_cf$cols <- factor(flow_matrix_cf$Season, labels = c("orange", "brown", "blue", "yellow"))
  points(flow_matrix_cf$Date, flow_matrix_cf$Flow, 
         col = as.character(flow_matrix_cf$cols), 
         cex = 0.8, pch = 16)
  legend("topleft", legend=c("Counterfactual", "Observed"),
         col=c("red", "purple"), lty=1, cex=1)
  legend("topright", legend = c("Winter", "Spring", "Summer", "Autumn"),
         col = c("blue", "yellow", "orange", "brown"), 
         pch = 16, 
         cex = 1)
  dev.off()
  
  # -----------------------
  # Observed flows compute
  # -----------------------
  
  par(mfrow = c(2, 3))
  flow_type       <- "observed"
  plot_out_base   <- paste0(out_file_2, "/", flow_type)
  if (valley == "Gwydir") # Gwydir seem to have some significant peaks happening later
  {
    spc_set <- is_special_lst[[i]]
    spc_set$dry_max_peak_flow_date <- 300
    spc_set$recess_init_timing_cutoff <- 50
    spc_set$recess_peak_filter_percentage <- 0.3
    out_metrics_obs <- getFFMetrics(flow_matrix_obs, 
                                    plot_out_base          = plot_out_base, 
                                    dry_max_peak_flow_date = spc_set$dry_max_peak_flow_date,
                                    dry_sensitivity        = spc_set$dry_sensitivity,
                                    min_dry_season_flow_percent = spc_set$min_dry_season_flow_percent,
                                    init_broad_sigma            = spc_set$init_broad_sigma,
                                    last_date_cutoff_initiation = spc_set$last_date_cutoff_initiation,
                                    wet_peak_detect_perc        = spc_set$wet_peak_detect_perc,
                                    recess_init_timing_cutoff     = spc_set$recess_init_timing_cutoff,
                                    recess_peak_filter_percentage = spc_set$recess_peak_filter_percentage, 
                                    do_plot = 1)
  } else {
    spc_set <- is_special_lst[[i]]
    out_metrics_obs <- getFFMetrics(flow_matrix_obs, 
                                    plot_out_base          = plot_out_base, 
                                    dry_max_peak_flow_date = spc_set$dry_max_peak_flow_date,
                                    dry_sensitivity        = spc_set$dry_sensitivity,
                                    min_dry_season_flow_percent = spc_set$min_dry_season_flow_percent,
                                    init_broad_sigma            = spc_set$init_broad_sigma,
                                    last_date_cutoff_initiation = spc_set$last_date_cutoff_initiation,
                                    wet_peak_detect_perc        = spc_set$wet_peak_detect_perc,
                                    recess_init_timing_cutoff     = spc_set$recess_init_timing_cutoff,
                                    recess_peak_filter_percentage = spc_set$recess_peak_filter_percentage, 
                                    do_plot = 1)
  }
  
  # -----------------------------
  # Counter factual flows compute
  # -----------------------------
  
  dev.off()
  par(mfrow = c(2, 3))
  flow_type      <- "counterfactual"
  plot_out_base  <- paste0(out_file_2, "/", flow_type)
  if (valley == "Gwydir")
  {
    spc_set <- is_special_lst[[i]]
    spc_set$dry_max_peak_flow_date <- 300
    spc_set$recess_init_timing_cutoff <- 50
    spc_set$recess_peak_filter_percentage <- 0.3
    out_metrics_cf <- getFFMetrics(flow_matrix_cf, 
                                   plot_out_base = plot_out_base, 
                                   dry_max_peak_flow_date = spc_set$dry_max_peak_flow_date,
                                   dry_sensitivity = spc_set$dry_sensitivity,
                                   min_dry_season_flow_percent = spc_set$min_dry_season_flow_percent,
                                   init_broad_sigma = spc_set$init_broad_sigma,
                                   last_date_cutoff_initiation = spc_set$last_date_cutoff_initiation,
                                   wet_peak_detect_perc = spc_set$wet_peak_detect_perc,
                                   recess_init_timing_cutoff = spc_set$recess_init_timing_cutoff,
                                   recess_peak_filter_percentage = spc_set$recess_peak_filter_percentage, 
                                   do_plot = 1)
  } else { 
    spc_set <- is_special_lst[[i]] 
    out_metrics_cf <- getFFMetrics(flow_matrix_cf, 
                                   plot_out_base = plot_out_base, 
                                   dry_max_peak_flow_date = spc_set$dry_max_peak_flow_date,
                                   dry_sensitivity = spc_set$dry_sensitivity,
                                   min_dry_season_flow_percent = spc_set$min_dry_season_flow_percent,
                                   init_broad_sigma = spc_set$init_broad_sigma,
                                   last_date_cutoff_initiation = spc_set$last_date_cutoff_initiation,
                                   wet_peak_detect_perc = spc_set$wet_peak_detect_perc,
                                   recess_init_timing_cutoff = spc_set$recess_init_timing_cutoff,
                                   recess_peak_filter_percentage = spc_set$recess_peak_filter_percentage, 
                                   do_plot = 1)
  }
  
  # ---------
  # Write out
  # ---------
  
  write.csv(out_metrics_obs$FFmetrics, file = paste0(out_file_2, "/", "observed_flow_metrics.csv"))
  write.csv(out_metrics_cf$FFmetrics,  file = paste0(out_file_2, "/", "counterfactual_flow_metrics.csv"))
}

# -----------------------------------------------
# Go through and do visual double check
#  - If things look incorrect then detail next 
#    along with the fix otherwise it gets a tick
#  - Date first check 14/06/2021
# ----------------------------------------------

#         Valley          Site       Valley_Long
# 1        MBG         Gundagai      Murrumbidgee - Tick 
# 731      MBG            Wagga      Murrumbidgee - Tick 
# 1461     MBG         Berembed      Murrumbidgee - Tick
# 2191     MBG       Narrandera      Murrumbidgee - Tick
# 2921     MBG     YancoOfftake      Murrumbidgee - Updated as counterfactual had too late peak initiation start date. - Tick
# 3651     MBG       Gogelderie      Murrumbidgee - Tick
# 4381     MBG       Darlington      Murrumbidgee - Tick
# 5111     MBG       Carrathool      Murrumbidgee - Tick
# 5841     MBG              Hay      Murrumbidgee - Tick
# 6571     MBG            Maude      Murrumbidgee - Tick
# 7301     MBG          Redbank      Murrumbidgee - Tick
# 8031     MBG        Balranald      Murrumbidgee - Tick
# 8761     CMP         Eppalock          Campaspe - 2016 dry season not being detected well - changed DSSD - 230 - Tick
# 9491     CMP        Rochester          Campaspe - 2016 dry season not being detected well - changed DSSD - 235 - Tick
# 10221    EWK       Deniliquin     Edward Wakool - Tick
# 10951    EWK     Moulamien Rd     Edward Wakool - Tick
# 11316    EWK   Gee Gee Bridge     Edward Wakool - Tick
# 11681    EWK    Mallan School     Edward Wakool - Tick
# 12046    EWK Barham Moulamien     Edward Wakool - Dry season start dates seem a bit early on default mode - changed DSSD - 330 - Tick
# 12411    EWK   Wakool Offtake     Edward Wakool - Tick
# 12776    EWK YallakoolOfftake     Edward Wakool - Tick
# 13506    EWK  ColligenOfftake     Edward Wakool - Dry season start dates seem a bit early on default mode - changed DSSD - 330 - Tick
# 14236    GWY        Pinegrove            Gwydir - Dry season start dates seem a bit early on default mode - changed DSSD - 330 - Tick
# 14966    GWY        Gravesend            Gwydir - Set forth a Gwydir wide value of DSSD = 300
# 15696    GWY     Pallamallawa            Gwydir - 2016 Recession peak is early need to set earlier - Tick
# 16426    GWY      MehiOfftake            Gwydir - Tick
# 17156    GWY        Boolooroo            Gwydir - Tick
# 17886    GWY         Yarraman            Gwydir - Tick
# 18616    GWY GinghamDiversion            Gwydir - Tick
# 19346    GWY          Brageen            Gwydir - Tick
# 20076    GWY         Allambie            Gwydir - Tick
# 20806    GWY          Millewa            Gwydir - Tick
# 21536    GWY            Moree            Gwydir - Tick
# 22266    GWY       Combadello            Gwydir - Tick
# 22996    GWY            Garah            Gwydir - Tick
# 23726    GWY          Gundare            Gwydir - Tick
# 24091    LCH            Cowra           Lachlan - Tick
# 24821    LCH           Forbes           Lachlan - Tick
# 25551    LCH       Condobolin           Lachlan - Tick
# 26281    LCH        Willandra           Lachlan - Tick
# 27011    LCH         Brewster           Lachlan - Needs a rerun with recession 0.25 - Tick
# 27741    LCH         Booligal           Lachlan - Needs a rerun with recession 0.25 - Tick
# 28106    LCH         Hillston           Lachlan - Needs a rerun with recession 0.25 - Tick
# 28471    LCH      Merrimajeel           Lachlan - Rerun with initiation start date = 30 - Tick
# 28836    LCH         Whealbah           Lachlan - Tick
# 29201    LOD      CairnCurran            Loddon - Needs a rerun with recession start peak earlier - Tick
# 29931    LOD        Tullaroop            Loddon - Tick
# 30661    LOD      Laanecoorie            Loddon - Needs a later dry-season peak date and recession start date earlier - Tick
# 31391    LOD       Serpentine            Loddon - Needs a later dry-season peak date - Tick
# 32121    LOD            Appin            Loddon - Tick
# 32851    MCQ       Burrendong         Macquarie - Tick
# 33581    MCQ          Baroona         Macquarie - Take a hit on no 2015 wet season - regigged wet start date to be less than DSSD - Tick
# 34311    MCQ           Warren         Macquarie - Same as Barronga - Tick
# 35041    MCQ         Marebone         Macquarie - Needs a later dry-season peak date, recession percentage - Tick
# 35771    OVN             King             Ovens - Tick
# 36501    OVN       Wangaratta             Ovens - Needs a rerun with recession 0.2 - Tick
# 37231    OVN          Buffalo             Ovens - Tick
# 37961    CNM          Doctors    Central Murray - Tick
# 38691    CNM       Yarrawonga    Central Murray - Tick
# 39421    CNM      Torrumbarry    Central Murray - Tick
# 40151    CNM         SwanHill    Central Murray - Tick
# 40881    CNM           Euston    Central Murray - Tick
# 41611    CNM          Lock 10    Central Murray - Tick
# 42341    LWM        FlowstoSA      Lower Murray - Tick
# 43071    LWM           Lock 6      Lower Murray - Tick
# 43801    LWM           Lock 5      Lower Murray - Tick
# 44531    LWM           Lock 4      Lower Murray - Tick
# 45261    LWM           Lock 3      Lower Murray - Tick
# 45991    LWM           Lock 2      Lower Murray - Tick
# 46721    LWM           Lock 1      Lower Murray - Tick
# 47451    LWM       Wellington      Lower Murray - Tick
# 48181    LWM         Barrages      Lower Murray - Tick No data for a few years which wrecks output
# 48911    CDB         StGeorge Condamine Balonne - Tick
# 49641    BRD          Flinton     Border Rivers - Run later dry-season start date - Tick
# 50371    BRD          Farnbro     Border Rivers - Run later dry-season start date - Tick
# 51101    BRD      Goondiwindi     Border Rivers - Run later dry-season start date - Tick
# 51831    WRG       Augathella           Warrego - Tick
# 52561    WRG       Cunnamulla           Warrego - Tick
# 53291    BDL            Louth    Barwon Darling - Earlier dry season start peak - Tick
# 54021    BDL     Collarenebri    Barwon Darling - Tick
# 54751    BDL           Bourke    Barwon Darling - Tick
# 55116    GLB           Eildon          Goulburn - Tick
# 55846    GLB          Trawool          Goulburn - Tick
# 56576    GLB        Murchison          Goulburn - Needed a late dry-seasson start - Tick
# 57306    GLB           McCoys          Goulburn - Tick
# 58036    BRK            Rices            Broken - Tick
# 58766    BRK           BackCk            Broken - Tick
# 59496    BRK      Wagarandall            Broken - Tick
# 60226    NAM          Chaffey             Namoi - Tick
# 60591    NAM       Piallamore             Namoi - Tick
# 60956    NAM        Bugilbone             Namoi - Tick
# 61321    NAM         Gunnedah             Namoi - Earlier recession date. - Tick
# 61686    NAM           Keepit             Namoi - Tick
# 62051    NAM           Mollee             Namoi - Tick
# 62416    NAM            Weeta             Namoi - Tick
# 62781    NAM          Walgett             Namoi - Tick
# 63146    LDL           Weir32     Lower Darling - Tick
# 63511    LDL         Bulpunga     Lower Darling - Tick
# 63876    LDL         Burtundy     Lower Darling - Tick
# 64241    LDL            Wycot     Lower Darling - Tick









