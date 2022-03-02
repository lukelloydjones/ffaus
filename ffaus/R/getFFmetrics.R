# =====================================================
# Main function for hydrological metrics computation
# Author (that's a strong word): Luke Lloyd-Jones
# Date started: 26/02/2021
# Date updated: 31/05/2021
# =====================================================
# Updates log
#


#' Global hydrological metrics calculation and exceedance summary
#' function.
#'
#' @description
#'  Primary ffaus function that takes typically daily flow data
#'  as a 'by date' row data frame with 2 columns named (Date, Flow).
#'  The getFFMetrics function calls each of the functional flow
#'  component functions and returns a summary data frame of the
#'  containing the hydrometrics by row and water year summarised
#'  in columns.
#' @export
#' @param flow_matrix a 'by date' row data frame with 2 columns named (Date, Flow).
#' @param class_number a remnant from Python implementation. Don't use.
#' @param do_plot should diagnostic plots be generated? 1 = yes, 0 = no.
#' @param just_all_year if = 1 then only returns all year summaries with
#'                      exceedance metrics i.e., no functional flow hydrometrics.
#' @param user_q75_thresh pass a user defined 75th percentile threshold for use in calculating exceedance metrics.
#' @param user_q90_thresh pass a user defined 90th percentile threshold for use in calculating exceedance metrics.
#' @param plot_out_base file path for diagnostics plots to be written to.
#' @param dry_max_peak_flow_date the last date in which to begin looking for final peak before dry season. A key parameter to adjust if
#' dry season start dates are not being detected correctly.
#' @param dry_sensitivity controls the sensitivity of the slope threshold for the dry season start date cutoff. Smaller values allow to find curvier (:)) start dates.
#' @param min_dry_season_flow_percent Percentile of flow taken to be the threshold for dry-season start. Percentile is computed over
#'                                    flow values from the final peak to the end of the water-year.
#' @param init_broad_sigma the standard deviation of the Gaussian kernel used to smooth intra-annual flow for detection of the flow initiation. A key parameter to adjust if
#'flow initiation start dates are not being detected correctly. Default is 15 so make small adjustments first either side.
#' @param last_date_cutoff_initiation the last date that flow initiation can begin. Can be later in water year for flashy water systems.
#' @param wet_peak_detect_perc the proportion of the maximum yearly (water-year) flow that the return to wet peak must be to be considered the peak that initiates the wet period.
#' @param whipple_exceed_thresh the exceedance percentile threshold for large flood flows.
#' @param recess_init_timing_cutoff earliest accepted date of water year in which the last peak before the recession can occur.
#' @param recess_peak_filter_percentage the proportion of the flow range (Qmax - Qmin) that the final peak must exceed
#' to be considered the final peak for detection of recession start. A key parameter to adjust if recession start dates are not as expected.
#'
#' @return A list with one data frame (at this stage) named FFmetrics
#' * The data frame FFmetrics includes the exceedance metrics and the functional
#'   flow metrics in the rows and the columns are the summary value
#'   for each water year.
getFFMetrics <- function(flow_matrix,
                         class_number = 3,
                         do_plot = 0,
                         just_all_year   = NULL,
                         user_q75_thresh = NULL,
                         user_q90_thresh = NULL,
                         plot_out_base   = NULL,
                         dry_max_peak_flow_date = 250,
                         dry_sensitivity = 800,
                         min_dry_season_flow_percent = 0.125,
                         init_broad_sigma       = 15,
                         last_date_cutoff_initiation = 152,
                         wet_peak_detect_perc = 0.3,
                         whipple_exceed_thresh  = 0.95,
                         recess_init_timing_cutoff = 80,
                         recess_peak_filter_percentage = 0.5)
{
  # ------------------------------------
  # Default class number is 3
  #  - As of 12/04/2021 no other classes
  # ------------------------------------
  # For internal checking
  # flow_matrix = flow_matrix_obs; class_number = 3; do_plot = 0; just_all_year = NULL; user_q75_thresh = NULL; user_q90_thresh = NULL
  # flow_matrix = flow_matrix_cf;  class_number = 3; do_plot = 0; just_all_year = NULL; user_q75_thresh = NULL; user_q90_thresh = NULL

  #(is.null(plot_out_base))

  # This is fine. Might add in some of the other annual requested outputs to this function
  all_year_out <- calcAllYear(flow_matrix, do_plot = 1, user_q75_thresh = user_q75_thresh, user_q90_thresh = user_q90_thresh)

  if (!is.null(plot_out_base))
  {
    plot_type <- "exceedances"
    file      <- paste0(plot_out_base, "_", plot_type, ".png")
    rstudioapi::savePlotAsImage(file, format = c("png"), 1400, 800)
  }

  out_all_year <- rbind(round(all_year_out$average_annual_flows, 3),
                        round(all_year_out$standard_deviations, 3),
                        round(all_year_out$coefficient_variations, 3),
                        round(all_year_out$mags, 3),
                        round(all_year_out$tims, 0), all_year_out$freqs, all_year_out$durs,
                        all_year_out$mags_long_evnt, all_year_out$tims_init_long_evnt, all_year_out$seas_init_long_evnt,
                        all_year_out$durs_long_evnt)

  out_all_year$All_Years_Avg <- c(round(c(mean(flow_matrix$Flow, na.rm = T), stats::sd(flow_matrix$Flow, na.rm = T),
                                        stats::sd(flow_matrix$Flow, na.rm = T) / mean(flow_matrix$Flow, na.rm = T),
                                        all_year_out$mags_global, floor(rowMeans(all_year_out$tims, na.rm = T)),
                                        floor(rowMeans(all_year_out$freqs, na.rm = T)),
                                        floor(rowMeans(all_year_out$durs, na.rm = T)),
                                        floor(rowMeans(all_year_out$mags_long_evnt, na.rm = T)),
                                        floor(rowMeans(all_year_out$tims_init_long_evnt, na.rm = T))), 3),
                                        as.character(unlist(apply(all_year_out$seas_init_long_evnt, 1, function(x) {names(which(table(as.character(x)) == max(table(as.character(x)))))[1]}))),
                                        round(floor(rowMeans(all_year_out$durs_long_evnt, na.rm = T)), 3))

  rownames(out_all_year)[seq(1, 3)] <- c("Avg_Ann", "SD_Ann", "CV_Ann")

  # Return these results here if that's all you want
  if (!is.null(just_all_year))
  {
    return(out_all_year)
  }

  # Characterise the dry season. This is mostly winter in Aus.
  dry_season_start_dates <- calcStartOfdry_season(flow_matrix = flow_matrix,
                                                  class_number = class_number,
                                                  max_peak_flow_date = dry_max_peak_flow_date,
                                                  sensitivity = dry_sensitivity,
                                                  min_dry_season_flow_percent = min_dry_season_flow_percent,
                                                  do_plot = do_plot)

  if (!is.null(plot_out_base))
  {
    plot_type <- "dry_season_start_dates"
    file      <- paste0(plot_out_base, "_", plot_type, ".png")
    rstudioapi::savePlotAsImage(file, format = c("png"), 1400, 800)
  }

  # I'm really not sure there is an initiation flows flush as FF US defines it but lets look and see what the code tells us
  initiation_flows_out <- calcFlowInitiationFlushTimingsDurations(flow_matrix = flow_matrix,
                                                                  dry_season_start_dates = dry_season_start_dates,
                                                                  class_number = class_number,
                                                                  last_date_cutoff_flush = last_date_cutoff_initiation,
                                                                  peak_detect_perc = wet_peak_detect_perc,
                                                                  broad_sigma = init_broad_sigma,
                                                                  do_plot = do_plot)

  if (!is.null(plot_out_base))
  {
    plot_type <- "initiation_flows"
    file      <- paste0(plot_out_base, "_", plot_type, ".png")
    rstudioapi::savePlotAsImage(file, format = c("png"), 1400, 800)
  }

  # This is not good without initiation_flows_out$wet_dates.
  dryseason_base_out <- calcDrySeasonBaseflowDurationsMagnitude(flow_matrix = flow_matrix,
                                                                dry_season_start_dates = dry_season_start_dates,
                                                                flow_initiation_flush_dates = initiation_flows_out$start_dates,
                                                                return_to_wet_dates = initiation_flows_out$wet_dates)

  # Let's get into the highflow
  high_flow_out   <- calcHighflowAnnual(flow_matrix = flow_matrix, do_plot = do_plot)

  # This could get ugly. The recession woohoo.
  recession_tim_mag <- calcRecessionTimingMagnitude(flow_matrix = flow_matrix,
                                                    class_number = class_number,
                                                    dry_season_start_dates = dry_season_start_dates,
                                                    timing_cutoff =  recess_init_timing_cutoff,
                                                    peak_filter_percentage = recess_peak_filter_percentage,
                                                    do_plot = 1)
  recession_dur     <- calcRecessionDuration(recession_timings = recession_tim_mag$timings,     dry_season_start_dates =    dry_season_start_dates)
  recession_roc     <- calcRecessionRoc(flow_matrix = flow_matrix,
                                        recession_timings = recession_tim_mag$timings,
                                        dry_season_start_dates = dry_season_start_dates)

  if (!is.null(plot_out_base))
  {
    plot_type <- "recession_timing_duration"
    file      <- paste0(plot_out_base, "_", plot_type, ".png")
    rstudioapi::savePlotAsImage(file, format = c("png"), 1400, 800)
  }

  # Wet season base flows
  wet_season_bf    <- calcWetSeasonBaseflow(flow_matrix = flow_matrix,
                                            wet_timings = initiation_flows_out$wet_dates,
                                            recession_timings = recession_tim_mag$timings,
                                            dry_season_start_dates = dry_season_start_dates,
                                            do_plot = 1)

  if (!is.null(plot_out_base))
  {
    plot_type <- "wet_season_bf"
    file      <- paste0(plot_out_base, "_", plot_type, ".png")
    rstudioapi::savePlotAsImage(file, format = c("png"), 1400, 800)
  }

  # Whipple extras
  whipple_hf      <- calcWhippleHighFlow(flow_matrix, whipple_exceed_thresh = 0.95)

  # -----------------------------------
  # Package up
  # -----------------------------------

  Avg_Ann        <- round(all_year_out$average_annual_flows, 3)
  SD_Ann         <- round(all_year_out$standard_deviations, 3)
  CV_Ann         <- round(all_year_out$coefficient_variations, 3)

  Init_Mag       <- initiation_flows_out$mags
  Init_Tim       <- initiation_flows_out$start_dates
  Init_Dur       <- initiation_flows_out$durations

  Wet_BFL_Mag_10 <- wet_season_bf$wet_baseflows_10
  Wet_BFL_Mag_50 <- wet_season_bf$wet_baseflows_50
  Wet_Tim        <- initiation_flows_out$wet_dates
  Wet_BFL_Dur    <- wet_season_bf$wet_bfl_durs

  Peak_Mag_Ann   <- as.numeric(high_flow_out$peak_magnitude)
  Peak_Mag_Tim   <- as.numeric(high_flow_out$peak_magnitude_tim)

  Peak_Mag_95  <- high_flow_out$magnitude["HFE0.95", ]
  Peak_Mag_90  <- high_flow_out$magnitude["HFE0.9", ]
  Peak_Mag_80  <- high_flow_out$magnitude["HFE0.8", ]

  Peak_Dur_95  <- high_flow_out$duration["HFE0.95", ]
  Peak_Dur_90  <- high_flow_out$duration["HFE0.9", ]
  Peak_Dur_80  <- high_flow_out$duration["HFE0.8", ]

  Peak_Fre_95  <- high_flow_out$freq["HFE0.95", ]
  Peak_Fre_90  <- high_flow_out$freq["HFE0.9", ]
  Peak_Fre_80  <- high_flow_out$freq["HFE0.8", ]

  Peak_Tim_95  <- high_flow_out$timing["HFE0.95", ]
  Peak_Tim_90  <- high_flow_out$timing["HFE0.9", ]
  Peak_Tim_80  <- high_flow_out$timing["HFE0.8", ]

  Rec_Mag        <- recession_tim_mag$magnitudes
  Rec_Tim        <- recession_tim_mag$timings
  Rec_Dur        <- recession_dur$duration
  Rec_ROC        <- recession_roc$rocs_only_neg

  Dry_Mag_90     <- dryseason_base_out$dry_season_90_magnitudes
  Dry_Mag_50     <- dryseason_base_out$dry_season_50_magnitudes
  Dry_Mag_10     <- dryseason_base_out$dry_season_10_magnitudes
  Dry_Tim        <- dry_season_start_dates
  Dry_Dur        <- dryseason_base_out$dry_season_wet_durations
  Dry_NoFL_Days  <- dryseason_base_out$dry_season_no_flow_counts

  # Table up
  Avg_Ann        <- round(all_year_out$average_annual_flows, 3)
  SD_Ann         <- round(all_year_out$standard_deviations, 3)
  CV_Ann         <- round(all_year_out$coefficient_variations, 3)
  Yr_Mags        <- t(all_year_out$mags)

  res_out <- t(data.frame( Init_Mag       = t(Init_Mag), Init_Tim = t(Init_Tim), Init_Dur = t(Init_Dur),
                           Wet_BFL_Mag_10 = t(Wet_BFL_Mag_10), Wet_BFL_Mag_50 = t(Wet_BFL_Mag_50), Wet_Tim = t(Wet_Tim), Wet_BFL_Dur = t(Wet_BFL_Dur),
                           Peak_Mag_Ann   = Peak_Mag_Ann, Peak_Mag_Tim = Peak_Mag_Tim,
                           Peak_Mag_95    = Peak_Mag_95,  Peak_Mag_90 = Peak_Mag_90, Peak_Mag_80 = Peak_Mag_80,
                           Peak_Dur_95    = Peak_Dur_95,  Peak_Dur_90 = Peak_Dur_90, Peak_Dur_80 = Peak_Dur_80,
                           Peak_Fre_95    = Peak_Fre_95,  Peak_Fre_90 = Peak_Fre_90, Peak_Fre_80 = Peak_Fre_80,
                           Peak_Tim_95    = Peak_Tim_95,  Peak_Tim_90 = Peak_Tim_90, Peak_Tim_80 = Peak_Tim_80,
                           Rec_Mag        = t(Rec_Mag),    Rec_Tim    = t(Rec_Tim),    Rec_Dur = t(Rec_Dur), Rec_ROC = t(Rec_ROC),
                           Dry_Mag_90     = t(Dry_Mag_90), Dry_Mag_50 = t(Dry_Mag_50), Dry_Mag_10 = t(Dry_Mag_10),
                           Dry_Tim        = t(Dry_Tim),    Dry_Dur    = t(Dry_Dur),    Dry_NoFL_Days = t(Dry_NoFL_Days)))
  res_out <- rbind(res_out, whipple_hf[-1, colnames(res_out)])
  res_out <- round(res_out, 3)
  # Generate some averages over all years
  all_yr_avgs <- c(rowMeans(res_out[grep("Init_Mag", rownames(res_out)):dim(res_out)[1], ], na.rm = T))
  res_out$All_Years_Avg <-  round(all_yr_avgs, 3)
  res_out <- rbind(out_all_year, res_out)

  return(list(FFmetrics = res_out))
}
