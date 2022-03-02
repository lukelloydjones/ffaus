# =====================================================
# Dry season baseflow, durations and timings
# Author (that's a strong word): Luke Lloyd-Jones
# Date started: 26/02/2021
# Date updated: 31/05/2021
# =====================================================
# Updates log
#
#' Dry season base flow summary.
#' @description
#' Computed dry-season baseflow duration and magnitude
#' metrics
#'
#' @export
#' @param flow_matrix A 'by date' row data frame with 2 columns named (Date, Flow).
#' @param dry_season_start_dates The water year date for each water year in which the dry-season is
#'                               computed to have begun. Computed using calcStartOfdry_season().
#' @param flow_initiation_flush_dates The water year date for each water year in which the flow initiation
#'                                    is computed to have begun. Corresponds typically to first substantial
#'                                    peak in the hydrograph post dry-season. Computed using
#'                                    calcFlowInitiationFlushTimingsDurations().
#' @param return_to_wet_dates The water year date for each water year in which the flow is beginning to
#'                            return to its wetter period. Computed using returnToWetDate().
#'
#' @return A list with elements
#' * dry_season_90_magnitudes
#' * dry_season_50_magnitudes
#' * dry_season_10_magnitudes
#' * dry_season_flush_durations
#' * dry_season_wet_durations
#' * dry_season_no_flow_counts
calcDrySeasonBaseflowDurationsMagnitude <- function(flow_matrix, dry_season_start_dates, flow_initiation_flush_dates, return_to_wet_dates)
{
  wtr_yrs                <- unique(flow_matrix$Water_Year)

  dry_season_90_magnitudes   <- data.frame(matrix(0, nrow = 1, ncol = length(wtr_yrs)))
  dry_season_50_magnitudes   <- data.frame(matrix(0, nrow = 1, ncol = length(wtr_yrs)))
  dry_season_10_magnitudes   <- data.frame(matrix(0, nrow = 1, ncol = length(wtr_yrs)))
  dry_season_flush_durations <- data.frame(matrix(0, nrow = 1, ncol = length(wtr_yrs)))
  dry_season_wet_durations   <- data.frame(matrix(0, nrow = 1, ncol = length(wtr_yrs)))
  dry_season_no_flow_counts  <- data.frame(matrix(0, nrow = 1, ncol = length(wtr_yrs)))

  colnames(dry_season_90_magnitudes)   <- wtr_yrs
  colnames(dry_season_50_magnitudes)   <- wtr_yrs
  colnames(dry_season_10_magnitudes)   <- wtr_yrs
  colnames(dry_season_flush_durations) <- wtr_yrs
  colnames(dry_season_wet_durations)   <- wtr_yrs
  colnames(dry_season_no_flow_counts)  <- wtr_yrs

  for (wtryr in seq(1, length(wtr_yrs)))
  {
    # Get the flow data for that water year
    flow_matrix_wy     <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr])

    # Get the flow data for the next year if possible i.e., not the last year
    if (wtryr != length(wtr_yrs) & (as.numeric(wtr_yrs[wtryr + 1]) - as.numeric(wtr_yrs[wtryr]) == 1) )
    {
      flow_matrix_wy_nxt <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr + 1])
    }

    flow_data_flush <- NA
    flow_data_wet   <- NA
    if ((wtryr == length(wtr_yrs)) | (as.numeric(wtr_yrs[wtryr + 1]) - as.numeric(wtr_yrs[wtryr]) > 1 ))
    {
      if (!(is.na(dry_season_start_dates[wtryr])))
      {
        su_date         <- as.numeric(dry_season_start_dates[wtryr])
        flow_data_wet   <- flow_matrix_wy$Flow[su_date:length(flow_matrix_wy$Flow)] #Just take the part of the dry season that we have
        flow_data_flush <- flow_data_wet # Make the wet and flush the same. We have equally no knowledge of both
      }
    } else {
      if (!is.na(dry_season_start_dates[wtryr]))
      {
        if (dry_season_start_dates[wtryr] > length(flow_matrix_wy$Flow))
        {
          su_date    <- length(flow_matrix_wy$Flow) - as.numeric(dry_season_start_dates[wtryr])
        } else {
          su_date         <- as.numeric(dry_season_start_dates[wtryr])
        }
        wet_date   <- as.numeric(return_to_wet_dates[wtryr + 1])
        flush_date <- as.numeric(flow_initiation_flush_dates[wtryr + 1])
        # Flush data
        if (!is.na(flush_date) & flush_date > 0 & su_date > 0)
        {
          flow_data_flush <- c(flow_matrix_wy$Flow[su_date:length(flow_matrix_wy$Flow)],
                               flow_matrix_wy_nxt$Flow[1:flush_date])

        } else if (!is.na(flush_date) & flush_date <= 0 & su_date > 0) {

          flow_data_flush <- c(flow_matrix_wy$Flow[su_date:(length(flow_matrix_wy$Flow) + flush_date)])

        } else if (!is.na(flush_date) & flush_date > 0  & su_date < 0) {

          flow_data_flush <- flow_matrix_wy_nxt$Flow[(1 - su_date):flush_date]
        } else {

          flow_data_flush <- NA
        }

        # Wet data
        if (!is.na(wet_date) & wet_date > 0 & su_date > 0)
        {
          flow_data_wet <- c(flow_matrix_wy$Flow[su_date:length(flow_matrix_wy$Flow)],
                             flow_matrix_wy_nxt$Flow[1:wet_date])

        } else if (!is.na(wet_date) & wet_date <= 0 & su_date > 0) {

          flow_data_wet <- c(flow_matrix_wy$Flow[su_date:(length(flow_matrix_wy$Flow) + wet_date)])

        } else if (!is.na(wet_date) & wet_date > 0  & su_date < 0) {

          flow_data_wet <- flow_matrix_wy_nxt$Flow[(1 - su_date):wet_date]

        } else {
          flow_data_wet <- NA
        }
      }
    }

    if (!all(is.na(flow_data_flush)) & !all(is.na(flow_data_wet)))
    {
      dry_season_90_magnitudes[wtryr]   <- stats::quantile(flow_data_wet, probs = 0.90, na.rm = T)
      dry_season_50_magnitudes[wtryr]   <- stats::quantile(flow_data_wet, probs = 0.50, na.rm = T)
      dry_season_10_magnitudes[wtryr]   <- stats::quantile(flow_data_wet, probs = 0.10, na.rm = T)
      dry_season_flush_durations[wtryr] <- length(flow_data_flush)
      dry_season_wet_durations[wtryr]   <- length(flow_data_wet)
      dry_season_no_flow_counts[wtryr]  <- length(flow_data_wet) - sum(flow_data_wet != 0)
    } else if (all(is.na(flow_data_flush)) & !all(is.na(flow_data_wet)))
    {
      dry_season_90_magnitudes[wtryr]   <- stats::quantile(flow_data_wet, probs = 0.90, na.rm = T)
      dry_season_50_magnitudes[wtryr]   <- stats::quantile(flow_data_wet, probs = 0.50, na.rm = T)
      dry_season_10_magnitudes[wtryr]   <- stats::quantile(flow_data_wet, probs = 0.10, na.rm = T)
      dry_season_flush_durations[wtryr] <- NA
      dry_season_wet_durations[wtryr]   <- length(flow_data_wet)
      dry_season_no_flow_counts[wtryr]  <- length(flow_data_wet) - sum(flow_data_wet != 0)
    }  else if (!all(is.na(flow_data_flush)) & all(is.na(flow_data_wet)))
    {
      dry_season_90_magnitudes[wtryr]   <- NA
      dry_season_50_magnitudes[wtryr]   <- NA
      dry_season_10_magnitudes[wtryr]   <- NA
      dry_season_flush_durations[wtryr] <- length(flow_data_flush)
      dry_season_wet_durations[wtryr]   <- NA
      dry_season_no_flow_counts[wtryr]  <- NA
    } else {
      dry_season_90_magnitudes[wtryr]   <- NA
      dry_season_50_magnitudes[wtryr]   <- NA
      dry_season_10_magnitudes[wtryr]   <- NA
      dry_season_flush_durations[wtryr] <- NA
      dry_season_wet_durations[wtryr]   <- NA
      dry_season_no_flow_counts[wtryr]  <- NA
    }
  }
  return(list(dry_season_90_magnitudes   = dry_season_90_magnitudes,
              dry_season_50_magnitudes   = dry_season_50_magnitudes,
              dry_season_10_magnitudes   = dry_season_10_magnitudes,
              dry_season_flush_durations = dry_season_flush_durations,
              dry_season_wet_durations   = dry_season_wet_durations,
              dry_season_no_flow_counts  = dry_season_no_flow_counts))
}

# =======================================
# Calculate the start of dry season
# =======================================

#' Start of dry season
#' @description
#' Computes the start of the dry season for each water year.
#'
#' @export
#' @param flow_matrix  A 'by date' row data frame with 2 columns named (Date, Flow).
#' @param class_number A remnant from Python implementation. Don't use.
#' @param max_nan_allowed_per_year  Maximum number of NA days per-year allowed.
#' @param sigma                    Scalar to set amount of smoothing.
#' @param sensitivity              Increased sensitivity returns smaller threshold for derivative.
#' @param peak_sensitivity         Identifies last major peak after which to search for start date.
#' @param max_peak_flow_date       Max search date for the final peak flow date of each water year.
#' @param min_dry_season_flow_percent Percentile of flow taken to be the threshold for dry-season start. Percentile is computed over
#'                                    flow values from the final peak to the end of the water-year.
#' @param min_flow_rate Don't calculate flow metrics if max flow is below this value.
#' @param do_plot Should diagnostic plots be generated? 1 = yes, 0 = no.
#'
#' @return A data frame of size 1 by no. water years with the dry-season start date for each
#'         water year.
calcStartOfdry_season <- function(flow_matrix,
                                  class_number,
                                  max_nan_allowed_per_year = 100,
                                  sigma = 5,
                                  sensitivity = 800,
                                  peak_sensitivity = 0.2,
                                  max_peak_flow_date = 250,
                                  min_dry_season_flow_percent = 0.125,
                                  min_flow_rate = 1,
                                  do_plot = 1)
{
  print("Hello")
  print(peak_sensitivity)
  print(min_dry_season_flow_percent)
  print(max_peak_flow_date)

  # This is what we want to fill up. The start dates of each dry season
  wtr_yrs     <- unique(flow_matrix$Water_Year)
  start_dates <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  colnames(start_dates) <- wtr_yrs
  for (wtryr in seq(1, length(wtr_yrs)))
  {
    print(paste0("Calculating start of dry season for water year ", wtr_yrs[wtryr]))
    flow_matrix_wy <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr])

    # Check if data has too many zeros or NaN, and if so skip to next water year
    if ( (dim(flow_matrix_wy)[1] < (365.25 - max_nan_allowed_per_year) )|
         (sum(is.na(flow_matrix_wy$Flow)) > max_nan_allowed_per_year) |
         (max(flow_matrix_wy$Flow) < min_flow_rate) )
    {
      print(paste0("WARNING: For water year ", wtr_yrs[wtryr], " not enough days or too low flow"))
      start_dates[wtryr] <- NA
      next
    }

    # Append each column with 30 more days from WY to this water year to help with smooth
    # Except the last year or if there are gaps in the years being analysed
    if ( (wtryr != length(wtr_yrs)) &
         (as.numeric(wtr_yrs[wtryr + 1]) - as.numeric(wtr_yrs[wtryr]) == 1) )
    {
      flow_matrix_wy_pls_one <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr + 1])
      flow_matrix_wy         <- rbind(flow_matrix_wy, flow_matrix_wy_pls_one[1:30, ])
    } else {
      flow_matrix_wy <- rbind(flow_matrix_wy, flow_matrix_wy[(dim(flow_matrix_wy)[1] - 30):dim(flow_matrix_wy)[1], ])
    }

    # Initial plot
    if (do_plot == 1)
    {
      plot(seq(1, length(flow_matrix_wy$Flow)), flow_matrix_wy$Flow,
           type = "l", lwd = 2, main = paste0("Start dry season ", wtr_yrs[wtryr]),
           xlab = 'Water year day + 30 days of next year',
           ylab = 'Flow (ML/d)')
    }
    # Replace any NaNs with previous day's flow. ****** MAKE NAN a USER PROBLEM REPLACE BEFORE WITH PREVIOUS DAY ******
    # flow_data = replace_nan(flow_data)

    # Set specific parameters for rain-dominated classes"""
    if (class_number == 4 |
        class_number == 6 |
        class_number == 7 |
        class_number == 8)
    {
      sensitivity <- 1100
      peak_sensitivity <- 0.1
      sigma <- 4
    }

    # Smooth out the time series using gaussian process
    flow_data   <- flow_matrix_wy$Flow
    x_axis      <- seq(1, length(flow_data))
    smooth_data <- sm::sm.regression(x_axis, flow_data, sigma, display = "none",
                                     eval.points = seq(1, length(x_axis)))$model.y
    if (do_plot == 1)
    {
      graphics::lines(x_axis, smooth_data, col = "blue", lwd = 2)
    }

    # Find spline fit equation for smoothed time series, and find derivative of spline
    spl_fit     <- stats::splinefun(x_axis, smooth_data, method = "natural")
    spl         <- spl_fit(x_axis, deriv = 0)
    spl_first   <- spl_fit(x_axis, deriv = 1)

    # Find the peak of the data
    max_flow_data  <- max(smooth_data, na.rm = T)
    max_flow_index <- which(smooth_data == max_flow_data)

    # Find the major peaks of the filtered data
    mean_flow <- mean(flow_data, na.rm = T)
    maxarray  <- peakDet(smooth_data, mean_flow * peak_sensitivity)$maxtab

    # Plot of the peak detection
    if (do_plot == 1)
    {
      graphics::points(maxarray[, 1],
             maxarray[, 2],
             col="Red", pch = 19, cex = 1.25)
      graphics::lines(x_axis, spl, col = "yellow", lwd = 2)
    }
    #maxarray, minarray = peakdet(smooth_data, mean_flow * peak_sensitivity)

    # Set search range after last smoothed peak flow

    for (index in rev(maxarray[, 1]))
    {
      if (index < max_peak_flow_date)
      {
        max_flow_index <- index
        break
      }
    }

    # Set a magnitude threshold below which start of dry_season can begin

    min_flow_data <- min(flow_data[max_flow_index:length(flow_data)])
    #threshold     <- as.numeric(quantile(flow_data[max_flow_index:length(flow_data)], probs = min_dry_season_flow_percent))
    threshold     <- as.numeric(stats::quantile(flow_data[max_flow_index:366], probs = min_dry_season_flow_percent))
    #threshold     <- as.numeric(flow_data[max_flow_index:length(flow_data), probs = min_dry_season_flow_percent))
    # This next if statement is for when you get weird low flows at start of year
    # if (threshold > min(smooth_data[250:length(smooth_data)]))
    # {
    #   threshold <-  min(smooth_data[250:length(smooth_data)]) * 1.05
    # }
    #threshold    <- min_flow_data + (smooth_data[max_flow_index] -  min_flow_data) * 0.05
    # threshold   <- min_flow_data + (smooth_data[max_flow_index]  - as.numeric(quantile(flow_matrix$Flow, probs = 0.05, na.rm = T))) * min_dry_season_flow_percent
    if (do_plot == 1)
    {
      graphics::abline(h = threshold)
    }

    current_sensitivity <- 1 / sensitivity
    for (flow_index in seq(1, length(smooth_data)))
    {
      # This ensures that the start date is for this water year. Not sure really make
      # that much sense as it's continuous.
      # if (flow_index == length(smooth_data) - 30 + 1)
      # {
      #   start_dates[wtryr] <- NA
      #   break
      # }
      # Search criteria: derivative is under rate of change threshold,
      #                  date is after last major peak,
      #                  and flow is less than specified percent of smoothed max flow
      #print(c(flow_index, abs(spl_first[flow_index]), max_flow_data * current_sensitivity, flow_data[flow_index], threshold))
      if ((abs(spl_first[flow_index]) < max_flow_data * current_sensitivity) &
          (flow_index > max_flow_index) &
          (flow_data[flow_index] <= threshold))
      {
        #print("Tick yes")
        start_dates[wtryr] <- flow_index
        break
      }
    }
    if (do_plot == 1 & !is.na(start_dates[wtryr]))
    {
      graphics::abline(v = as.numeric(start_dates[wtryr]))
      lvs <- seq(as.numeric(start_dates[wtryr] - 20), as.numeric(start_dates[wtryr] + 20))
      mv  <- max(abs(lvs * -(max_flow_data * current_sensitivity)))
      graphics::lines(lvs, threshold + mv + lvs * -(max_flow_data * current_sensitivity), col = 'red' ) #-
               #  (as.numeric(start_dates[wtryr]) * (max_flow_data * current_sensitivity)) )
    }

  }
  # if (length(which(is.na(start_dates))))
  # {
  #   start_dates[which(is.na(start_dates))] <- 335
  # }
  return(start_dates)
}
