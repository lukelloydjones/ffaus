# =====================================================
# Spring transition parameters and functions
# Author (that's a strong word): Luke Lloyd-Jones
# Date started: 26/02/2021
# Date updated: 31/05/2021
# =====================================================
# Updates log
#

#' Flow recession timing and magnitude.
#' @description
#' Summarises the initiation and magnitude of the flow recession,
#' which corresponds to the final peak and decay of the hydrograph
#' to the dry season.
#'
#' @export
#' @param flow_matrix a 'by date' row data frame with 2 columns named (Date, Flow).
#' @param dry_season_start_dates the water year date for each water year in which the dry-season is
#'                               computed to have begun. Computed using calcStartOfdry_season().
#' @param class_number a remnant from Python implementation. Don't use.
#' @param max_nan_allowed_per_year maximum number of NA days per-year allowed. Recommend imputing NAs prior to running ffaus functions.
#' @param max_peak_flow_date maximum search date for the final peak before flow recedes.
#' @param search_window_left the number of search window days to the left of the peak before flow recedes.
#' @param search_window_right the number of search window days to the right of the peak before flow recedes.
#' @param peak_sensitivity the sensitivity used for peak detection algorithm. Smaller values leads to more peaks detected.
#' @param peak_filter_percentage the proportion of the flow range (Qmax - Qmin) that the final peak must exceed to be considered the final peak.
#' @param min_max_flow_rate If filtered max flow is below this, automatically set recession timing to date of max flow.
#' @param window_sigma heavy standard deviation for Gaussian kernel smoothing to identify major peaks in entire water year.
#' @param fit_sigma smaller standard deviation  to identify small peaks in windowed data.
#' @param sensitivity controls the threshold as a proportion of maximum flow for first order derivative threshold. 0.1 - 10, 10 being the most sensitive.
#' @param min_percentage_of_max_flow  the detected date's flow in the window around the peak has be certain percentage of the max flow in the window.
#' @param timing_cutoff earliest accepted date at which recession can begin.
#' @param min_flow_rate minimum allowable magnitude threshold for fall flush flow
#' @param do_plot Should diagnostic plots be generated? 1 = yes, 0 = no.
#'
#' @return A list with two data frame elements of size 1 by no. water years
#' * timings - the start date of the flow recession.
#' * magnitudes - the magnitude of flow at recession start date.
calcRecessionTimingMagnitude <- function(flow_matrix,
                                         dry_season_start_dates,
                                         class_number = 3,
                                         max_nan_allowed_per_year   = 100,
                                         max_peak_flow_date         = 300,  # max search date for the peak flow date
                                         search_window_left         = 20,   # left side of search window set around max peak
                                         search_window_right        = 50,   # right side of search window set around max peak
                                         peak_sensitivity           = 0.1,  # smaller':> more peaks detection
                                         peak_filter_percentage     = 0.5,  # Relative flow (Q-Qmin) of start of spring must be certain percentage of peak relative flow (Qmax-Qmin)
                                         min_max_flow_rate          = 0.1,  # If filtered max flow is below this, automatically set spring timing to max flow
                                         window_sigma               = 10,   # Heavy filter to identify major peaks in entire water year
                                         fit_sigma                  = 1.3,  # Smaller filter to identify small peaks in windowed data (smaller sigma val => less filter)
                                         sensitivity                = 0.2,  # 0.1 - 10, 10 being the most sensitive
                                         min_percentage_of_max_flow = 0.5,  # the detected date's flow has be certain percentage of the max flow in that region
                                         timing_cutoff              = 80,  # Earliest accepted date for autumn timing, in Julian Date counting from Jul 1st = 0 (i.e. February 15 = 229)
                                         min_flow_rate              = 1,
                                         do_plot = 1)
{

  wtr_yrs     <- unique(flow_matrix$Water_Year)
  timings     <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  magnitudes  <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))

  colnames(timings)    <- wtr_yrs
  colnames(magnitudes) <- wtr_yrs

  for (wtryr in seq(1, length(wtr_yrs)))
  {
    print(paste0("Calculating recession timings and durations for water year ", wtr_yrs[wtryr]))
    flow_matrix_wy <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr])

    # Check if data has too many zeros or NaN, and if so skip to next water year
    if ( (dim(flow_matrix_wy)[1] < (365.25 - max_nan_allowed_per_year) ) |
         (sum(is.na(flow_matrix_wy$Flow)) > max_nan_allowed_per_year) |
         (max(flow_matrix_wy$Flow) < min_flow_rate) )
    {
      next
    }

    # Get flow data and interpolate between NA values"""
    flow_data <- flow_matrix_wy$Flow

    # Extract for use in optional plotting
    x_axis <- seq(1, length(flow_data))

    current_sensitivity <- sensitivity / 1000

    if (class_number == 4)
    {
      # Use specialized smoothing sigma values for rain-dominated classes
      window_sigma <- 2.5
      # Use specialized relative peak magnitude requirements for rain-dominated classes
      peak_filter_percentage <- 0.10
      min_percentage_of_max_flow <- 0.05
      # Reduce sensitivity in rain-dominated gauges
      max_peak_flow_date <- 255
    }
    if (class_number == 6)
    {
      window_sigma <- 2.5
      peak_filter_percentage <- 0.12
      min_percentage_of_max_flow <- 0.12
      max_peak_flow_date <- 255
    }
    if (class_number == 7)
    {
      window_sigma <- 2
      peak_filter_percentage <- 0.05
      min_percentage_of_max_flow <- 0.05
      max_peak_flow_date <- 255
    }
    if (class_number == 8)
    {
      window_sigma <- 2.5
      peak_filter_percentage <- 0.15
      min_percentage_of_max_flow <- 0.15
      max_peak_flow_date <- 255
    }

    # Using Gaussian with heavy sigma to smooth the curve
    filter_data <- sm::sm.regression(x_axis, flow_data, window_sigma, eval.points = seq(1, length(x_axis)), display = "none")$model.y

    # ---------------
    # TOOLS
    # ---------------
    # sm.regression(x_axis, flow_data, broad_sigma, display = "none", eval.points = seq(1, length(x_axis)))$model.y
    # spl_fit     <- splinefun(x_axis, filter_data, method = "natural")
    # spl         <- spl_fit(x_axis, deriv = 0)
    # spl_first   <- spl_fit(x_axis, deriv = 1)
    # mean_flow = mean(filter_data, na.rm = T)
    # maxarray  = peakDet(spl, mean_flow * peak_sensitivity)$maxtab


    # Find the peaks and valleys of the filtered data
    mean_flow <- mean(filter_data, na.rm = T)
    # Returns array with the index and flow magnitude for each peak and valley
    maxarray <- peakDet(filter_data, mean_flow * peak_sensitivity)$maxtab
    if (dim(maxarray)[1] == 0)
    {
      maxarray <- peakDet(filter_data, mean_flow * 0.01)$maxtab
    }
    minarray <- peakDet(filter_data, mean_flow * peak_sensitivity)$mintab

    if (do_plot == 1)
    {
      plot(x_axis, flow_data,
           type = "l", lwd = 2, main = paste0("Flow recession ", wtr_yrs[wtryr]),
           xlab = 'Water year day',
           ylab = 'Flow (ML/d)')
      graphics::lines(x_axis, filter_data, col = "red", lwd = 2)
      graphics::points(maxarray[, 1], maxarray[, 2], col = "green", pch = 16)
      graphics::points(minarray[, 1], minarray[, 2], col = "blue", pch = 16)
    }


    # Find the max flow in the curve and determine flow range requirements"""
    # Let's not look for the maximum flow in the dry season. Minimum search date is 100
    min_max_flow_day <- timing_cutoff
    fd_lngth         <- length(filter_data)
    max_flow         <- max(filter_data[min_max_flow_day:max_peak_flow_date], na.rm = T)
    if (!is.na(dry_season_start_dates[wtryr]))
    {
      max_flow_index   <- maxarray[max(which(maxarray[, 1] < dry_season_start_dates[wtryr])), 1]
    } else {
      max_flow_index   <- maxarray[max(which(maxarray[, 1] < max_peak_flow_date)), 1]
    }
    min_flow         <- min(filter_data[min_max_flow_day:max_peak_flow_date], na.rm = T)
    flow_range       <- max_flow - min_flow

    # Identify rightmost peak that fulfills date and magnitude requirements
    if (!is.na(dry_season_start_dates[wtryr]))
    {
      for (index in rev(seq(1, dim(maxarray)[1])))
      {
        if ( (maxarray[index, 1] < max_peak_flow_date) &
             (maxarray[index, 1] >= min_max_flow_day)  &
             (((maxarray[index, 2] - min_flow) / flow_range) > peak_filter_percentage) &
             (maxarray[index, 1] < dry_season_start_dates[wtryr]) )
        {
          max_flow_index <- maxarray[index, 1]
          break
        }
      }
    }

    if (do_plot == 1)
    {
      graphics::abline(v=max_flow_index)
    }

    if (max_flow < min_max_flow_rate)
    {
      # Set start of spring index to the max flow index, when the annual max flow is below certain threshold.
      #         This is used for extremely low flows where seasonal timings are harder to find
      max_flow_data     <- max(flow_data, na.rm = T)
      timings[wtryr]    <- which(flow_data == max_flow_data)
      magnitudes[wtryr] <- max_flow_data
    } else
    {

      if (max_flow_index < search_window_left)
      {

        current_search_window_left <- 0

      } else
      {
        current_search_window_left <- search_window_left

      }

      if (max_flow_index > 366 - search_window_right)
      {
        current_search_window_right <- 366 - max_flow_index

      } else {

        current_search_window_right <- search_window_right

      }

      # Get indices of windowed data
      windw_inds            <- (max_flow_index - current_search_window_left):(max_flow_index + current_search_window_right)
      flow_data_windw       <- flow_data[windw_inds]
      max_flow_index_window <- max(flow_data_windw, na.rm = T)
      timings[wtryr]        <- windw_inds[max(which(flow_data_windw == max_flow_index_window))]
      magnitudes[wtryr]     <- max_flow_index_window

      # Gaussian filter again on the windowed data (smaller filter this time)
      x_axis_windw     <- windw_inds
      flow_data_window <- sm::sm.regression(x_axis_windw, flow_data_windw, fit_sigma, display = "none", eval.points = x_axis_windw)$model.y
      if (do_plot == 1)
      {
        graphics::lines(x_axis_windw, flow_data_window, col = "purple", lwd = 4)
      }

      # If search window is too small, move on to next value in maxarray.
      # If it is the last value in maxarray, proceed inside loop
      if (length(flow_data_window) < 50 & length(maxarray) != 1)
      {
        next
      }

      # spl_fit     <- splinefun(x_axis, filter_data, method = "natural")
      # spl         <- spl_fit(x_axis, deriv = 0)
      # spl_first   <- spl_fit(x_axis, deriv = 1)
      # Fit a spline on top of the Gaussian curve
      spl_fit <- stats::splinefun(x_axis_windw, flow_data_window, method = "natural")
      spl     <- spl_fit(x_axis_windw, deriv = 0)
      #plot(flow_matrix_wy$Date[x_axis_windw], spl, type = "l", col = "red")
      #points(flow_matrix_wy$Date[x_axis_windw[index_zeros]], spl[index_zeros], col = "blue")
      # Calculate the first derivative of the spline
      spl_first <- spl_fit(x_axis_windw, deriv = 1)

      #plot(flow_matrix_wy$Date[x_axis_windw], spl_first, type = "l", col = "red")
      #points(flow_matrix_wy$Date[x_axis_windw[index_zeros]], spl_first[index_zeros], col = "blue")
      #abline(v=flow_matrix_wy$Date[149])
      # Find where the derivative of the spline crosses zero
      index_zeros <- x_axis_windw[which(diff(sign(spl_first)) != 0)]

      if (do_plot == 1)
      {
        graphics::lines(x_axis_windw, spl, col = "yellow", lwd = 4)
        graphics::points(index_zeros, spl_fit(index_zeros), col = 'red')
      }

      # Loop through the indices where derivative=0, from right to left
      for (nw_ind in rev(index_zeros))
      {
        #print(nw_ind)
        threshold       <- max(spl_first)
        max_flow_window <- max(spl)
        min_flow_window <- min(spl)
        range_window    <- max_flow_window - min_flow_window

        # Set timing as index which fulfills the following requirements
        if ( !is.na(dry_season_start_dates[wtryr]) &
             (nw_ind < dry_season_start_dates[wtryr]) &
             (nw_ind > timing_cutoff) &
             ((spl_fit(nw_ind)     - spl_fit(nw_ind - 1)) > threshold * current_sensitivity * 1) &
             ((spl_fit(nw_ind - 1) - spl_fit(nw_ind - 2)) > threshold * current_sensitivity * 2) &
             ((spl_fit(nw_ind - 2) - spl_fit(nw_ind - 3)) > threshold * current_sensitivity * 3) &
             ((spl_fit(nw_ind - 3) - spl_fit(nw_ind - 4)) > threshold * current_sensitivity * 4) &
             (((spl_fit(nw_ind) - min_flow_window) / range_window) > min_percentage_of_max_flow) )
        {
          #print("yep")
          timings[wtryr] <- nw_ind
          break
        }
      }

      # Check if timings is after max flow index
      if (timings[wtryr] < max_flow_index)
      {
        timings[wtryr] <- max_flow_index
      }

      # Find max flow 4 days before and 7 days ahead. Assign as new start date"""
      fr_dy_sv_dy <- flow_data[(as.numeric(timings[wtryr]) - 4):(as.numeric(timings[wtryr]) + 7)]
      #plot(flow_matrix_wy$Date[(as.numeric(timings[wtryr]) - 4):(as.numeric(timings[wtryr]) + 7)],
      #     flow_matrix_wy$Flow[(as.numeric(timings[wtryr]) - 4):(as.numeric(timings[wtryr]) + 7)], type = "l")
      if (length(fr_dy_sv_dy) > 10)
      {
        max_flow_window_new <- max(fr_dy_sv_dy)
        timings[wtryr]      <- timings[wtryr] + (max(which(fr_dy_sv_dy == max_flow_window_new)) - 5) # Original timings is always the fifth element
        magnitudes[wtryr]   <- max_flow_window_new
      }

      if ( (!is.na(dry_season_start_dates[wtryr])) & (timings[wtryr] > dry_season_start_dates[wtryr]))
      {
        timings[wtryr] <- NA
      }
    }

    if (do_plot == 1)
    {
      graphics::abline(v = timings[wtryr], col = "brown")
      graphics::abline(v = dry_season_start_dates[wtryr], col = "red")
    }
  }

  #_spring_transition_plotter(x_axis, flow_data, filter_data, x_axis_window, spl_first_deriv, new_index, max_flow_index, timings, current_search_window_left, current_search_window_right, spl, column_number, maxarray)
  return(list(timings = timings, magnitudes = magnitudes))
}

# ============================================
# Flow recession duration function
# ============================================

#' Flow recession duration.
#' @description
#' Summarises the number of water days for the flow
#' to recede from the final peak to the start of the dry season.
#'
#' @export
#' @param recession_timings vector of flow recession start dates for each water year.
#' @param dry_season_start_dates the water year date for each water year in which the dry-season is
#'                               computed to have begun. Computed using calcStartOfdry_season().
#'
#' @return A list with two data frame elements of size 1 by no. water years
#' * duration - the length of the flow recession in days for each water year.
calcRecessionDuration <- function(recession_timings, dry_season_start_dates)
{
  duration <- data.frame(matrix(NA, nrow = 1, ncol = length(recession_timings)))
  colnames(duration) <- colnames(recession_timings)
  for (wtryr in seq(1, length(recession_timings)))
  {
    if (!is.na(recession_timings[wtryr]) & !is.na(dry_season_start_dates[wtryr]) & (dry_season_start_dates[wtryr] > recession_timings[wtryr]))
    {
      duration[wtryr] <- dry_season_start_dates[wtryr] - recession_timings[wtryr]
    } else {
      duration[wtryr] <- NA
    }
  }
  return(list(duration = duration))
}

# ============================================
# Flow recession rate of change (ROC) function
# ============================================

#' Flow recession duration.
#' @description
#' Summarises the number of water days for the flow
#' to recede from the final peak to the start of the dry season.
#'
#' @export
#' @param flow_matrix a 'by date' row data frame with 2 columns named (Date, Flow).
#' @param recession_timings vector of flow recession start dates for each water year.
#' @param dry_season_start_dates the water year date for each water year in which the dry-season is
#'                               computed to have begun. Computed using calcStartOfdry_season().
#'
#' @return A list with two data frame elements of size 1 by no. water years
#' * rocs_only_neg - the median over flow recession days of the recession slope as a percentage change from previous day.
calcRecessionRoc <- function(flow_matrix, recession_timings, dry_season_start_dates)
{
  # Three methods to calculate rate of change
  # 1. median of daily rate of change
  # 2. median of daily rate of change only for negative changes
  # 3. start - end / days

  rocs <- data.frame(matrix(NA, nrow = 1, ncol = length(recession_timings)))
  colnames(rocs) <- colnames(recession_timings)

  rocs_start_end <- data.frame(matrix(NA, nrow = 1, ncol = length(recession_timings)))
  colnames(rocs_start_end) <- colnames(recession_timings)

  rocs_only_neg <- data.frame(matrix(NA, nrow = 1, ncol = length(recession_timings)))
  colnames(rocs_only_neg) <- colnames(recession_timings)

  wtr_yrs <- colnames(recession_timings)
  for (wtryr in seq(1, length(wtr_yrs)))
  {
    recession_timing <- recession_timings[wtryr]
    dry_season_start_date <- dry_season_start_dates[wtryr]

    rate_of_change           <- c()
    rate_of_change_neg       <- c()
    rate_of_change_start_end <- NA

    flow_matrix_wy <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr])
    if (wtryr < length(wtr_yrs))
    {
      flow_matrix_wy_pls_one <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr + 1])
    }

    if (!is.na(recession_timing) &
        !is.na(dry_season_start_date) &
        (dry_season_start_date > recession_timing))
    {
      if ( (wtryr == length(wtr_yrs)) |
           (as.numeric(wtr_yrs[wtryr + 1]) - as.numeric(wtr_yrs[wtryr]) > 1) )
      {
        raw_flow <- c(flow_matrix_wy$Flow, flow_matrix_wy$Flow[1:30])
      } else {
        raw_flow <- c(flow_matrix_wy$Flow, flow_matrix_wy_pls_one$Flow[1:30])
      }

      flow_data <- raw_flow[as.numeric(recession_timing):as.numeric(dry_season_start_date)]
      rate_of_change_start_end <- (flow_data[length(flow_data)] - flow_data[1]) / flow_data[1]

      for (flow_index in seq(1, length(flow_data)))
      {
        if (flow_index == length(flow_data))
        {
          next
        }
        else if (flow_data[flow_index + 1] < flow_data[flow_index])
        {
          rate_of_change     <- c(rate_of_change,     (flow_data[flow_index] - flow_data[flow_index + 1]) / flow_data[flow_index])
          rate_of_change_neg <- c(rate_of_change_neg, (flow_data[flow_index] - flow_data[flow_index + 1]) / flow_data[flow_index])
        } else {
          rate_of_change     <- c(rate_of_change,     (flow_data[flow_index] - flow_data[flow_index + 1]) / flow_data[flow_index])
        }
      }
    } else
    {
      rocs[wtryr]           <- NA
      rocs_start_end[wtryr] <- NA
      rocs_only_neg[wtryr]  <- NA
      next
    }

    rocs[wtryr]            <- stats::median(rate_of_change, na.rm = T)
    rocs_start_end[wtryr]  <- rate_of_change_start_end
    rocs_only_neg[wtryr]   <- stats::median(rate_of_change_neg, na.rm = T)
  }
  return(list(rocs_only_neg = rocs_only_neg))
}
