# =====================================================
# Flow initiation (typically winter/spring flush timings
# and durations.
# Author (that's a strong word): Luke Lloyd-Jones
# Date started: 26/02/2021
# Date updated: 31/05/2021
# =====================================================
# Updates log
#
# What's in here
# -

# --------------------------------
# Fall flush timings and durations
# --------------------------------

#' Initiation flow timing and duration.
#' @description
#' Summarises the first return of water to the hydrograph following the dry season
#' for each water year.
#'
#' @export
#' @param flow_matrix a 'by date' row data frame with 2 columns named (Date, Flow).
#' @param class_number a remnant from Python implementation. Don't use.
#' @param max_nan_allowed_per_year maximum number of NA days per-year allowed. Recommend imputing NAs prior to running ffaus functions.
#' @param dry_season_start_dates the water year date for each water year in which the dry-season is
#'                               computed to have begun. Computed using calcStartOfdry_season().
#' @param min_flow_rate  Don't calculate flow metrics if max flow is below this value.
#' @param sigma  Standard deviation for Gaussian kernel smoothing of within year hydrograph used for filtering.
#' @param broad_sigma Larger standard deviation filter to provide heavy smooth of within year hydrograph to find wet season peak
#' @param wet_season_sigma Medium standard deviation filter to provide intermediate smooth of within year hydrograph to find wet season initiation peak
#' @param peak_sensitivity Sensitivity of peak detection algorithm.
#' @param peak_sensitivity_wet Sensitivity of peak detection algorithm for detecting peaks of wet-season smoothed data.
#' @param max_flush_duration Maximum duration in days from start to end, for initiation/flush episode.
#' @param wet_threshold_perc Return to wet season flow must be 'this' percentage of that year's max flow.
#' @param peak_detect_perc The peak percentage of max flow identified to search before for wet season initiation.
#' @param flush_threshold_perc Flow at half duration of peak must be 'this' proportion of the maximum.
#' @param min_flush_threshold Minimum allowable magnitude threshold for initiation flow
#' @param last_date_cutoff_flush Latest accepted date for winter spring flush, in Julian Date counting from Jul 1st = 1. (i.e., 2017-11-30)
#' @param slope_sensitivity Sets sensitivity of slope threshold for wet season start time.
#' @param do_plot Should diagnostic plots be generated? 1 = yes, 0 = no.
#'
#' @return A list with data frame elements of size 1 by no. water years
#' * start_dates - flow initiation start dates.
#' * mags - the peak magnitude of flow associated with the flow initiation start dates.
#' * wet_dates - the date in which the wet season is deemed to have started for that water year.
#' * durations - the duration of the flow initiation peak, which is typically the no. of days to from the left minimum of the peak to the peak.
calcFlowInitiationFlushTimingsDurations <- function(flow_matrix,
                                                    class_number = 3,
                                                    max_nan_allowed_per_year  = 100,
                                                    dry_season_start_dates,
                                                    min_flow_rate             = 1,
                                                    sigma                     = 2,
                                                    broad_sigma               = 15,
                                                    wet_season_sigma          = 12,
                                                    peak_sensitivity          = 0.005,
                                                    peak_sensitivity_wet      = 0.005,
                                                    max_flush_duration        = 40,
                                                    wet_threshold_perc        = 0.2,
                                                    peak_detect_perc          = 0.30,
                                                    flush_threshold_perc      = 0.30,
                                                    min_flush_threshold       = 1,
                                                    last_date_cutoff_flush    = 152,
                                                    slope_sensitivity         = 300,
                                                    do_plot = 1)
{


  wtr_yrs       <- unique(flow_matrix$Water_Year)
  start_dates   <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  wet_dates     <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  durations     <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  durations_max <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  mags          <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  mags_max      <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))

  colnames(start_dates)   <- wtr_yrs
  colnames(wet_dates)     <- wtr_yrs
  colnames(durations)     <- wtr_yrs
  colnames(durations_max) <- wtr_yrs
  colnames(mags)          <- wtr_yrs
  colnames(mags_max)      <- wtr_yrs

  for (wtryr in seq(1, length(wtr_yrs)))
  {
    print(paste0("Calculating flow initiation flush timings and durations for water year ", wtr_yrs[wtryr]))
    flow_matrix_wy <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr])

    # Check if data has too many zeros or NaN, and if so skip to next water year
    if ( (dim(flow_matrix_wy)[1] < (365.25 - max_nan_allowed_per_year) ) |
         (sum(is.na(flow_matrix_wy$Flow)) > max_nan_allowed_per_year) |
         (max(flow_matrix_wy$Flow) < min_flow_rate) )
    {
      next
    }


    # Get flow data
    flow_data <- flow_matrix_wy$Flow
    x_axis    <- flow_matrix_wy$Water_Year_Day
    days_frow_lst_year <- 0 # Initialise to 0


    # Add some extra data to the start to help with edges of smooth and to see
    # if pulse has ramped up earlier
    if (wtryr != 1)
    {
      if ((as.numeric(wtr_yrs[wtryr]) - as.numeric(wtr_yrs[wtryr - 1]) == 1) & !is.na(dry_season_start_dates[1, (wtryr - 1)]))
      {
        flow_matrix_wy_mns_one <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr - 1])
        #days_frow_lst_year     <- 30 # Just take 30 as the dry season can start pretty earlier and one year's dry season is another's wet
        if (dry_season_start_dates[1, (wtryr - 1)] > length(flow_matrix_wy_mns_one$Flow))
        {
          flow_prt_mns_one <- c()
        } else {
          flow_prt_mns_one       <- flow_matrix_wy_mns_one$Flow[dry_season_start_dates[1, (wtryr - 1)]:length(flow_matrix_wy_mns_one$Flow)]
        }
        flow_data <- c(flow_prt_mns_one, flow_data)
        x_axis    <- seq(1, length(flow_data))
        days_frow_lst_year <- length(flow_prt_mns_one)
      } else if ((as.numeric(wtr_yrs[wtryr]) - as.numeric(wtr_yrs[wtryr - 1]) == 1) & !is.na(dry_season_start_dates[1, (wtryr - 1)]))
      {
        flow_matrix_wy_mns_one <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr - 1])
        #days_frow_lst_year     <- 30 # Just take 30 as the dry season can start pretty earlier and one year's dry season is another's wet
        flow_prt_mns_one       <- flow_matrix_wy_mns_one$Flow[335:length(flow_matrix_wy_mns_one$Flow)]
        flow_data <- c(flow_prt_mns_one, flow_data)
        x_axis    <- seq(1, length(flow_data))
        days_frow_lst_year <- length(flow_prt_mns_one)
      }
    }

    # Interpolate between None values # BACK ON THE USER
    # flow_data = replace_nan(flow_data)

    # Return to Wet Season

    if (class_number == 3 |
        class_number == 4 |
        class_number == 5 |
        class_number == 6 |
        class_number == 7 |
        class_number == 8)
    {
      wet_season_filter_data <- sm::sm.regression(x_axis, flow_data, 4, display = "none", eval.points = seq(1, length(x_axis)))$model.y
    } else {
      wet_season_filter_data <- sm::sm.regression(x_axis, flow_data, wet_season_sigma, display = "none", eval.points = seq(1, length(x_axis)))$model.y
    }

    if (!all(wet_season_filter_data > 0))
    {
      print("Some negative values in wet_season_filter_data filter data")
      wet_season_filter_data[wet_season_filter_data <= 0] <- min(flow_data)
    }

    if (class_number == 1 |
        class_number == 2 |
        class_number == 9)
    {
      slope_detection_data <- sm::sm.regression(x_axis, flow_data, 7, display = "none", eval.points = seq(1, length(x_axis)))$model.y
    } else if (class_number == 3 |
               class_number == 4 |
               class_number == 5 |
               class_number == 6 |
               class_number == 7 |
               class_number == 8)
    {
      slope_detection_data <- sm::sm.regression(x_axis,  flow_data, 2, display = "none", eval.points = seq(1, length(x_axis)))$model.y
    } else
    {
      slope_detection_data <- sm::sm.regression(x_axis, flow_data, 4, display = "none", eval.points = seq(1, length(x_axis)))$model.y
    }

    if (!all(slope_detection_data > 0))
    {
      print("Some negative values in slope_detection_data data")
      slope_detection_data[slope_detection_data <= 0] <- min(flow_data)
    }

    broad_filter_data      <- sm::sm.regression(x_axis, flow_data, broad_sigma, positive = T, display = "none", eval.points = seq(1, length(x_axis)))$model.y

    if (!all(broad_filter_data > 0))
    {
      print("Some negative values in broad filter data")
      broad_filter_data[broad_filter_data <= 0] <- min(flow_data)
    }

    # Initial plot
    if (do_plot == 1)
    {
      #par(mfrow=c(1, 2))
      plot(x_axis, flow_data,
           type = "l", lwd = 2, main = paste0("Flow initiation pulse ", wtr_yrs[wtryr]),
           xlab = 'Water year day + dry season of prior year',
           ylab = 'Flow (ML/d)')
      graphics::lines(x_axis, broad_filter_data, col = "blue", lwd = 2)
      graphics::lines(x_axis, wet_season_filter_data, col = "red", lwd = 2)
      graphics::lines(x_axis, slope_detection_data, col = "green", lwd = 2)
      # plot(seq(1, length(flow_matrix_wy$Flow)), flow_matrix_wy$Flow,
      #      type = "l", lwd = 2, main = paste0("Flow initiation pulse ", wtr_yrs[wtryr]),
      #      xlab = 'Water year day',
      #      ylab = 'Flow (ML/d)')
    }

    last_wet_date <- dry_season_start_dates[1, wtryr] + days_frow_lst_year
    return_date   <- returnToWetDate(flow_data,
                                     wet_season_filter_data,
                                     broad_filter_data,
                                     slope_detection_data,
                                     wet_threshold_perc,
                                     peak_detect_perc,
                                     peak_sensitivity_wet,
                                     slope_sensitivity,
                                     last_wet_date)

    #print("Hi I'm here 2014")

    if (!is.na(return_date))
    {
      # Need to correct to the start of this water year as we
      # added some flow on from the start if last year's dry season.
      wet_dates[wtryr] <- return_date - days_frow_lst_year
    }



    #print(paste0("Wet dates now ", wet_dates[wtryr], " ", return_date))

    # Filter noise data with small sigma to find fall flush hump
    filter_data <-  sm::sm.regression(x_axis, flow_data, sigma, display = "none", eval.points = seq(1, length(x_axis)))$model.y
    if (sum(is.na(filter_data)) > 0)
    {
      filter_data[is.na(filter_data)] <- flow_data[is.na(filter_data)]
    }


    # Fit spline
    spl_fit     <- stats::splinefun(x_axis, filter_data, method = "natural")
    spl         <- spl_fit(x_axis, deriv = 0)
    spl_first   <- spl_fit(x_axis, deriv = 1)


    # Find the peaks and valleys of the filtered data
    mean_flow <- mean(filter_data, na.rm = T)
    maxarray  <- peakDet(spl, mean_flow * peak_sensitivity)$maxtab
    minarray  <- peakDet(spl, mean_flow * peak_sensitivity)$mintab

    if (do_plot == 1)
    {
      #plot(seq(1, length(flow_matrix_wy$Flow)), flow_matrix_wy$Flow,
      #     type = "l", lwd = 2, main = paste0("Flow initiation pulse ", wtr_yrs[wtryr]),
      #     xlab = 'Water year day',
      #     ylab = 'Flow (ML/d)')
      #lines(seq(1, length(wet_season_filter_data[-seq(1, days_frow_lst_year)])), wet_season_filter_data[-seq(1, days_frow_lst_year)], col = "red", lwd = 2)
      #lines(seq(1, length(filter_data[-seq(1, days_frow_lst_year)])), filter_data[-seq(1, days_frow_lst_year)], col = "green", lwd = 2)
      graphics::lines(x_axis, filter_data, col = "purple", lwd = 2)
      graphics::points(maxarray[, 1], maxarray[, 2], col = "red", pch = 16)
      graphics::points(minarray[, 1], minarray[, 2], col = "green", pch = 16)
      graphics::abline(v = return_date, col = 'blue')

    }

    # Find max and min of filtered flow data
    max_flow       <- max(filter_data)
    max_flow_index <- which(filter_data == max_flow)

    min_flow <- min(broad_filter_data[1:max_flow_index])

    # If could not find any max and find
    if ((dim(maxarray)[1] == 0) | (dim(minarray)[1] == 0)) # | (minarray[1, 1] > max_flow_index))
    {
      next
    }

    # Get flow magnitude threshold from previous winter's base flow
    if (wtryr == 1 & !is.na(wet_dates[wtryr]))
    {
      wet_date <- wet_dates[1, wtryr]
      baseflow <- flow_matrix_wy$Flow[1:wet_date]
      bs_med   <- as.numeric(stats::quantile(baseflow, probs = 0.50, na.rm = T))
    } else if (is.na(wet_dates[wtryr]))
    {
      baseflow <- flow_matrix_wy$Flow[as.numeric(dry_season_start_dates[wtryr]):length(flow_matrix_wy$Flow)]
      bs_med  <- as.numeric(stats::quantile(baseflow, probs = 0.50, na.rm = T))

    } else if (((as.numeric(wtr_yrs[wtryr]) - as.numeric(wtr_yrs[wtryr - 1]) != 1)) & !is.na(wet_dates[wtryr]))
    {
      wet_date <- wet_dates[1, wtryr]
      baseflow <- flow_matrix_wy$Flow[1:wet_date]
      bs_med   <- as.numeric(stats::quantile(baseflow, probs = 0.50, na.rm = T))
    } else {
      if (!is.na(wet_dates[wtryr]))
      {
        baseflow <- flow_data[1:as.numeric(return_date)]
        bs_med   <- as.numeric(stats::quantile(baseflow, probs = 0.50, na.rm = T))
      }
    }
    #print("Hi I'm here 2014 2")
    # Get initiation flow flush peak
    # Only test duration for first half of fall flush peak
    half_duration <- ceiling(max_flush_duration / 2)
    if (bs_med > 25)
    {
      # if median baseflow is large (>25), magnitude threshold is 50% above median baseflow of previous summer
      min_flush_magnitude <- bs_med * 1.5
    } else {
      # otherwise magnitude threshold is 100% above median baseflow of previous summer
      min_flush_magnitude <- bs_med * 2
    }

    if (min_flush_magnitude < min_flush_threshold)
    {
      min_flush_magnitude <- min_flush_threshold
    }

    # Reset and then update the date_cutoff to include the extra days added at start of year
    last_date_cutoff_flush_up <- last_date_cutoff_flush + days_frow_lst_year

    #print("Hi I'm here 2014 3")
    for (index in seq(1, length(maxarray[, 1])))
    {
      #print(index)
      #if (!is.na(wet_dates[wtryr]))
      #{
      # if (as.numeric(wet_dates[wtryr]) == 1)
      # {
      #       #print("Hi I'm here 2014 5")
      #       start_dates[wtryr] <- maxarray[1, 1]
      #       mags[wtryr]        <- maxarray[1, 2]
      #       break
      # } else
      if ( (maxarray[index, 1] <  half_duration) &
           (maxarray[index, 1] != 1) &
           (maxarray[index, 2] >  broad_filter_data[maxarray[index, 1]]) &
           (maxarray[index, 2] >  min_flush_magnitude) &
           (maxarray[index, 1] <= last_date_cutoff_flush_up) )
      {
        #print('yes 1')
        #if index found is before the half duration allowed"""
        start_dates[wtryr] <- maxarray[index, 1]
        mags[wtryr]        <- maxarray[index, 2]
        break
      } else if ( ( (((maxarray[index, 2] - spl_fit((maxarray[index, 1] - half_duration))) / maxarray[index, 2]) > flush_threshold_perc) |
                    (minarray[index, 1]  - maxarray[index, 1] < half_duration)) &
                  (maxarray[index, 2]    > broad_filter_data[maxarray[index, 1]]) &
                  (maxarray[index, 2]    > min_flush_magnitude) &
                  (maxarray[index, 1]    <= last_date_cutoff_flush_up) )
      {
        #print('yes 2')
        # If peak and valley is separated by half duration, or half duration to the left is less than 30% of its value"""
        start_dates[wtryr] <- maxarray[index, 1]
        mags[wtryr]        <- maxarray[index, 2]
        break

      } else if (index == dim(maxarray)[1] & maxarray[index, 1] <= last_date_cutoff_flush_up)
      {
        #print('yes 3')
        start_dates[wtryr] <- maxarray[index, 1]
        mags[wtryr]        <- maxarray[index, 2]
        break
      } else if ((maxarray[index, 1] > half_duration) &
                 ((spl_fit(maxarray[index, 1] - half_duration) - min_flow) / (maxarray[index, 2] - min_flow) < flush_threshold_perc) &
                 ((spl_fit(maxarray[index, 1] + half_duration) - min_flow) / (maxarray[index, 2] - min_flow) < flush_threshold_perc) &
                 (maxarray[index, 2] > broad_filter_data[maxarray[index, 1]]) &
                 (maxarray[index, 2] > min_flush_magnitude) &
                 (maxarray[index, 1] <= last_date_cutoff_flush_up) )
      {
        #print('yes 5')
        # Both sides of flow value at the peak + half duration index fall below flush_threshold_perc
        start_dates[wtryr] <- maxarray[index, 1]
        mags[wtryr]        <- maxarray[index, 2]
        break
      } else if  (index != 1)
      {
        if (((minarray[index, 1] - maxarray[index, 1]     < half_duration) |
             (maxarray[index, 1]  - minarray[index - 1, 1] < half_duration)) &
            (maxarray[index, 2]   > broad_filter_data[maxarray[index, 1]]) &
            (maxarray[index, 2]   > min_flush_magnitude) &
            (maxarray[index, 1]   <= last_date_cutoff_flush_up) )
        {
          #print('yes 4')
          # valley and peak are distanced by less than half dur from either side"""
          start_dates[wtryr] <- maxarray[index, 1]
          mags[wtryr]        <- maxarray[index, 2]
          break
        }
      }
    }

    # Check to see if last start_date falls behind the max_allowed_date
    #print(c(start_dates[wtryr], wet_dates[wtryr]))
    if (!is.na(wet_dates[wtryr]))
    {
      if ((is.na(start_dates[wtryr])))
      {
        #print("yes yes")
        start_dates[wtryr]   <- return_date
        mags[wtryr]          <- flow_data[as.numeric(return_date)]
      }
    }

    # Get duration of each fall flush
    if (!is.na(start_dates[wtryr]))
    {
      flush_durations  <- calcFlowInitiationFlushFlushDurations(filter_data, start_dates[wtryr])
      durations[wtryr] <- flush_durations$duration
    } else {
      durations[wtryr] <- NA
    }

    if (do_plot == 1 & !is.na(start_dates[wtryr]))
    {
      graphics::abline(v = start_dates[wtryr],    col = 'yellow')
      graphics::abline(v = flush_durations$left,  col = 'red')
      graphics::abline(v = flush_durations$right, col = 'red')
    }

    # Reset the start dates by the number of days added on at start
    start_dates[wtryr] <- start_dates[wtryr] - days_frow_lst_year

  }

  if (is.na(wet_dates[1]) & !is.na(start_dates[1])) # NA at start of wet dates is cause by high flows starting at beginning of hydrograph. Just set the wet dates to be the flush start dates
  {
    wet_dates[1] <- start_dates[1]
  }
  return(list(start_dates = start_dates, mags = mags, wet_dates = wet_dates,
              durations = durations))
}


#' Return to wet date.
#' @description
#' Calculate the return to wet date, which signals the return to a wetter period
#' of each water year following the dry season.
#'
#'
#' @export
#' @param flow_data Flow data for a particular water year. Typically a vector of size 1 by 365/366.
#' @param wet_season_filter_data Smoothed (larger standard deviation filter default = 12) flow data for a particular water year. Typically a vector of size 1 by 365/366.
#' @param broad_filter_data Smoothed (medium standard deviation filter default = 15) flow data for a particular water year. Typically a vector of size 1 by 365/366.
#' @param slope_detection_data Smoothed (small standard deviation filter default = 2) flow data for a particular water year. Typically a vector of size 1 by 365/366.
#' @param wet_threshold_perc Return to wet season flow must be 'this' percentage of that year's max flow.
#' @param peak_detect_perc The peak percentage of max flow identified to search before for wet season initiation.
#' @param peak_sensitivity_wet Sensitivity of peak detection algorithm for when detecting peaks of wet-season smoothed data.
#' @param slope_sensitivity Sets sensitivity of slope threshold for wet season start time.
#' @param last_wet_date the final date in which the wet season can begin. Usually take to be the dry-season start date.
#'
#' @return A numeric water year day corresponding to the start date of the wet season.
returnToWetDate <- function(flow_data,
                            wet_season_filter_data,
                            broad_filter_data,
                            slope_detection_data,
                            wet_threshold_perc,
                            peak_detect_perc,
                            peak_sensitivity_wet,
                            slope_sensitivity,
                            last_wet_date)
{
  max_wet_peak_mag   <- max(wet_season_filter_data, na.rm = T)
  max_wet_peak_index <- which(wet_season_filter_data == max_wet_peak_mag)
  # Set the maximum initial peak date to be 150 days of the water year
  max_intial_wet_peak_index <- 150

  if (length(wet_season_filter_data[max_wet_peak_index]) == 0)
  {
    return(NA)
  }

  if (max_wet_peak_index != 1)
  {
    min_wet_peak_mag <- min(wet_season_filter_data[1:max_wet_peak_index], na.rm = T)
  } else {
    min_wet_peak_mag <- min(wet_season_filter_data[max_wet_peak_index:100], na.rm = T)
  }
  maxarray_wet     <- peakDet(wet_season_filter_data, peak_sensitivity_wet)$maxtab

  # Get the derivative of smoothed data for rate of change requirement
  x_axis      <- seq(1, length(slope_detection_data))
  spl_fit     <- stats::splinefun(x_axis, slope_detection_data, method = "natural")
  #spl.fit.val <- spl.fit(x_axis, n = length(x_axis) + 1)
  spl_first   <- spl_fit(x_axis, deriv = 1)
  #lines(x_axis, slope_detection_data)

  # plot(x_axis, wet_season_filter_data,
  #      type = "l", lwd = 2, main = paste0("Flow initiation pulse ", wtr_yrs[wtryr]),
  #      xlab = 'Water year day + 30 days of next year',
  #      ylab = 'Flow (ML/d)')
  # lines(x_axis, broad_filter_data, col = "blue", lwd = 2)
  # lines(x_axis, slope_detection_data, col = "green")
  # #lines(x_axis, spl.fit.val)
  # points(maxarray_wet[, 1], maxarray_wet[, 2], col = "red")

  # Loop through peaks to find starting point of search
  search_index <- 0
  #maxarray_wet <- maxarray_wet[which(maxarray_wet[, 1] < max_intial_wet_peak_index), ]
  for (index in seq(1, dim(maxarray_wet)[1]))
  {
    print(index)
    if (dim(maxarray_wet)[1] == 1)
    {
      if (maxarray_wet[1, 1] == 1)
      {
        search_index <- max_wet_peak_index
        break
      } else {
        search_index <- maxarray_wet[1, 1]
        break
      }
    } else {
      if ( ((maxarray_wet[index, 2] - min_wet_peak_mag) / (max_wet_peak_mag - min_wet_peak_mag) > peak_detect_perc) &
           (maxarray_wet[index, 1] < last_wet_date) )
      {
        search_index <- maxarray_wet[index, 1]
        break
      }
    }
  }
  #abline(v=search_index)
  # Loop backwards from max flow index to beginning, to search for wet season

  if (search_index == 0)
  {
    return(NA)
  }

  for (index in seq(length(wet_season_filter_data[1:search_index]), 1))
  {
    value     <- wet_season_filter_data[index]
    #print(value)
    ratio_1   <- (value - min_wet_peak_mag) / (max_wet_peak_mag - min_wet_peak_mag)
    ratio_2   <- max_wet_peak_mag / slope_sensitivity
    deriv_val <- abs(spl_first[index])
    tru.fl    <- ((ratio_1 < wet_threshold_perc) & (deriv_val < ratio_2))
    #print(c(index, value, ratio_1, wet_threshold_perc, deriv_val, ratio_2, tru.fl))
    if (index == 1)
    {
      return_date <- index
    } else if ((ratio_1 < wet_threshold_perc) & (deriv_val < ratio_2))
    {
      # If value percentage falls below wet_threshold_perc
      return_date <- index
      break
      #return(return_date)
    }
  }
  return(return_date)
}


# -------------------------------------------------------------
# Winter-spring flush durations - helper function
# -------------------------------------------------------------


calcFlowInitiationFlushFlushDurations <- function(filter_data, # Smoothed flow data from smoothed calcFlowInitiationFlushTimingsDurations() using default sigma = 2
                                                  date) # Date of the water year in which peak of initiation flow has been detected.
{
  date <- as.numeric(date)
  # Left side sharp
  der_percent_threshold_left  <- 50  # Slope of rising limb (i.e. derivative) must be "sharp"
  flow_percent_threshold_left <- 80

  # Right side mellow
  der_percent_threshold_right  <- 30  # Slope of falling limb (i.e. derivative) has lower requirement to be part of flush duration
  flow_percent_threshold_right <- 80

  duration <- NA
  left  <- NA
  right <- NA

  if (!is.na(date) | (date == 1))
  {
    left_minarray  <- peakDet(filter_data[1:(date)], 0.01)$mintab
    right_minarray <- peakDet(filter_data[date:length(filter_data)], 0.01)$mintab

    if (dim(left_minarray)[1] < 1)
    {
      left <- 1
    } else {
      left <- left_minarray[dim(left_minarray)[1], 1]
    }

    if (dim(right_minarray)[1]  < 1)
    {
      right <- 1
    } else {
      right <- date + right_minarray[1][1]
    }

    if (date - left > 10)
    {
      # create spline, and find derivative"""
      x_axis_left    <- seq(1, length(filter_data[left:date]))
      spl_left       <- stats::splinefun(x_axis_left, filter_data[left:date], method = "natural")
      spl_first_left <- spl_left(x_axis_left, deriv = 1)

      # check if derivative value falls below certain threshold ######## THIS IS A PERCENTILE FUNCTION np.nanpercentile ##########
      spl_first_left_median <- stats::quantile(spl_first_left, probs = der_percent_threshold_left/100)

      # check if actual value falls below threshold, avoiding the rounded peak"""
      median_left <- stats::quantile(filter_data[left:date], probs = flow_percent_threshold_left/100)

      for (index_left in rev(seq(1, length(spl_first_left)))) # OG: enumerate(reversed(spl_first_left(x_axis_left))):
      {
        #
        # print(index_left)
        if ((spl_first_left[index_left] < spl_first_left_median) &
            (filter_data[date - index_left + 1] < median_left))
        {
          left <- date - index_left
          break
        }

      }
    }


    if (right - date > 10)
    {
      # create spline, and find derivative"""

      x_axis_right    <- seq(1, length(filter_data[date:right]))
      spl_right       <- stats::splinefun(x_axis_right, filter_data[date:right], method = "natural")
      spl_first_right <- spl_right(x_axis_right, deriv = 1)

      spl_first_right_median <- stats::quantile(spl_first_right, probs = der_percent_threshold_right/100)
      median_right <- stats::quantile(filter_data[date:right], probs = flow_percent_threshold_right/100)

      for (index_right in seq(1, length(spl_first_right))) # OG index_right, der in enumerate(spl_first_right(x_axis_right))
      {
        if (abs(spl_first_right[index_right] < spl_first_right_median) &
            (filter_data[date + index_right] < median_right))
        {
          right <- date + index_right
          break
        }
      }
    }

    if (!is.na(left) & left != date & (date - left) < 40)
    {
      duration <- date - left
    } else if (!is.na(right))
    {
      duration <- right - date
    } else {
      duration <- 0
    }
  }
  return(list(duration = duration, left = left, right = right))
}

