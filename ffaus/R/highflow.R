# =====================================================
# High flow functions
# Author (that's a strong word): Luke Lloyd-Jones
# Date started: 26/02/2021
# Date updated: 31/05/2021
# =====================================================
# Updates log
#


#' Water year high flow exccedance and shape
#' @description
#' Summarises the initiation and magnitude of the flow recession,
#' which corresponds to the final peak and decay of the hydrograph
#' to the dry season. For the following metrics we compute the 80th,
#' 90th, 95th, and 98th percentiles of the flow data (Q*) for the whole period of
#' flow. These are then used as exceedance values for the computation
#' of the following metrics. For each magnitude threshold the flow
#' events above these thresholds are summarised.
#'
#' @export
#' @param flow_matrix a 'by date' row data frame with 2 columns named (Date, Flow).
#' @param exceedance_percent vector of length 1 by 4 containing the exceedance percentages for calculating thresholds.
#' @param max_nan_allowed_per_year maximum number of NA days per-year allowed. Recommend imputing NAs prior to running ffaus functions.
#' @param do_plot should diagnostic plots be generated? 1 = yes, 0 = no.
#'
#' @return A list with two data frame elements of size 1 by no. water years
#' * timing - the median over the exceedance event start dates.
#' * duration - Sum over the days in which flow exceeds the Q∗ threshold.
#' * freq - the number of events that exceed the Q∗ threshold. An event has to be at lest one
#'          day and a gap of three days to be a unique event.
#' * magnitude - for all events that exceed the Q∗ threshold take the median over the maximum values for each event.
#' * magnitude_exceed - the exceedance thresholds for each of the exceedance_percent vector percentiles.
#' * peak_magnitude - the peak magnitude for that water year.
#' * peak_magnitude_tim - the water year day of peak magnitude.
calcHighflowAnnual <- function(flow_matrix,
                               exceedance_percent = c(98, 95, 90, 80),
                               max_nan_allowed_per_year  = 100,
                               do_plot = 1)
{

  # Get the water years
  wtr_yrs <- unique(flow_matrix$Water_Year)

  # Get peak percentiles calculated from each year's peak flow value

  high_percentiles <- exceedance_percent / 100     # for high flow metrics

  peak_flows                 <- data.frame(matrix(0, nrow = 1, ncol = length(wtr_yrs)))
  highflow_exceedance_values <- data.frame(matrix(0, nrow = 1, ncol = length(high_percentiles)))

  colnames(peak_flows)                 <- wtr_yrs
  colnames(highflow_exceedance_values) <- paste0("HFE", high_percentiles)


  # Add high flow percentiles and peak flow exceedance vals together for final list of exceedance values

  for (pct in seq(1, length(high_percentiles)))
  {
    highflow_exceedance_values[pct] <- stats::quantile(flow_matrix$Flow, probs = (high_percentiles[pct]))
  }

  exceedance_values <- data.frame(highflow_exceedance_values) # five peak exceedance vals plus four high flow exceedance vals

  freq           <- matrix(NA, nrow = length(exceedance_values), ncol = length(wtr_yrs))
  rownames(freq) <- names(exceedance_values); colnames(freq) <- wtr_yrs

  duration       <- matrix(NA, nrow = length(exceedance_values), ncol = length(wtr_yrs))
  rownames(duration) <- names(exceedance_values); colnames(duration) <- wtr_yrs

  timing         <- matrix(NA, nrow = length(exceedance_values), ncol = length(wtr_yrs))
  rownames(timing) <- names(exceedance_values); colnames(timing) <- wtr_yrs

  magnitude      <- matrix(NA, nrow = length(exceedance_values), ncol = length(wtr_yrs))
  rownames(magnitude) <- names(exceedance_values); colnames(magnitude) <- wtr_yrs

  magnitude_exceed      <- matrix(NA, nrow = length(exceedance_values), ncol = length(wtr_yrs))
  rownames(magnitude_exceed) <- names(exceedance_values); colnames(magnitude_exceed) <- wtr_yrs

  peak_magnitude <- matrix(NA, nrow = 1, ncol = length(wtr_yrs))
  rownames(peak_magnitude) <- "Mag"; colnames(peak_magnitude) <- wtr_yrs

  peak_magnitude_tim <- matrix(NA, nrow = 1, ncol = length(wtr_yrs))
  rownames(peak_magnitude_tim) <- "Mag_Tim"; colnames(peak_magnitude_tim) <- wtr_yrs


  for (wtryr in seq(1, length(wtr_yrs)))
  {
    flow_matrix_wy <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr])

    # Get peak mag
    peak_magnitude[1, wtryr]     <- max(flow_matrix_wy$Flow, na.rm = T)[1]
    peak_magnitude_tim[1, wtryr] <- which(flow_matrix_wy$Flow ==  peak_magnitude[1, wtryr])[1]

    if ( (sum(is.na(flow_matrix_wy$Flow)) > max_nan_allowed_per_year) |
         all(flow_matrix_wy$Flow == 0) )
    {
      print(paste0("Looking bad for high flow calcs in ", wtr_yrs[wtryr]))
      next
    }

    # -------------------
    # This is my solution
    # -------------------

    # Try another way
    if (do_plot == 1)
    {
      plot(seq(1, length(flow_matrix_wy$Flow)), flow_matrix_wy$Flow,
           type = "l", lwd = 2, main = paste0("High-flow Q80 ", wtr_yrs[wtryr]),
           xlab = 'Water year day',
           ylab = 'Flow (ML/d)')
      graphics::abline(h=as.numeric(exceedance_values["HFE0.8"]))
    }

    # Setup the matrix so the exceedance function likes it
    ts_dat <- flow_matrix_wy[, c("Date", "Flow")]
    colnames(ts_dat) <- c("t", "temp")

    for (ev in seq(1, length(exceedance_values)))
    {
      if (max(flow_matrix_wy$Flow) >= as.numeric(exceedance_values[ev]))
      {
        if (as.numeric(exceedance_values[ev]) == 0)
        {
          res    <- exceedance(ts_dat, threshold = 0.001,
                               max_gap = 3, min_duration = 1)$exceedance
        } else {
          res    <- exceedance(ts_dat, threshold = as.numeric(exceedance_values[ev]),
                               max_gap = 3, min_duration = 1)$exceedance
        }
        # Assign results to bigger matrices

        # Frequency := how many times greater you exceeded the threshold
        freq[ev, wtryr]      <- max(res$exceedance_no)

        # Duration := sum over the days exceeding this threshold
        duration[ev, wtryr]  <- sum(res$duration)

        # Timing is a bit weird := median exceedance day over the start days and reported as a water year day
        timing[ev, wtryr]    <- stats::median(res$index_start)

        # Magnitude is weird as well. They just use the exceedance value. I'm going with the median over the maximums
        magnitude[ev, wtryr] <- stats::median(res$int_max_abs)

      }
      # Let's give the exceedance values as well
      magnitude_exceed[ev, wtryr] <- as.numeric(exceedance_values[ev])
    }
  }

  return(list(timing = timing, duration = duration, freq = freq,
              magnitude = magnitude, magnitude_exceed = magnitude_exceed,
              peak_magnitude = peak_magnitude,
              peak_magnitude_tim = peak_magnitude_tim))
}

# ------------------------------------------
# Get peak flow Whipple shape parameters
# ------------------------------------------

#' Highflow/flood shape and frequency of peaks.
#'
#' @description
#' We further summarise peak flow shape and peak frequency hydrometrics functions from
#' Allison Whipple’s hydrospatial R package. The code rests on passing a user defined percentile
#' value in which to summarise the peaks of each year’s hydrograph using the whipple_exceed_thresh
#' parameter, with the default being 0.95.
#'
#'
#' @export
#' @param flow_matrix a 'by date' row data frame with 2 columns named (Date, Flow).
#' @param whipple_exceed_thresh percentile threshold to compute flow value from flow data over all years.
#'
#' @return A data frame of size 5 by no. water years named whipple_shape. Each row corresponds to
#' metric
#' * No_Events  -  Number of events that exceed the threshold defined by whipple_exceed_thresh
#' * No_Peaks_Avg    - The average number of peaks within exceedance events.
#' * No_Peaks_Max    - The maximum number of inter-event peaks.
#' * Frac_V_Cent_Avg - Over all events the mean value of the number of days to > 0.5 of total flood event volume divided by the total number event days.
#' * Frac_V_Cent_Max - For the event that contains the maximum flow for the water year, the value of the number of days to > 0.5 of total flood event volume divided by the total number event days.
calcWhippleHighFlow <- function(flow_matrix, whipple_exceed_thresh = 0.95)
{
  wtr_yrs          <- unique(flow_matrix$Water_Year)


  QV <- stats::quantile(flow_matrix$Flow, probs = whipple_exceed_thresh)

  # Call the Whipple prep functions. This might need rearranging for Aus context
  flw_whip    <- flow_matrix[, c("Date", "Flow")]
  colnames(flw_whip) <- c("dt", "flw")
  flw_whip$dt <- format.Date(flw_whip$dt, "%m-%d-%Y")
  flow_preped <- utils_flowformat_2(flw_whip, 7)

  # Compute the flood events
  fld_events <- utils_floodid(flow_preped, Q = QV)[[1]][[1]]

  # Table into something that a watering year can interpret
  wtr_yrs_whp <- unique(flow_preped$wyr)

  # Set up an empty data frame for the results
  whp_res <- data.frame(matrix(NA, nrow = 5, ncol = length(wtr_yrs_whp)))
  colnames(whp_res) <- wtr_yrs_whp-1
  rownames(whp_res) <- c("No_Events", "No_Peaks_Avg", "No_Peaks_Max", "Frac_V_Cent_Avg", "Frac_V_Cent_Max")

  for (wtryr in seq(1, length(wtr_yrs_whp)))
  {
    print(paste0("Calculating flood shape metrics for year ", wtryr))
    if (length(flow_preped$wyr[which(flow_preped$wyr == wtr_yrs_whp[wtryr])]) < 250 |
        dim(fld_events[which(fld_events$wyr == wtr_yrs_whp[wtryr]), ])[1] == 0)
    {
      next
    }
    evts_wy <- fld_events[which(fld_events$wyr == wtr_yrs_whp[wtryr]), ]
    max_ind <- which(evts_wy$flw_pk == max(evts_wy$flw_pk))[1]

     # No. of exceedence events in each watering year
    whp_res["No_Events", wtryr] <- dim(evts_wy)[1]

    # Average no. of peaks in the exceedences
    whp_res["No_Peaks_Avg", wtryr]    <- mean(evts_wy$no_pks, na.rm = T)

    # No. of peaks in the maximum event
    whp_res["No_Peaks_Max", wtryr]    <- evts_wy$no_pks[max_ind]

    # Average over the frac v cent
    whp_res["Frac_V_Cent_Avg", wtryr] <- mean(evts_wy$frac_v_cent, na.rm = T)

    # Frac v cent for max event
    whp_res["Frac_V_Cent_Max", wtryr] <-  evts_wy$frac_v_cent[max_ind]

  }

  return(whipple_shape = whp_res)
}




# ------------------------------------------------------
# Helper functions ported from Hydrospatioal R package
# ------------------------------------------------------

utils_flowformat_2 <- function(d, start_month)
{
  d$dt <- lubridate::mdy(d$dt)
  d$wyr <- ifelse(lubridate::month(d$dt) >= start_month, lubridate::year(d$dt) + 1, lubridate::year(d$dt))
  for (i in min(d$wyr):max(d$wyr)) {
    d$wyrd[d$wyr == i] <- seq(1:sum(d$wyr == i))
    d$cflw[d$wyr == i] <- cumsum(d$flw[d$wyr == i]) * 3600 *
      24
  }
  d$jd <- lubridate::yday(d$dt)
  anvol <- data.frame(d %>% dplyr::group_by(wyr) %>% dplyr::summarize(vol = sum(flw) *
                                                          3600 * 24))
  qntl <- stats::quantile(anvol$vol, probs = seq(0, 1, 0.2))
  qntln <- c(0.2, 0.4, 0.6, 0.8, 1)
  anvol$qntl <- NA
  for (i in 1:5) {
    anvol$qntl[which(anvol$vol <= qntl[i + 1] & anvol$vol >=
                       qntl[i])] <- qntln[i]
  }
  for (i in min(d$wyr):max(d$wyr)) {
    d$anvol[d$wyr == i] <- anvol$vol[anvol$wyr == i]
    d$qntl[d$wyr == i] <- anvol$qntl[anvol$wyr == i]
  }
  d$hflw <- NA
  d$hflw[1] <- d$flw[1]
  d$hflw[2] <- max(d$flw[1:2])
  d$hflw[3] <- max(d$flw[1:3])
  d$hflw[4] <- max(d$flw[1:4])
  d$hflw[5] <- max(d$flw[1:5])
  d$hflw[6] <- max(d$flw[1:6])
  d$hflw[7] <- max(d$flw[1:7])
  for (i in (8:nrow(d))) {
    d$hflw[i] <- max(d$flw[(i - 7):i])
  }
  d <- transform(d, limb = ifelse(flw >= hflw, "r", "f"))
  return(d)
}
