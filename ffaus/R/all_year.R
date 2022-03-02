# =====================================================
# All year summary function
# Author (that's a strong word): Luke Lloyd-Jones
# Date started: 26/02/2021
# Date updated: 31/05/2021
# =====================================================
# Updates log
#

#' Summary metrics
#' @description
#' Computes by water year and over all year
#' summary statistics and exceedance metrics
#'
#'
#' @export
#' @param flow_matrix  A 'by date' row data frame with 2 columns named (Date, Flow).
#' @param max_nan_allowed_per_year Maximum number of NA days per-year allowed. Recommend imputing NAs prior to running ffaus functions.
#' @param do_plot Should diagnostic plots be generated? 1 = yes, 0 = no.
#' @param user_q75_thresh A user defined 75th percentile threshold value.
#'                        In the same units of flow as in flow_matrix.
#' @param user_q90_thresh A user defined 90th percentile threshold value.
#'                        In the same units of flow as in flow_matrix.
#'
#' @return A list with elements
#' * average_annual_flows - a one-row data frame with average flow for each
#'                          water year in each column.
#' * standard_deviations
#' * coefficient_variations
#' * mags_global
#' * mags
#' * tims
#' * freqs
#' * durs
#' * mags_long_evnt
#' * tims_init_long_evnt
#' * seas_init_long_evnt
#' * durs_long_evnt
calcAllYear <- function(flow_matrix,
                        max_nan_allowed_per_year = 100,
                        do_plot = 1,
                        user_q75_thresh = NULL,
                        user_q90_thresh = NULL)
{

  wtr_yrs                          <- unique(flow_matrix$Water_Year)
  average_annual_flows             <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  colnames(average_annual_flows)   <- wtr_yrs
  standard_deviations              <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  colnames(standard_deviations)    <- wtr_yrs
  coefficient_variations           <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  colnames(coefficient_variations) <- wtr_yrs

  # Want magnitudes, timing?, frequency, duration within each year
  qnt_nms        <- c("Q10", "Q25", "Q50", "Q75","Q90")
  mags           <- data.frame(matrix(NA, nrow = 5, ncol = length(wtr_yrs)))
  colnames(mags) <- wtr_yrs
  rownames(mags) <- paste0(qnt_nms, "_Mag")

  tims           <- data.frame(matrix(NA, nrow = 5, ncol = length(wtr_yrs)))
  colnames(tims) <- wtr_yrs
  rownames(tims) <- paste0(qnt_nms, "_Tim")

  freqs           <- data.frame(matrix(NA, nrow = 5, ncol = length(wtr_yrs)))
  colnames(freqs) <- wtr_yrs
  rownames(freqs) <- paste0(qnt_nms, "_Fre")

  durs           <- data.frame(matrix(NA, nrow = 5, ncol = length(wtr_yrs)))
  colnames(durs) <- wtr_yrs
  rownames(durs) <- paste0(qnt_nms, "_Dur")

  # Longest event
  mags_long_evnt      <- mags; rownames(mags_long_evnt)      <- paste0(qnt_nms, "_Mag_Lng_Evt")
  tims_init_long_evnt <- tims; rownames(tims_init_long_evnt) <- paste0(qnt_nms, "_Strt_Tim_Lng_Evt")
  seas_init_long_evnt <- tims; rownames(seas_init_long_evnt) <- paste0(qnt_nms, "_Strt_Sea_Lng_Evt")
  durs_long_evnt      <- durs; rownames(durs_long_evnt)      <- paste0(qnt_nms, "_Dur_Lng_Evt")

  mags_global    <- stats::quantile(flow_matrix$Flow, probs = c(0.1, 0.25, 0.5, 0.75, 0.90), na.rm = T)
  if (!is.null(user_q75_thresh))
  {
    mags_global[4] <- user_q75_thresh
  }
  if (!is.null(user_q90_thresh))
  {
    mags_global[5] <- user_q90_thresh
  }

  for (wtryr in seq(1, length(wtr_yrs)))
  {
    print(paste0("Calculating annual summary statistics for water year ", wtr_yrs[wtryr]))
    flow_matrix_wy <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr])

    # Check to see if water year has more than allowed NA or zeros
    if (sum(is.na(flow_matrix_wy$Flow)) > max_nan_allowed_per_year | length(flow_matrix_wy$Flow) < 300)
    {
      next
    }
    average_annual_flows[wtryr]   <- mean(flow_matrix_wy$Flow, na.rm = T)
    standard_deviations[wtryr]    <- stats::sd(flow_matrix_wy$Flow,   na.rm = T)
    coefficient_variations[wtryr] <- standard_deviations[wtryr]  / average_annual_flows[wtryr]
    mags[, wtryr]                 <- stats::quantile(flow_matrix_wy$Flow, probs = c(0.1, 0.25, 0.5, 0.75, 0.90), na.rm = T)

    if (do_plot == 1)
    {
      plot(seq(1, length(flow_matrix_wy$Flow)), flow_matrix_wy$Flow,
           type = "l", lwd = 2, main = paste0("Annual percentiles ", wtr_yrs[wtryr]),
           xlab = 'Water year day',
           ylab = 'Flow (ML/d)')
    }
    # Special exceedance function
    ts_dat <- flow_matrix_wy[, c("Date", "Flow")]
    colnames(ts_dat) <- c("t", "temp")
    cols <- grDevices::colors()[100:105]
    for (j in seq(1, 5))
    {
      if ( (max(flow_matrix_wy$Flow) >= as.numeric(mags_global[j]) &
            min(flow_matrix_wy$Flow) < as.numeric(mags_global[j])) )
      {
        if (mags_global[j] < 0.001)
        {
          res    <- exceedance(ts_dat, threshold = 0.001, max_gap = 3, min_duration = 1)
        } else {
          res    <- exceedance(ts_dat, threshold = as.numeric(mags_global[j]), max_gap = 3, min_duration = 1)
        }
        if (do_plot == 1)
        {
          graphics::abline(h = as.numeric(mags_global[j]), col = cols[j], lwd = 2)
        }
        if (dim(res$exceedance)[1] > 0)
        {
          freqs[j, wtryr] <- dim(res$exceedance)[1]  # Number of sections of the hydrograph where the flow was above QX
          # Weighted average of initiation dates
          tims[j, wtryr]  <- sum(res$exceedance$index_start * res$exceedance$duration) /
                                 sum(res$exceedance$duration) # Trying to weight by the most days
          durs[j, wtryr]  <- sum(res$exceedance$duration)

          lng_evt                       <- which.max(res$exceedance$duration)
          mags_long_evnt[j, wtryr]      <- res$exceedance[lng_evt, "int_max_abs"]
          tims_init_long_evnt[j, wtryr] <- res$exceedance[lng_evt, "index_start"]
          seas_init_long_evnt[j, wtryr] <- as.character(flow_matrix_wy[res$exceedance[lng_evt, "index_start"], "Season"])
          durs_long_evnt[j, wtryr]      <- res$exceedance[lng_evt, "duration"]
        }
      } else if (min(flow_matrix_wy$Flow) >= as.numeric(mags_global[j])) # All the data is bigger
      {
        # All the data is bigger
        freqs[j, wtryr] <- 1
        # Weighted average of initiation dates
        tims[j, wtryr]  <- 1
        durs[j, wtryr]  <- dim(ts_dat)[1]

        mags_long_evnt[j, wtryr]      <- max(ts_dat[, 2])
        tims_init_long_evnt[j, wtryr] <- 1
        seas_init_long_evnt[j, wtryr] <- as.character(flow_matrix_wy[1, "Season"])
        durs_long_evnt[j, wtryr]      <- dim(ts_dat)[1]
      }
    }
  }

  return(list(average_annual_flows   = average_annual_flows,
              standard_deviations    = standard_deviations,
              coefficient_variations = coefficient_variations,
              mags_global = mags_global,
              mags  = mags,
              tims  = tims,
              freqs = freqs,
              durs  = durs,
              mags_long_evnt = mags_long_evnt,
              tims_init_long_evnt = tims_init_long_evnt,
              seas_init_long_evnt = seas_init_long_evnt,
              durs_long_evnt = durs_long_evnt))
}
