# =====================================================
# Fall winter baseflow
# Author (that's a strong word): Luke Lloyd-Jones
# Date started: 26/02/2021
# Date updated: 31/05/2021
# =====================================================
# Updates log
#

#' Wet season baseflow summary
#' @description
#' Summarises the wet season baseflow magnitude and duration.
#'
#' @export
#' @param flow_matrix a 'by date' row data frame with 2 columns named (Date, Flow).
#' @param wet_timings The water year date for each water year in which the flow is beginning to
#'                            return to its wetter period. Computed using returnToWetDate().
#' @param recession_timings The water year date for each water year in which the flow initiation
#'                                    is computed to have begun. Corresponds typically to first substantial
#'                                    peak in the hydrograph post dry-season. Computed using
#'                                    calcFlowInitiationFlushTimingsDurations().
#' @param dry_season_start_dates the water year date for each water year in which the dry-season is
#'                               computed to have begun. Computed using calcStartOfdry_season().
#' @param do_plot should diagnostic plots be generated? 1 = yes, 0 = no.
#'
#' @return A list with two data frame elements of size 1 by no. water years
#' * wet_baseflows_10 - 10th percentile of the flow data between the wet season start date and recession start date.
#' * wet_baseflows_50 - 50th percentile of the flow data between the wet season start date and recession start date.
#' * wet_bfl_durs     - number of water years days between wet season start date and recession start date.
calcWetSeasonBaseflow <- function(flow_matrix, wet_timings, recession_timings, dry_season_start_dates, do_plot = 1)
{
  wtr_yrs          <- unique(flow_matrix$Water_Year)
  wet_baseflows_10 <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  wet_baseflows_50 <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))
  wet_bfl_durs     <- data.frame(matrix(NA, nrow = 1, ncol = length(wtr_yrs)))

  colnames(wet_baseflows_10) <- wtr_yrs
  colnames(wet_baseflows_50) <- wtr_yrs
  colnames(wet_bfl_durs)     <- wtr_yrs


  for (wtryr in seq(1, length(wtr_yrs)))
  {
    flow_matrix_wy   <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr])
    flow_data        <- flow_matrix_wy$Flow
    x_axis    <- seq(1, length(flow_data))
    recession_timing <- recession_timings[wtryr]
    wet_timing       <- wet_timings[wtryr]

    if (wtryr != 1)
    {
      if ((as.numeric(wtr_yrs[wtryr]) - as.numeric(wtr_yrs[wtryr - 1]) == 1) &
          !is.na(wet_timing) &
          !is.na(recession_timing) )
      {
        flow_matrix_wy_mns_one <- dplyr::filter(flow_matrix, flow_matrix$Water_Year == wtr_yrs[wtryr - 1])
        #days_frow_lst_year     <- 30 # Just take 30 as the dry season can start pretty earlier and one year's dry season is another's wet
        if (!is.na(dry_season_start_dates[1, (wtryr - 1)]))
        {
          if (dry_season_start_dates[1, (wtryr - 1)] > length(flow_matrix_wy_mns_one$Flow))
          {
            flow_prt_mns_one       <- c()
          } else {
            flow_prt_mns_one       <- flow_matrix_wy_mns_one$Flow[dry_season_start_dates[1, (wtryr - 1)]:length(flow_matrix_wy_mns_one$Flow)]
          }
        } else {
          flow_prt_mns_one       <- flow_matrix_wy_mns_one$Flow[(length(flow_matrix_wy_mns_one$Flow) - 30):length(flow_matrix_wy_mns_one$Flow)]
        }
        flow_data <- c(flow_prt_mns_one, flow_data)
        x_axis    <- seq(1, length(flow_data))
        days_frow_lst_year <- length(flow_prt_mns_one)
        wet_timing <- days_frow_lst_year + wet_timing
        recession_timing <- days_frow_lst_year + recession_timing
      }
    }

    if (do_plot == 1)
    {
      #par(mfrow=c(1, 2))
      plot(x_axis, flow_data,
           type = "l", lwd = 2, main = paste0("Wet Season Baseflow", wtr_yrs[wtryr]),
           xlab = 'Water year day + 30 days of next year',
           ylab = 'Flow (ML/d)')
      graphics::abline(v = wet_timing, col = "blue")
      graphics::abline(v = recession_timing, col = 'red')
    }

    if ( (!is.na(recession_timing)) &
         (wet_timing) &
         !is.na(recession_timing) &
         !is.na(wet_timing) )
    {
      if (!is.na(wet_timing) & (recession_timing > wet_timing))
      {
        flow_data = flow_data[as.numeric(wet_timing):as.numeric(recession_timing)]
      } else {
        flow_data = c()
      }
    } else {
      flow_data = c()
    }

    if (!is.null(flow_data))
    {
      wet_baseflows_10[wtryr] <- stats::quantile(flow_data, probs = 0.10, na.rm = T)
      wet_baseflows_50[wtryr] <- stats::quantile(flow_data, probs = 0.50, na.rm = T)
      wet_bfl_durs[wtryr]     <- length(flow_data)
    } else {
      wet_baseflows_10[wtryr] <- NA
      wet_baseflows_50[wtryr] <- NA
      wet_bfl_durs[wtryr]     <- NA
    }
  }
  return(list(wet_baseflows_10 = wet_baseflows_10, wet_baseflows_50 = wet_baseflows_50, wet_bfl_durs = wet_bfl_durs))
}
