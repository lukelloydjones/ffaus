# =====================================================
# Functions to help with making the flow matrix and
# determining peaks of slow time series
# Author (that's a strong word): Luke Lloyd-Jones
# Date started: 26/02/2021
# Date updated: 26/02/2021
# =====================================================
# Updates log
#
# What's in here
# -

# ------------------------------------------------
# Flow peak detection
# ------------------------------------------------
#' @importFrom magrittr %>%
NULL

peakDet <- function(v, delta, x = NULL)
{

  maxtab <- matrix(0, nrow = 0, ncol = 2)
  mintab <- matrix(0, nrow = 0, ncol = 2)

  if (is.null(x))
  {
    x <- seq(1, length(v))
  }

  if (length(v) != length(x))
  {
    stop("Input vectors v and x must have same length")
  }

  if (!assertthat::is.scalar(delta))
  {
    stop('Input argument delta must be a scalar')
  }

  if (delta <= 0)
  {
    stop('Input argument delta must be positive')
  }

  mn <- Inf
  mx <- -Inf
  mnpos <- NaN
  mxpos <- NaN

  lookformax <- TRUE

  for (i in seq(1, length(v)))
  {
    this <- v[i]
    if (this > mx)
    {
      mx <- this
      mxpos <- x[i]
    }
    if (this < mn)
    {
      mn <- this
      mnpos <- x[i]
    }
    if (lookformax)
    {
      if (this < mx - delta)
      {
        maxtab <- rbind(maxtab, c(mxpos, mx))
        mn <- this
        mnpos <- x[i]
        lookformax <- FALSE
      }
    } else {
      if (this > mn + delta)
      {
        mintab <- rbind(mintab, c(mnpos, mn))
        mx <- this
        mxpos <- x[i]
        lookformax <- TRUE
      }
    }
  }
  # Add on the last minimum in an attempt to get the same
  # dim matrices
  mintab <- rbind(mintab, c(mnpos, mn))
  return(list(maxtab = maxtab, mintab = mintab))
}


# ------------------------------------------------
# Make flow matrix
# ------------------------------------------------


elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#' Flow to ffaus flow format.
#' @description
#' Function that takes flow data (Date, Flow) and processes to
#' ffaus flow format.
#'
#' @export
#' @param flow_matrix a 'by date' row data frame with 2 columns named (Date, Flow).
#'                    The colnames need to be Date and Flow.
#' @param water_year_st a character date in format %m-%d that specifies the month and day start of the water year.
#'                      For example "07-01" is the first of the July water year start date.
#' @param impute would you like to impute missing values and dates? Only recommend if less than a handful per year.
#'
#' @return A list with two data frame elements
#' * flow_deep - Date (starting at first water year day) by 8 column object that contains in each row
#'
#'               * Date - in %Y-%m-%d format.
#'               * Flow - in units of data provided.
#'               * Season - summer (sum), autumn (aut), winter (win), or spring (spr).
#'               * Month (Julian) - Month as an interger 1-12 (Jan-Dec).
#'               * Year - Integer for year.
#'               * Year_Day - Julian day of year
#'               * Water_Year - Integer for water year.
#'               * Water_Year_Day - Integer for water year day relative to water_year_st.
makeFlowMatrix <- function(flow_matrix, water_year_st, impute = NULL)
{

  # Input - Take a flow object with Date, Flow columns - Date in format "%Y/%m/%d" preferred
  #       - Column name for Flow must be "Flow"!
  # Output
  #       - A deep matrix with Date, Flow, Year, Year_Day, Season, Month
  #       - A wide matrix with
  if (colnames(flow_matrix)[(grep("Flow", colnames(flow_matrix), fixed = T))] != "Flow")
  {
    stop("You need to name the column with the flow data 'Flow'")
  }
  if (is.null(water_year_st))
  {
    stop("You need to provide a water year start time in format %Y-%m-%d format")
  }
  if (class(flow_matrix$Date) != "Date")
  {
    flow_matrix$Date <- as.Date(lubridate::parse_date_time(x = flow_matrix$Date,
                                                          orders = c("m/d/y", "m/d/Y", "d/m/y", "d/m/Y")))
  }
  if (class(flow_matrix$Flow) != "numeric")
  {
    print("Warning: data class type for flow is not numeric. Check output carefully. Consider changing to numeric.")
    flow_matrix$Flow <- as.numeric(flow_matrix$Flow)
  }
  # Make sure the start date reference is in Date format
  water_year_st <- as.Date(paste0((min(lubridate::year(flow_matrix$Date)) - 1), "-", water_year_st))
  # Season
  #  - Summer - 01/12 - last Day Feb
  #  - Autumn - 01/03 - 31/05
  #  - Winter - 01/06 - 30/08
  #  - Spring - 01/09 - 31/11
  # Add a year, year day, month and seasons variables
  flow_matrix$Season         <- metR::season(flow_matrix$Date)
  levels(flow_matrix$Season) <- c("sum", "aut", "win", "spr")
  flow_matrix$Month          <- lubridate::month(flow_matrix$Date)
  flow_matrix$Year           <- lubridate::year(flow_matrix$Date)
  flow_matrix$Year_Day       <- lubridate::yday(flow_matrix$Date)
  flow_matrix$Water_Year     <- floor(elapsed_months(flow_matrix$Date, water_year_st) / 12) + lubridate::year(water_year_st)
  flow_matrix$Water_Year_Day <- flow_matrix$Date - water_year_st
  flow_matrix                <- as.data.frame(flow_matrix %>% dplyr::group_by(Water_Year) %>% dplyr::mutate(Min_Day = min(Water_Year_Day)))
  flow_matrix                <- as.data.frame(flow_matrix %>% dplyr::group_by(Water_Year) %>% dplyr::mutate(Max_Day = max(Water_Year_Day)))
  flow_matrix                <- as.data.frame(flow_matrix %>% dplyr::group_by(Water_Year) %>% dplyr::mutate(Yr_Length_Day = max(Max_Day - Water_Year_Day + 1)))
  # Remove small year lengths
  too_few          <- as.data.frame(flow_matrix %>% dplyr::group_by(Water_Year) %>% dplyr::summarise(no_values = length(Flow)))
  print(paste0("These years removed as too few data for that year "))
  print(too_few$Water_Year[too_few$no_values < 365])
  too_few_years <- too_few$Water_Year[too_few$no_values < 365]
  if (length(too_few_years > 0))
  {
    flow_matrix                <- flow_matrix[-which(flow_matrix$Water_Year %in% too_few_years), ]
  }
  flow_matrix$Water_Year_Day <- as.numeric(flow_matrix$Yr_Length_Day - (flow_matrix$Max_Day - flow_matrix$Water_Year_Day))
  # Replace back the first year
  str_year_vals                             <- which(flow_matrix$Year == lubridate::year(water_year_st))
  flow_matrix$Water_Year_Day[str_year_vals] <- as.numeric(flow_matrix$Water_Year_Day[str_year_vals] + flow_matrix$Min_Day[str_year_vals] - 1)
  flow_matrix   <- subset(flow_matrix, select = -c(Min_Day, Max_Day, Yr_Length_Day))
  flow_mat_wd            <- stats::reshape(flow_matrix,
                                           idvar     = "Water_Year_Day",
                                           timevar   = "Water_Year",
                                           direction = "wide")
  flow_mat_wd          <- flow_mat_wd[order(flow_mat_wd$Water_Year_Day), ]
  if (!is.null(impute))
  {
    if (sum(is.na(flow_matrix$Flow)) > 0)
    {
      print(paste0("This many missing values in your data ", sum(is.na(flow_matrix$Flow))))
      xaxis   <- seq(1, length(flow_matrix$Flow))
      spl_fit <- stats::splinefun(xaxis,
                                  flow_matrix$Flow,
                                  method = "natural")
      spl          <- spl_fit(xaxis, deriv = 0)
      spl[spl < 0] <- 0
      flow_matrix$Flow[is.na(flow_matrix$Flow)] <- spl[is.na(flow_matrix$Flow)]
    }
  }
  # Return
  return(list(flow_deep = flow_matrix))
}

# ------------------------------------
# OLD HELPER FUNCTIONS
# ------------------------------------

argmax <- function(x, y, y.smooth, w = 1, ...) {
  n <- length(y)
  y.max <- zoo::rollapply(zoo::zoo(y.smooth), 2 * w + 1, max, align = "center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

## "Proof" that the derivatives are okay, by comparing with approximation
diff.quot <- function(x, y) {
  ## Difference quotient (central differences where available)
  n <- length(x);
  i1 <- 1:2;
  i2 <- (n-1):n
  c(diff(y[i1]) / diff(x[i1]), (y[-i1] - y[-i2]) / (x[-i1] - x[-i2]),
    diff(y[i2]) / diff(x[i2]))
}

# ----------------------------------------------
# R package RmarineHeatWaves exceedance function
# ----------------------------------------------

exceedance  <- function (data, x = t, y = temp, threshold = 20, below = FALSE,
          min_duration = 5, join_across_gaps = TRUE, max_gap = 2, max_pad_length = 3)
{
  temp <- NULL
  ts.x <- eval(substitute(x), data)
  ts.y <- eval(substitute(y), data)
  t_series <- tibble::tibble(ts.x, ts.y)
  rm(ts.x)
  rm(ts.y)
  t_series$ts.y <- zoo::na.approx(t_series$ts.y, maxgap = max_pad_length)
  if (missing(threshold))
    stop("Oh no! Please provide a threshold against which to calculate exceedances.")
  if (threshold > max(t_series$ts.y, na.rm = T)) {
    stop(paste("The given threshold value of ", threshold,
               " is greater than the maximum stream flow of ", max(t_series$ts.y,
                                                                   na.rm = T), " present in this time series.",
               sep = ""))
  }
  if (threshold < min(t_series$ts.y, na.rm = T)) {
    stop(paste("The given threshold value of ", threshold,
               " is less than the minimum stream flow of ", min(t_series$ts.y,
                                                                na.rm = T), " present in this time series.",
               sep = ""))
  }
  if (below) {
    t_series$ts.y <- -t_series$ts.y
    threshold <- -threshold
  }
  t_series$thresh <- rep(threshold, nrow(t_series))
  t_series$thresh_criterion <- t_series$ts.y >= t_series$thresh
  ex1 <- rle(t_series$thresh_criterion)
  ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
  s1 <- split(zoo::index(t_series$thresh_criterion), ind1)
  proto_exceedances <- s1[ex1$values == TRUE]
  index_stop <- index_start <- NULL
  proto_exceedances_rng <- lapply(proto_exceedances, function(x) data.frame(index_start = min(x),
                                                                            index_stop = max(x)))
  duration <- NULL
  protoFunc <- function(proto_data) {
    out <- proto_data %>% dplyr::mutate(duration = index_stop -
                                          index_start + 1) %>% dplyr::filter(duration >= min_duration) %>%
      dplyr::mutate(date_start = t_series$ts.x[index_start]) %>%
      dplyr::mutate(date_stop = t_series$ts.x[index_stop])
  }
  proto_exceedances <- do.call(rbind, proto_exceedances_rng) %>%
    dplyr::mutate(exceedance_no = cumsum(ex1$values[ex1$values ==
                                                      TRUE])) %>% protoFunc()
  if (length(proto_exceedances$index_start) == 0 & below ==
      FALSE) {
    stop(paste("No stream flow over ", threshold, " detected.",
               sep = ""))
  }
  if (length(proto_exceedances$index_start) == 0 & below ==
      TRUE) {
    stop(paste("No stream flow under ", threshold, " detected.",
               sep = ""))
  }
  t_series$duration_criterion <- rep(FALSE, nrow(t_series))
  for (i in 1:nrow(proto_exceedances)) {
    t_series$duration_criterion[proto_exceedances$index_start[i]:proto_exceedances$index_stop[i]] <- rep(TRUE,
                                                                                                         length = proto_exceedances$duration[i])
  }
  ex2 <- rle(t_series$duration_criterion)
  ind2 <- rep(seq_along(ex2$lengths), ex2$lengths)
  s2 <- split(zoo::index(t_series$thresh_criterion), ind2)
  proto_gaps <- s2[ex2$values == FALSE]
  proto_gaps_rng <- lapply(proto_gaps, function(x) data.frame(index_start = min(x),
                                                              index_stop = max(x)))
  proto_gaps <- do.call(rbind, proto_gaps_rng) %>% dplyr::mutate(exceedance_no = c(1:length(ex2$values[ex2$values ==
                                                                                                         FALSE]))) %>% dplyr::mutate(duration = index_stop - index_start +
                                                                                                                                       1)
  if (any(proto_gaps$duration >= 1 & proto_gaps$duration <=
          max_gap)) {
    proto_gaps <- proto_gaps %>% dplyr::mutate(date_start = t_series$ts.x[index_start]) %>%
      dplyr::mutate(date_stop = t_series$ts.x[index_stop]) %>%
      dplyr::filter(duration >= 1 & duration <= max_gap)
  }
  else {
    join_across_gaps <- FALSE
  }
  if (length(proto_gaps$index_start) == 0) {
    stop(paste("No stream flow in exceedance of ", threshold,
               " detected for ", min_duration, " or more consecutive days.",
               sep = ""))
  }
  if (join_across_gaps) {
    t_series$exceedance <- t_series$duration_criterion
    for (i in 1:nrow(proto_gaps)) {
      t_series$exceedance[proto_gaps$index_start[i]:proto_gaps$index_stop[i]] <- rep(TRUE,
                                                                                     length = proto_gaps$duration[i])
    }
  }
  else {
    t_series$exceedance <- t_series$duration_criterion
  }
  ex3 <- rle(t_series$exceedance)
  ind3 <- rep(seq_along(ex3$lengths), ex3$lengths)
  s3 <- split(zoo::index(t_series$exceedance), ind3)
  exceedances <- s3[ex3$values == TRUE]
  exceedance_no <- NULL
  exceedances_rng <- lapply(exceedances, function(x) data.frame(index_start = min(x),
                                                                index_stop = max(x)))
  exceedances <- do.call(rbind, exceedances_rng) %>% dplyr::mutate(exceedance_no = cumsum(ex3$values[ex3$values ==
                                                                                                       TRUE])) %>% protoFunc()
  t_series$exceedance_no <- rep(NA, nrow(t_series))
  for (i in 1:nrow(exceedances)) {
    t_series$exceedance_no[exceedances$index_start[i]:exceedances$index_stop[i]] <- rep(i,
                                                                                        length = exceedances$duration[i])
  }
  exceedances_list <- plyr::dlply(exceedances, c("exceedance_no"),
                                  function(x) with(t_series, data.frame(ts.x = c(ts.x[x$index_start:x$index_stop]),
                                                                        ts.y = c(ts.y[x$index_start:x$index_stop]), thresh = c(thresh[x$index_start:x$index_stop]),
                                                                        exceedance_rel_thresh = c(ts.y[x$index_start:x$index_stop]) -
                                                                          c(thresh[x$index_start:x$index_stop]))))
  thresh <- int_mean <- int_max <- int_cum <- exceedance_rel_thresh <- int_mean_abs <- int_max_abs <- int_cum_abs <- ts.y <- NULL
  exceedances <- cbind(exceedances, exceedances_list %>% dplyr::bind_rows(.id = "exceedance_no") %>%
                         dplyr::group_by(exceedance_no) %>% dplyr::summarise(date_peak = ts.x[ts.y ==
                                                                                                max(ts.y)][1], int_mean = mean(exceedance_rel_thresh),
                                                                             int_max = max(exceedance_rel_thresh), int_var = sqrt(stats::var(exceedance_rel_thresh)),
                                                                             int_cum = max(cumsum(exceedance_rel_thresh)), int_mean_abs = mean(ts.y),
                                                                             int_max_abs = max(ts.y), int_var_abs = sqrt(stats::var(ts.y)),
                                                                             int_cum_abs = max(cumsum(ts.y))) %>% dplyr::arrange(as.numeric(exceedance_no)) %>%
                         dplyr::select(-exceedance_no))
  exceedance_rel_thresh <- t_series$ts.y - t_series$thresh
  A <- exceedance_rel_thresh[exceedances$index_start]
  B <- t_series$ts.y[exceedances$index_start - 1]
  C <- t_series$thresh[exceedances$index_start - 1]
  if (length(B) + 1 == length(A)) {
    B <- c(NA, B)
    C <- c(NA, C)
  }
  exceedance_rel_thresh_start <- 0.5 * (A + B - C)
  exceedances$rate_onset <- ifelse(exceedances$index_start >
                                     1, (exceedances$int_max - exceedance_rel_thresh_start)/(as.numeric(difftime(exceedances$date_peak,
                                                                                                                 exceedances$date_start, units = "days")) + 0.5), NA)
  D <- exceedance_rel_thresh[exceedances$index_stop]
  E <- t_series$ts.y[exceedances$index_stop + 1]
  F <- t_series$thresh[exceedances$index_stop + 1]
  exceedance_rel_thresh_end <- 0.5 * (D + E - F)
  exceedances$rate_decline <- ifelse(exceedances$index_stop <
                                       nrow(t_series), (exceedances$int_max - exceedance_rel_thresh_end)/(as.numeric(difftime(exceedances$date_stop,
                                                                                                                              exceedances$date_peak, units = "days")) + 0.5), NA)
  if (below) {
    exceedances <- exceedances %>% dplyr::mutate(int_mean = -int_mean,
                                                 int_max = -int_max, int_cum = -int_cum, int_mean_abs = -int_mean_abs,
                                                 int_max_abs = -int_max_abs, int_cum_abs = -int_cum_abs)
    t_series <- t_series %>% dplyr::mutate(ts.y = -ts.y,
                                           thresh = -thresh)
  }
  names(t_series)[1] <- paste(substitute(x))
  names(t_series)[2] <- paste(substitute(y))
  list(threshold = t_series, exceedance = exceedances)
}
# ---------------------------------------
# R package Hydrospatial functions
# ---------------------------------------

utils_flowformat_2 <- function(d, start_month)
{
  d$dt <- lubridate::mdy(d$dt)
  d$wyr <- ifelse(lubridate::month(d$dt) >= start_month, lubridate::year(d$dt) + 1, lubridate::year(d$dt))
  for (i in min(d$wyr):max(d$wyr)) {
    d$wyrd[d$wyr == i] <- seq(1:sum(d$wyr == i))
    d$cflw[d$wyr == i] <- cumsum(d$flw[d$wyr == i]) * 3600 *
      24
  }
  d$jd  <- lubridate::yday(d$dt)
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

utils_floodid <- function (d, Q, outdir = NULL)
{
  f_data <- vector("list", length(Q))
  f_filenames <- vector("character", length(Q))
  daily_data <- vector("list", length(Q))
  for (i in 1:length(Q)) {
    names <- c("wyr", "start", "end", "start_wym", "end_wym",
               "start_d", "end_d", "cent_d", "pk_d", "no_d", "no_pks",
               "flw_pk", "flw_mean", "vol", "frac_v_cent", "r_rising",
               "r_falling", "cflw", "anvol")
    floods <- as.data.frame(matrix(0, nrow = nrow(d), ncol = length(names)))
    colnames(floods) <- names
    names_d <- c("dt", "wyr", "event_no", "day", "flw", "wyrd",
                 "mo", "yr", "fday", "anvol", "hflw", "limb")
    daily <- as.data.frame(matrix(0, nrow = nrow(d), ncol = length(names_d)))
    colnames(daily) <- names_d
    daily$dt <- as.Date(as.character(NA))
    daily$limb <- as.character(daily$limb)
    threshold <- Q[i]
    notflood <- 1
    flooddays <- 0
    meanflow <- 0
    volume <- 0
    vol_v <- numeric(length = 366)
    r_rising <- 0
    r_falling <- 0
    pk_cnt <- 0
    trigger_pk <- 0
    trigger_vly <- 1
    k <- 0
    dc <- 0
    for (j in 1:nrow(d)) {
      if (j == nrow(d)) {
        nxtflw <- d$flw[j]
      }
      else {
        nxtflw <- d$flw[j + 1]
      }
      if (d$flw[j] < threshold | (lubridate::month(d$dt[j]) == 9 &
                                  lubridate::day(d$dt[j]) == 30)) {
        if (notflood == 0) {
          floods$end[k] <- d$dt[j]
          floods$end_wym[k] <- ifelse(lubridate::month(d$dt[j]) >=
                                        10, lubridate::month(d$dt[j]) - 9, lubridate::month(d$dt[j]) +
                                        3)
          floods$end_d[k] <- d$wyrd[j]
          floods$no_d[k] <- flooddays
          if (flooddays == 1) {
            floods$no_pks[k] <- 1
          }
          else {
            floods$no_pks[k] <- pk_cnt
          }
          floods$flw_mean[k] <- meanflow/flooddays
          floods$vol[k] <- volume
          #vol_v <- vol_v[vol_v != 0]
          for (m in 1:length(vol_v)) {
            if (vol_v[m] > (0.5 * volume)) {
              floods$frac_v_cent[k] <- m/flooddays
              floods$cent_d[k] <- m + floods$start_d[k] - 1
              break
            }
          }
          floods$r_rising[k] <- r_rising
          floods$r_falling[k] <- r_falling
          flooddays <- 0
          meanflow <- 0
          volume <- 0
          vol_v <- numeric(length = 366)
          r_rising <- 0
          r_falling <- 0
          pk_cnt <- 0
          trigger_pk <- 0
          trigger_vly <- 1
          notflood <- 1
        }
      }
      else {
        if (notflood == 1) {
          k <- k + 1
          dc <- dc + 1
          floods$wyr[k] <- d$wyr[j]
          floods$anvol[k] <- d$anvol[j]
          floods$start[k] <- d$dt[j]
          floods$start_wym[k] <- ifelse(lubridate::month(d$dt[j]) >=
                                          10, lubridate::month(d$dt[j]) - 9, lubridate::month(d$dt[j]) +
                                          3)
          floods$start_d[k] <- d$wyrd[j]
          floods$cflw[k] <- d$cflw[j]
          flooddays <- flooddays + 1
          if (d$flw[j] > floods$flw_pk[k]) {
            floods$pk_d[k] <- d$wyrd[j]
            floods$flw_pk[k] <- d$flw[j]
          }
          meanflow <- meanflow + d$flw[j]
          volume <- volume + d$flw[j] * 3600 * 24
          vol_v[flooddays] <- volume
          r_rising <- min(r_rising, d$flw[j - 1] - d$flw[j])
          r_falling <- max(r_falling, d$flw[j] - nxtflw)
          notflood <- 0
          daily$wyr[dc] <- d$wyr[j]
          daily$wyrd[dc] <- d$wyrd[j]
          daily$mo[dc] <- lubridate::month(d$dt[j])
          daily$day[dc] <- lubridate::day(d$dt[j])
          daily$yr[dc] <- lubridate::year(d$dt[j])
          daily$event_no[dc] <- k
          daily$fday[dc] <- flooddays
          daily$flw[dc] <- d$flw[j]
          daily$dt[dc] <- d$dt[j]
          daily$anvol[dc] <- d$anvol[j]
          daily$hflw[dc] <- d$hflw[j]
          daily$limb[dc] <- as.character(d$limb[j])
        }
        else {
          flooddays <- flooddays + 1
          dc <- dc + 1
          if (d$flw[j] > floods$flw_pk[k]) {
            floods$pk_d[k] <- d$wyrd[j]
            floods$flw_pk[k] <- d$flw[j]
          }
          meanflow <- meanflow + d$flw[j]
          volume <- volume + d$flw[j] * 3600 * 24
          vol_v[flooddays] <- volume
          r_rising <- min(r_rising, d$flw[j - 1] - d$flw[j])
          r_falling <- max(r_falling, d$flw[j] - nxtflw)
          if ((d$flw[j] - nxtflw) > 0 & trigger_pk ==
              0) {
            pk_cnt <- pk_cnt + 1
            trigger_pk <- 1
            trigger_vly <- 0
          }
          if ((d$flw[j - 1] - d$flw[j]) == 0 & trigger_pk ==
              0 & (d$flw[j] - nxtflw) > 0) {
            pk_cnt <- pk_cnt + 1
            trigger_pk <- 1
            trigger_vly <- 0
          }
          if ((d$flw[j] - nxtflw) < 0 & trigger_vly ==
              0) {
            trigger_vly <- 1
            trigger_pk <- 0
          }
          daily$wyr[dc] <- d$wyr[j]
          daily$wyrd[dc] <- d$wyrd[j]
          daily$mo[dc] <- lubridate::month(d$dt[j])
          daily$day[dc] <- lubridate::day(d$dt[j])
          daily$yr[dc] <- lubridate::year(d$dt[j])
          daily$event_no[dc] <- k
          daily$fday[dc] <- flooddays
          daily$flw[dc] <- d$flw[j]
          daily$dt[dc] <- d$dt[j]
          daily$anvol[dc] <- d$anvol[j]
          daily$hflw[dc] <- d$hflw[j]
          daily$limb[dc] <- as.character(d$limb[j])
        }
      }
    }
    floods <- floods[apply(floods[, -1], 1, function(x) !all(x ==
                                                               0)), ]
    assign(paste("floods", round(Q[i]), sep = ""), floods)
    f_data[[i]] <- floods
    f_filenames[i] <- paste0("floodid_events", round(Q[i]))
    if (!is.null(outdir)) {
      utils::write.csv(floods, file = paste0(outdir, "floods",
                                      round(Q[i]), ".csv"), row.names = FALSE)
    }
    daily <- stats::na.omit(daily)
    assign(paste("daily", round(Q[i]), sep = ""), daily)
    daily_data[[i]] <- daily
    if (!is.null(outdir)) {
      utils::write.csv(daily, file = paste0(outdir, "floodid_daily",
                                     round(Q[i]), ".csv"), row.names = FALSE)
    }
  }
  output <- list(f_data, f_filenames, daily_data)
  return(output)
}
