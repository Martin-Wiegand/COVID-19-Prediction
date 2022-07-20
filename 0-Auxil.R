# Load packages
library("tidyverse")
library("lubridate")

read.epic.csv <- function(file, colClasses){
  read.csv(file = file.path(raw_path, file),
           header = T,
           colClasses = colClasses,
           stringsAsFactors=FALSE) %>%
    as_tibble
}

setClass("iso8601space")
setAs("character",
      "iso8601space",
      function(from){
        as.POSIXct(from, format="%Y-%m-%d %H:%M:%S", tz = "Europe/London")
      })

get.syst <- function(x){
  as.numeric(do.call(rbind,strsplit(x,split = "/"))[,1])
}

get.dias <- function(x){
  as.numeric(do.call(rbind,strsplit(x,split = "/"))[,2])
}

convert.to.celcius <- function(x,threshold = 50){
  return(ifelse(x >= threshold,
                (x-32)*5/9,
                x))
}

colorGenerator <- function(n){
  color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  sample(color, n) %>% return
}

# Convert < > than characters in numeric values
convert.numeric <- function(x){
  x %>% str_remove_all(pattern = "<") %>%
    str_remove_all(pattern = ">") %>%
    str_remove_all(pattern = " ") %>%
    as.numeric
  # lapply(X = strsplit(x,split = c(">","<","<")),function(y) tail(y,1)) %>% unlist %>% as.numeric
}

ICU_check <- function(IN,DEPT,iw = ICU_wards,cv = conv_times,rcv = re_conv_times,pd = pull_date){

  if(length(IN) == 1) NEXT = pd
  if(length(IN) > 1) NEXT = c(tail(IN,-1),pd)

  # print(NEXT)

  return(DEPT == iw[1] |
           DEPT == iw[2] |
           (DEPT == iw[3] & IN >= cv[3]) |
           (DEPT == iw[3] & NEXT >= cv[3]) |
           (DEPT == iw[4] & IN >= cv[4] & IN <= rcv[4]) |
           (DEPT == iw[4] & NEXT >= cv[4] & NEXT <= rcv[4]) |
           (DEPT == iw[5] & IN >= cv[5] & IN <= rcv[5]) |
           (DEPT == iw[5] & NEXT >= cv[5] & NEXT <= rcv[5]) |
           (DEPT == iw[6] & IN >= cv[6] & IN <= rcv[6]) |
           (DEPT == iw[6] & NEXT >= cv[6] & NEXT <= rcv[6]))

}




Hosp_Match <- function(adt_data_id,
                       data,
                       buffer.in = hours(0),
                       buffer.out = hours(0),
                       default_out_date = pull_date,
                       add_time_0 = F){

  ID <- adt_data_id$STUDY_SUBJECT_DIGEST %>% unique
  STAY <- adt_data_id$STAY %>% unique
  IN <- adt_data_id %>% filter(EVENT_TYPE_C == 0) %>% pull(IN_DTTM)
  ICU <- adt_data_id %>% pull(ICU)


  if(any(adt_data_id$EVENT_TYPE_C == 2)){
    OUT <- adt_data_id %>% filter(EVENT_TYPE_C == 2) %>% pull(IN_DTTM)
    STATUS <- "CONCLUDED"
  } else{
    # OUT <- adt_data_id %>% pull(IN_DTTM) %>% max
    OUT <- default_out_date
    STATUS <- "ACTIVE"
  }

  if(add_time_0){
    TIME_0 <- adt_data_id %>% pull(TIME_0) %>% unique
    
    return_data <- data %>%
      filter(STUDY_SUBJECT_DIGEST == ID) %>%
      filter(IN - buffer.in <= DATETIME) %>%
      filter(DATETIME <= OUT + buffer.out) %>%
      mutate(STAY = STAY) %>%
      mutate(STATUS = STATUS) %>%
      mutate(IN_DTTM = IN) %>%
      mutate(OUT_DTTM = OUT) %>%
      mutate(ICU_EVER = any(ICU)) %>%
      mutate(TIME_0 = TIME_0) %>%
      select(STUDY_SUBJECT_DIGEST,STAY,STATUS,IN_DTTM,TIME_0,OUT_DTTM,ICU_EVER,DATETIME,everything()) %>%
      arrange(DATETIME)
    
  }else{
    return_data <- data %>%
      filter(STUDY_SUBJECT_DIGEST == ID) %>%
      filter(IN - buffer.in <= DATETIME) %>%
      filter(DATETIME <= OUT + buffer.out) %>%
      mutate(STAY = STAY) %>%
      mutate(STATUS = STATUS) %>%
      mutate(IN_DTTM = IN) %>%
      mutate(OUT_DTTM = OUT) %>%
      mutate(ICU_EVER = any(ICU)) %>%
      select(STUDY_SUBJECT_DIGEST,STAY,STATUS,IN_DTTM,OUT_DTTM,ICU_EVER,DATETIME,everything()) %>%
      arrange(DATETIME)
    
  }

  return(return_data)
}

Hosp_Assign <- function(ID,time,adm,in.buffer,out.buffer,pull_date){
  
  # max.time = ifelse(is.na(),,)
  
  return_stays = adm %>% filter(STUDY_SUBJECT_DIGEST == ID) %>%
    mutate(max.time = ifelse(is.na(HOSP_DISCH_TIME),
                             as.POSIXct(pull_date),
                             as.POSIXct(HOSP_DISCH_TIME)))
  
  if(nrow(return_stays) != 0){
    return_stays = return_stays %>%
      filter(IN_DTTM - in.buffer <= time & 
               time <= max.time + out.buffer) %>%
      pull(STAY)
  }
  
  
  
  ifelse(length(return_stays) != 0,return_stays,NA)%>%
    return
  
}

ICU_stay <- function(ICU){
  counter <- 1
  ICUSTAY <- numeric(0)
  n <- length(ICU)

  if(length(ICU) > 1){
    for(i in 1:(n-1)){
      if(ICU[i]){ ICUSTAY[i] <- counter
      if(!ICU[i+1]) counter <- counter + 1
      }else{
        ICUSTAY[i] <- NA
      }
    }

    if(nth(ICU,-1)){ICUSTAY[n] <- counter
    }else{ICUSTAY[n] <- NA}

  }else{
    if(ICU) ICUSTAY <- 1
    else ICUSTAY <- NA
  }

  return(ICUSTAY)
}


ICU_discharge <- function(ICU,IN){
  discharge_time <- as.POSIXct(character(0))
  ICUSTAY <- numeric(0)
  n <- length(ICU)

  if(n > 1){
    for(i in 1:(n-1)){
      if(ICU[i] & !ICU[i+1]) discharge_time <- c(discharge_time,IN[i+1])
    }
  }


  return(discharge_time)
}



Ventilation_Stay <- function(data,vent.buffer = 24){
  # counter <- 1
  # ventstay <- numeric(0)
  time <- data$DATETIME
  mode <- data$mechanical_ventilation
  # n <- nrow(data)

  pos.data <- data %>% filter(mechanical_ventilation)
  pos.time <- pos.data$DATETIME
  pos.diff <- c(0,difftime(tail(pos.time,-1),head(pos.time,-1),unit = "hours"))

  ventstay <- cumsum(ifelse(pos.diff <= vent.buffer,
                      0,
                      1)) + 1

  data <- data %>% mutate(PERIOD = NA)

  data$PERIOD[which(data$mechanical_ventilation)] <- ventstay

  return(data)
}

Medication_Stay <- function(data,med.buffer = 12){
  # counter <- 1
  # ventstay <- numeric(0)
  data <- data %>% arrange(TimeAdministered)
  time <- data$TimeAdministered
  stopped <- grepl("Stopped|Cancelled",data$MARAction,ignore.case = T)
  n <- nrow(data)

  duration <- numeric(1)

  if(n > 1){
    for(i in 1:(n-1)){
      dt <- difftime(time[i+1],time[i],unit = "hours") %>% as.numeric
      if(!stopped[i]) duration <- duration + min(dt,med.buffer)
    }
    return(duration)
  }
  if(n == 1 & !stopped[1]) return(med.buffer)
  if(n == 1 & stopped[1]) return(0)
  if(n == 0) return(0)


}

pullRDS <- function(x,RDSpath) assign(paste0(x,"_data"),value = readRDS(file = paste0(RDSpath,x,".rds")))

# Automated processing of "vanilla" tests

tests_nonnumeric <- function(.data){
  value <- .data$value
  value_numeric <- suppressWarnings({
    as.numeric(value)
  })
  value_is_nonnumeric <- is.na(value_numeric)
  value[value_is_nonnumeric]
}

tests_find_possibly_missed <- function(.data,
                                       names_cuh,
                                       names_external = NA,
                                       search_pattern,
                                       search_exclude = NA,
                                       search_exclude_group = NA){
  if (!all(is.na(search_exclude))){
    search_exclude <- c(names_cuh, search_exclude)
  } else {
    search_exclude <- names_cuh
  }
  if (!all(is.na(names_external))){
    search_exclude <- c(search_exclude, names_external)
  }
  if (all(is.na(search_exclude_group))){
    search_exclude_group <- c()
  }
  # TestName or test_name
  search_pattern <- paste0(search_pattern, collapse = "|")
  .data %>%
    filter(str_detect(TestName, regex(search_pattern, ignore_case = TRUE)) &
             (!TestName %in% c(names_cuh, search_exclude)) &
             (!TestGroupName %in% search_exclude_group)) %>%
    group_by(TestName) %>%
    tally
}

tests_pull <- function(.data,
                       symbol,
                       title,
                       names_cuh,
                       names_external = NA,
                       search_pattern,
                       search_exclude = NA,
                       search_exclude_group = NA,
                       silently_exclude_na_when = FALSE,
                       silently_exclude_when = FALSE,
                       censoring_fn = case_when(TRUE ~ NA_character_),
                       nonnumeric_filter = TRUE,
                       nonnumeric_fn = case_when(TRUE ~ value_numeric),
                       unit_rescale_fn = case_when(TRUE ~ value),
                       unit_relabel_fn = case_when(TRUE ~ unit),
                       expect,
                       range_mainly_low,
                       range_mainly_high,
                       range_discard_below = NA,
                       range_discard_above = NA){
  censoring_fn <- enquo(censoring_fn)
  nonnumeric_fn <- enquo(nonnumeric_fn)
  silently_exclude_na_when <- enquo(silently_exclude_na_when)
  silently_exclude_when <- enquo(silently_exclude_when)
  unit_rescale_fn <- enquo(unit_rescale_fn)
  unit_relabel_fn <- enquo(unit_relabel_fn)
  expect <- enquo(expect)
  
  out <- .data %>%
    tests_rename %>%
    filter(name %in% names_cuh)
  
  nrow_data_original <- nrow(out)
  out <- out %>%
    distinct
  nrow_data_distinct <- nrow(out)
  if (nrow_data_original != nrow_data_distinct){
    message("Discarding ",
            nrow_data_original - nrow_data_distinct,
            " duplicate rows")
  }
  
  out <- out %>%
    filter(!(is.na(value) & !!silently_exclude_na_when)) %>%
    filter(!(!!silently_exclude_when))
  
  out <- out %>%
    group_by(name) %>%
    mutate(value_numeric = suppressWarnings({
      as.numeric(value)
    }),
    censoring_type = !!censoring_fn,
    value = !!nonnumeric_fn) %>%
    select(-value_numeric) %>%
    ungroup
  
  nonnumeric <- tests_nonnumeric(out)
  if (length(nonnumeric) > 0){
    stop("*** Non-numeric value found ***\n",
         print(table(nonnumeric, useNA = "ifany")),
         "\n")
  }
  
  out <- out %>%
    mutate(value = !!unit_rescale_fn,
           unit = !!unit_relabel_fn)
  
  unexpected <- out %>%
    filter(!(!! expect))
  if (nrow(unexpected)){
    unexpected <- unexpected %>%
      select(group, name, value, reference_low, reference_high, unit)
    stop("unexpected cases\n", print(unexpected))
  }
  
  out <- out %>%
    tests_unrename
  
  # This probably needs more thought
  out <- out %>%
    mutate(COLLECTED_DATETIME =
             case_when(!is.na(COLLECTED_DATETIME) ~ COLLECTED_DATETIME,
                       is.na(COLLECTED_DATETIME) &
                         !is.na(ORDERED_DATETIME) ~ ORDERED_DATETIME,
                       is.na(COLLECTED_DATETIME) &
                         is.na(ORDERED_DATETIME) &
                         !is.na(ResultDate) ~ ResultDate)) %>%
    filter(!is.na(COLLECTED_DATETIME))
  
  if(plot.vals){
    png(png_file(paste0("Cohort/", symbol,"_",Sys.Date())),
        width = 30,
        height = 20,
        units = "cm",
        res = 200)
    histogram_plot <- ggplot(out, aes(x = ResultValue)) +
      geom_histogram(bins = 30) +
      geom_vline(xintercept = range_mainly_low, colour = "green") +
      geom_vline(xintercept = range_mainly_high, colour = "green") +
      ggtitle(paste(title, ": histogram of all values"))
    if (!is.na(range_discard_below)){
      histogram_plot <- histogram_plot +
        geom_vline(xintercept = range_discard_below, colour = "red")
    }
    if (!is.na(range_discard_above)){
      histogram_plot <- histogram_plot +
        geom_vline(xintercept = range_discard_above, colour = "red")
    }
    print(histogram_plot)
    dev.off()
    
    if (!is.na(range_mainly_low)){
      png(png_file(paste0("Cohort/", symbol,"_low_",Sys.Date())),
          width = 30,
          height = 20,
          units = "cm",
          res = 200)
      out_range_low <- out %>%
        filter(ResultValue < range_mainly_low)
      
      histogram_low_plot <- ggplot(out_range_low,
                                   aes(x = ResultValue,
                                       fill = STUDY_SUBJECT_DIGEST)) +
        geom_histogram(boundary = range_mainly_low,
                       bins = 30) +
        geom_vline(xintercept = range_mainly_low, colour = "green") +
        guides(fill = FALSE) +
        ggtitle(paste(title, ": histogram of values below ", range_mainly_low))
      if (!is.na(range_discard_below)){
        histogram_low_plot <- histogram_low_plot +
          geom_vline(xintercept = range_discard_below, colour = "red")
      }
      print(histogram_low_plot)
      dev.off()
    }
    
    if (!is.na(range_mainly_high)){
      png(png_file(paste0("Cohort/", symbol,"_high_",Sys.Date())),
          width = 30,
          height = 20,
          units = "cm",
          res = 200)
      out_range_high <- out %>%
        filter(ResultValue > range_mainly_high)
      histogram_high_plot <- ggplot(out_range_high,
                                    aes(x = ResultValue,
                                        fill = STUDY_SUBJECT_DIGEST)) +
        geom_histogram(boundary = range_mainly_high,
                       bins = 30) +
        geom_vline(xintercept = range_mainly_high, colour = "green") +
        guides(fill = FALSE) +
        ggtitle(paste(title, ": histogram of values above ", range_mainly_high))
      if (!is.na(range_discard_above)){
        histogram_high_plot <- histogram_high_plot +
          geom_vline(xintercept = range_discard_above, colour = "red")
      }
      print(histogram_high_plot)
      dev.off()
    }
  }
  
  if (!is.na(range_discard_below)){
    discard <- out %>%
      filter(ResultValue < range_discard_below)
    message("Discarding ", nrow(discard), " rows below ", range_discard_below)
    out <- out %>%
      filter(ResultValue > range_discard_below)
  }
  
  if (!is.na(range_discard_above)){
    discard <- out %>%
      filter(ResultValue > range_discard_above)
    message("Discarding ", nrow(discard), " rows above ", range_discard_above)
    
    out <- out %>%
      filter(ResultValue < range_discard_above)
  }
  
  if(save.vals) saveRDS(out, rds_file(symbol))
  
  out
  #   relocate(test_name, .after = "received_date") %>%
  #   add_column(test = symbol, .after = 1) %>%
  #   arrange()
}

tests_rename <- function(.data){
  .data %>%
    rename(id = STUDY_SUBJECT_DIGEST,
           name = TestName,
           value = ResultValue,
           ordered_date = ORDERED_DATETIME,
           collected_date = COLLECTED_DATETIME,
           received_date = RECEIVED_DATETIME,
           result_date = ResultDate,
           reference_low = ReferenceLow,
           reference_high = ReferenceHigh,
           unit = ResultUnit,
           method = Method,
           group = TestGroupName,
           order_id = OrderProcId)
}

tests_unrename <- function(.data){
  .data %>%
    rename(STUDY_SUBJECT_DIGEST = id,
           TestName = name,
           ResultValue = value,
           ORDERED_DATETIME = ordered_date,
           COLLECTED_DATETIME = collected_date,
           RECEIVED_DATETIME = received_date,
           ResultDate = result_date,
           ReferenceLow = reference_low,
           ReferenceHigh = reference_high,
           ResultUnit = unit,
           Method = method,
           TestGroupName = group,
           OrderProcId = order_id)
}


# Get n distinguishable colours (rainbow(n) sometimes too close together)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Boxplot extension to leave out outliers from computing & plotting range

calc_boxplot_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

#' Report all values that do not convert to numeric without error
#'
#' Report the values of a vector that become \code{NA} when converted to a
#' numeric.
#'
#' @param x A (typically character) vector, containing something that is
#' (typically) numbers
#' @return A character vector (of length 1) describing the non-numeric values,
#' separated by "and". If everything in \code{x} can be converted to a number,
#' then \code{"All numeric"} is returned.
report_non_numeric_values <- function(x){
  non_numerics <- x[is.na(as.numeric(x))]
  non_numerics_except_na <- non_numerics[!is.na(non_numerics)]
  if (length(non_numerics_except_na) == 0){
    "All numeric"
  } else {
    paste(unique(non_numerics_except_na), collapse = " and ")
  }
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

rmcl = function(cor_mat, threshold) {
  cor_mat = abs(cor_mat)
  stopifnot(sum(cor_mat[is.na(cor_mat)]) == 0) 
  for (i in 1:(nrow(cor_mat) - 1)) {
    for (j in (i+1):ncol(cor_mat)) {
      if(cor_mat[i, j] > threshold) {
        cor_mat[i, ] = rep(NA, ncol(cor_mat))
        break
      }
    }
  }
  idx = which(!is.na(cor_mat[, 1]))
  cor_mat[idx, idx]
}



integrate.xy <- function (x, fx, a, b, use.spline = TRUE, xtol = 2e-08) 
{
  if (is.list(x)) {
    fx <- x$y
    x <- x$x
    if (length(x) == 0) 
      stop("list 'x' has no valid $x component")
  }
  if ((n <- length(x)) != length(fx)) 
    stop("'fx' must have same length as 'x'")
  if (is.unsorted(x)) {
    i <- sort.list(x)
    x <- x[i]
    fx <- fx[i]
  }
  if (any(i <- duplicated(x))) {
    n <- length(x <- x[!i])
    fx <- fx[!i]
  }
  if (any(diff(x) == 0)) 
    stop("bug in 'duplicated()' killed me: have still multiple x[]!")
  if (missing(a)) 
    a <- x[1]
  else if (any(a < x[1])) 
    stop("'a' must NOT be smaller than min(x)")
  if (missing(b)) 
    b <- x[n]
  else if (any(b > x[n])) 
    stop("'b' must NOT be larger  than max(x)")
  if (length(a) != 1 && length(b) != 1 && length(a) != length(b)) 
    stop("'a' and 'b' must have length 1 or same length !")
  else {
    k <- max(length(a), length(b))
    if (any(b < a)) 
      stop("'b' must be elementwise >= 'a'")
  }
  if (use.spline) {
    xy <- spline(x, fx, n = max(1024, 3 * n))
    if (xy$x[length(xy$x)] < x[n]) {
      if (TRUE) 
        cat("working around spline(.) BUG --- hmm, really?\n\n")
      xy$x <- c(xy$x, x[n])
      xy$y <- c(xy$y, fx[n])
    }
    x <- xy$x
    fx <- xy$y
    n <- length(x)
  }
  ab <- unique(c(a, b))
  BB <- abs(outer(x, ab, "-")) < (xtol * max(b - a))
  if (any(j <- 0 == colSums(BB))) {
    y <- approx(x, fx, xout = ab[j])$y
    x <- c(ab[j], x)
    i <- sort.list(x)
    x <- x[i]
    fx <- c(y, fx)[i]
    n <- length(x)
  }
  dig0 <- floor(-log10(xtol))
  f.match <- function(x, table, dig) match(signif(x, dig), 
                                           signif(table, dig))
  d <- dig0
  while (anyNA(ai <- f.match(a, x, d))) d <- d - 1/8
  ai <- rep_len(ai, k)
  d <- dig0
  while (anyNA(bi <- f.match(b, x, d))) d <- d - 1/8
  bi <- rep_len(bi, k)
  dfx <- fx[-c(1, n)] * diff(x, lag = 2)
  r <- numeric(k)
  for (i in 1:k) {
    a <- ai[i]
    b <- bi[i]
    r[i] <- (x[a + 1] - x[a]) * fx[a] + (x[b] - x[b - 1]) * 
      fx[b] + sum(dfx[seq(a, length = max(0, b - a - 1))])
  }
  r/2
}
