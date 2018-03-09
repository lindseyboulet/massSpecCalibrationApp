cleanSpecData <- function(df){
  y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
         "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
         "rmarkdown", "lubridate", "chron")
  for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
  if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")
  }
  library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
  }
  df <- df[3:nrow(df), c(1:7,9)]
  colnames(df) <- c("julian_day", "o2", "co2", "n2", "ar", "h2o", "total", "comments")
  df$julian_day <- as.numeric(as.character(df$julian_day))
  start <- ymd_hms("2016-12-31 00:00:00")
  df <- mutate(df, date_time = start + julian_day *3600 *24)
  df$date_time <- as.character(df$date_time)
  time <- unlist(strsplit(df$date_time, split =" "))
  df <- cbind(df, "time" = time[seq(2, length(time), 2)])
  df$seconds <- as.numeric(times(df$time))*24*60*60
  df$day <- substr(df$date_time, 9, 10)
  df$day <- as.numeric(df$day)
  df$day <- df$day - df$day[1]
  df$seconds[df$day>0] <- df$seconds[df$day>0] + 86393
  df$seconds[df$day>1] <- df$seconds[df$day>1] + 86393
  df$seconds[df$day>2] <- df$seconds[df$day>2] + 86393
  # df$seconds[df$day=="01"] <- df$seconds[df$day=="01"] + 172784
  df$seconds <- df$seconds-df$seconds[1]
  df$o2 <- as.numeric(as.character(df$o2))
  df$co2 <- as.numeric(as.character(df$co2))
  df$n2 <- as.numeric(as.character(df$n2))
  df$ar <- as.numeric(as.character(df$ar))
  df$comments <- as.character(df$comments)
  df
}