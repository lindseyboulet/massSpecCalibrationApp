cleanCalData <- function(df){
  stage <- df$stage[1]
  idCol <- which(colnames(df)=="dataID")
  if(length(idCol == 1)){id <- df$dataID[1]}
  df <- summarise_all(df, mean, na.rm = TRUE)
  if(stage == "N2"){
    df$o2Stan <- 0
    df$co2Stan <- 0
    df$n2Stan <- 100
  } else if(stage == "calGas"){
    df$o2Stan <- 12
    df$co2Stan <- 6
    df$n2Stan <- 82
  } else if(stage == "CO2"){
    df$o2Stan <- 0
    df$co2Stan <- 9
    df$n2Stan <- 91
  }else if(stage == "mixed"){
    df$o2Stan <- 5
    df$co2Stan <- 6
    df$n2Stan <- 89
  }else if(stage == "O2"){
    df$o2Stan <- 100
    df$co2Stan <- NA
    df$n2Stan <- NA
  }else if (stage == "CO2pure"){
    df$o2Stan <- NA
    df$co2Stan <- 100
    df$n2Stan <- NA
  } else if (stage == "standby"){
    df$o2Stan <- 0
    df$co2Stan <- 0
    df$n2Stan <- 0
  } else if (stage == "roomAir"){
    df$o2Stan <- 20.98
    df$co2Stan <- 0.04
    df$n2Stan <- 80
  }else if (stage == "compressed"){
    df$o2Stan <- 21.0
    df$co2Stan <- 0
    df$n2Stan <- 79
  }
  df$stage <- stage
  if(length(idCol == 1)){df$dataID <- id}
  df
}