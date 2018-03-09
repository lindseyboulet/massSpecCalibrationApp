cleanCalData <- function(df){
  stage <- df$stage[1]
  df <- summarise_all(df, mean)
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
  }else if(stage == "test"){
    df$o2Stan <- 5
    df$co2Stan <- 6
    df$n2Stan <- 89
  }else if(stage == "O2"){
    df$o2Stan <- 100
    df$co2Stan <- 0
    df$n2Stan <- 0
  }
  df
}