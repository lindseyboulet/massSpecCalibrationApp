y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
       "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown", "lubridate", "chron", "ggthemes", "knitr")
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")
}
library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
}

fileNames <- list.files(here::here("data"))
source(here::here("functions", "cleanSpecDataFunc.R"))
source(here::here("functions", "cleanCalData.R"))


ui <- fluidPage(
  tags$style(HTML("
        .tabs-above > .nav > li[class=active] > a {
           background-color: #3e3e40;
           color: #3e3e40;
        }")),
  h2(strong("MIMS Analysis")),
  tabsetPanel(
    tabPanel("Raw Data",
       br(),
      fluidRow(
        column(4,
          selectInput(inputId = "calFile", 
                      label = strong("Raw File"),choices = fileNames)
        ),
        column(2,
               textInput(inputId = "xmin", label = strong("X-min"),
                         value = NULL)),
        column(2,
               textInput(inputId = "xmax", label = strong("X-max"),
                         value = NULL)),
        column(2,
               textInput(inputId = "ymin", label = strong("Y-min"),
                         value = NULL)),
        column(2,
               textInput(inputId = "ymax", label = strong("Y-max"),
                         value = NULL))
      ),
      fluidRow(
        column(2,
               textOutput('text1'),
               tags$head(tags$style("#text1{color:#e41a1c;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
               )
               )
             ),
        column(2,
               textOutput('text2'),
               tags$head(tags$style("#text2{color:#377eb8;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
               )
               )
        ),
        column(2,
               textOutput('text3'),
               tags$head(tags$style("#text3{color: #4daf4a;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
               )
               )
        )
        ),
      fluidRow(
        column(12,
          plotOutput(outputId = "distPlot",
            brush = brushOpts(
            id = "plot1_brush"))
        )
      ),
      br(),
      fluidRow(
        column(1, actionButton("exclude_toggle",
                                      "Select Points")
        ),
        column(1,
               actionButton("exclude_reset", "Reset")
        )
        ,
        column(1,
               actionButton("refreshData", "Refresh Data")
        ),
        column(2,
               selectInput(inputId = "gasStage", 
                           label = strong("Gas Stage"),
                           choices = list("100% N2" = "N2",
                                          "Cal Gas" = "calGas",
                                          "9% CO2, Bal N2" = "CO2",
                                          "Mixed Venous" = "mixed",
                                          "100% O2" = "O2",
                                          "100% CO2" = "CO2pure",
                                          "Stand By" = "standby",
                                          "Room Air" = "roomAir",
                                          "Compressed" = "compressed"))
        ),
        column(1,
               numericInput(inputId = "baro", label = strong("ATM Pressure"),
                         value = NULL)),
        column(1,
               radioButtons("atmUnits", "ATM Units", c("in Hg" = "in",
                                                       "mmHg" = "mm"))),
        column(1, 
               radioButtons("dataType", "Data Type", choices = c("Calibration" = "cal",
                                                                 "Experimental" = "exp"))
        ),
       
        conditionalPanel(condition = "input.dataType == 'exp'",
                column(1,
                    textInput("expTag", label = "Data Tag", value = NULL)
               )),
        column(1,
               actionButton("saveData", strong("Save Data"))
        )
        
        
        
      )
    ),
    tabPanel("Data Output",
      fluidRow(
       column(6,
              h3(strong("O2 Curve"))
       )
      ),
      fluidRow(
       column(6,
              plotOutput(outputId = "O2Cal")
       ),
       column(6,
              tableOutput(outputId = "O2table")
              )
      ),
      fluidRow(
        column(6,
               h3(strong("CO2 Curve"))
        )
      ),
      fluidRow(
        column(6,
               plotOutput(outputId = "CO2Cal")
        ),
        column(6,
               tableOutput(outputId = "CO2table")
        )
      ),
      fluidRow(
        column(6,
               h3(strong("N2 Curve"))
        )
      ),
      fluidRow(
        column(6,
               plotOutput(outputId = "N2Cal")
        ),
        column(6,
               tableOutput(outputId = "N2table")
        )
      ),
      br(),
      fluidRow(
        column(6,
               h3(strong("Experimental Data"))
        )
      ),
      fluidRow(
        column(6,
               p("(predicted values in mmHg)")
        )
      ),
      fluidRow(
        column(12,
               tableOutput(outputId = "resultsTable")
               )
      ),
      br(),
      fluidRow(
        column(3, downloadButton("renderMarkdown",
                               "Save Report")
        ),
        column(3, actionButton("refreshCurves",
                                 "Refresh")
        )
      )
      )
    )
  )

server <- function(input, output, session) {
  
  output$text1 <- renderText({ "Red: O2" })
  output$text2 <- renderText({ "Blue: CO2" })
  output$text3 <- renderText({ "Green: N2" })

  
  cleanedSpecData <- reactive({
    input$refreshData
    fileName <- paste(here::here("data", input$calFile))
    calData <-  read.table(fileName, fill = TRUE, skip = 21)
    rawDataList <- cleanSpecData(calData)
    rawDataList
  })

  specVals <- reactiveValues( 
    keepRows = rep(TRUE, isolate(nrow(cleanedSpecData()))))
  
  output$distPlot <- renderPlot({
    df <- cleanedSpecData()
    
    keep    <- df[specVals$keepRows, , drop = FALSE]
    exclude <- df[!specVals$keepRows, , drop = FALSE]
    
    ggplot(keep, aes(x = seconds)) +
        geom_point(aes(y=o2), color = "#e41a1c") +
        geom_point(aes(y=co2), color = "#377eb8") +
        geom_point(aes(y=n2), color = "#4daf4a") +
        geom_point(data = exclude, aes(y=o2), color = "#984ea3") +
        geom_point(data = exclude, aes(y=co2), color = "#984ea3") +
        geom_point(data = exclude, aes(y=n2), color = "#984ea3") +
        coord_cartesian() +
        annotate("text", y = 9e-07, x = df$seconds, label = df$comments,
                 angle = 90) +
        scale_x_continuous(limits = as.numeric(c(input$xmin, input$xmax))) + 
        scale_y_continuous(limits = as.numeric(c(input$ymin, input$ymax)))
  })

# Create data frame of selected data
  selectData <- reactive({
    avData <- cleanedSpecData()
    selectedAv <- avData[!specVals$keepRows, , drop = FALSE]
    selectedAv$stage <- input$gasStage
    selectedAv
  })
# Toggle points that are brushed, when button is clicked on plot 1
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(cleanedSpecData(), input$plot1_brush, allRows = TRUE)
    specVals$keepRows<- xor(specVals$keepRows, res$selected_)
  })
  
# Reset all points for plot 1
  observeEvent(input$exclude_reset, {
    specVals$keepRows <- rep(TRUE, nrow(cleanedSpecData()))
  })
  
# save selected data
  observeEvent(input$saveData, {
    if(input$dataType == "cal"){dirName <- "calOutput"}else{dirName <- "expOutput"}

    if(!dir.exists(here::here("output", dirName))){
      dir.create(here::here("output", dirName))
    }
    saveName <- here::here("output", dirName, 
                           paste(substr(input$calFile, 1, nchar(input$calFile)-4), "_", input$gasStage, ".csv",
                           sep = ""))
    i = 1
    if(file.exists(saveName)){
      saveName <- paste(substr(saveName,1, nchar(saveName)-4), "_", i,".csv", sep = "")
      i <- i +1
      while(file.exists(saveName)){
        saveName <- paste(substr(saveName,1, nchar(saveName)-6), "_", i,".csv", sep = "")
        i <- i+1
      }
   
    }
    df <- selectData()
    if(input$dataType == "exp" & !is.null(input$expTag)){df$dataID <- input$expTag}
    write.csv(df, saveName, row.names = FALSE)
  })
  
  # Load Cal Curve Data
  calCurveData <- reactive({
    input$saveData
    input$refreshCurves
    calFileName <- list.files(paste(here::here("output", "calOutput")), full.names = TRUE)
    calFiles <- lapply(calFileName, read.csv)
    calData <- ldply(calFiles, cleanCalData)
    # if(input$atmUnits == "in"){baro <- input$baro * #conversion factor
    # } else{
        baro <- input$baro
      # }
    calData$atm <- baro
    calData
  })
  
  # Load Exp Curve Data
  expCurveData <- reactive({
    input$saveData
    input$refreshCurves
    calFileName <- list.files(paste(here::here("output", "expOutput")), full.names = TRUE)
    if(length(calFileName)>0){
      calFiles <- lapply(calFileName, read.csv)
      expData <- ldply(calFiles, cleanCalData)
      # if(input$atmUnits == "in"){baro <- input$baro * #conversion factor
      # } else{
      baro <- input$baro
      # }
      expData$atm <- baro
    }else(expData <- data.frame())
      expData
  })
  
coefExt <- function(model){
  modelSum <- summary(model)
  coef <- data.frame(modelSum$coefficients)
  coef <- coef[,c(1,4)]
  inter <- 
    tab <- data.frame(matrix(nrow = 3, ncol = 2))
  tab[1:3, 1] <- c("Slope:", "p-Value:", "Intercept:")
  tab[1, 2] <- formatC(t(coef[2,1]), format = "e", digits = 2)
  tab[2, 2] <- round(t(coef[2,2]),2)
  tab[3, 2] <- round(t(coef[1,1]),1)
  colnames(tab) <- ""
  tab
}  
  
o2Curve <- reactive({
    df <- calCurveData()
    df2 <- expCurveData()
   p1 <-  ggplot(df, aes(x = o2Stan, y = o2)) +
      geom_point(color = "#e41a1c", size = 4) +
      geom_smooth(method = "lm", se = FALSE, color = "#984ea3") +
      geom_smooth(method = "lm", se = FALSE, color = "#984ea3") +
      labs(x = "Standard (%)", y = "Measured (%)")+
      theme_solarized(base_size = 16) 
   if(nrow(df2)>0){p1 <- p1 + geom_point(data = df2, color = "#ff7f00", size = 4, shape = 17)+
     geom_text(data = df2, aes(label=rownames(df2)),hjust=-0.5, vjust=-0.5)}
   p1
  })
output$O2Cal <- renderPlot({o2Curve()})

o2CurveTable <- reactive({
    df <- calCurveData()
    model <- lm(o2Stan ~ o2, data = df)
    coefExt(model)
  })
output$O2table <- renderTable({o2CurveTable()}, colnames = FALSE)
  
  co2Curve <- reactive({
    df <- calCurveData()
    df2 <- expCurveData()
    p1 <- ggplot(df, aes(x = co2Stan, y = co2)) +
      geom_point(color = "#377eb8", size = 4) +
      geom_smooth(method = "lm", se = FALSE, color = "#984ea3") +
      labs(x = "Standard (%)", y = "Measured (%)")+
      theme_solarized(base_size = 16) 
    if(nrow(df2)>0){p1 <- p1 + geom_point(data = df2, color = "#ff7f00", size = 4, shape = 17)+
      geom_text(data = df2, aes(label=rownames(df2)),hjust=-0.5, vjust=-0.5)}
    p1
  })
  output$CO2Cal <- renderPlot({co2Curve()})
  
  co2CurveTable <- reactive({
    df <- calCurveData()
    model <- lm(co2Stan ~ co2, data = df)
    coefExt(model)
  })
  output$CO2table <- renderTable({co2CurveTable()}, colnames = FALSE)
  
    n2Curve <- reactive({
    df <- calCurveData()
    df2 <- expCurveData()
    p1 <- ggplot(df, aes(x = n2Stan, y = n2)) +
      geom_point(color = "#4daf4a", size = 4) +
      geom_smooth(method = "lm", se = FALSE, color = "#984ea3") +
      labs(x = "Standard (%)", y = "Measured (%)")+
      theme_solarized(base_size = 16)
    if(nrow(df2)>0){p1 <- p1 + geom_point(data = df2, color = "#ff7f00", size = 4, shape = 17)+
      geom_text(data = df2, aes(label=rownames(df2)),hjust=-0.5, vjust=-0.5)}
    p1
  })
    output$N2Cal <- renderPlot({n2Curve()})
    
  n2CurveTable <- reactive({
    df <- calCurveData()
    model <- lm(n2Stan ~ n2, data = df)
    coefExt(model)
  })
  output$N2table <- renderTable({n2CurveTable()}, colnames = FALSE)
  
  expDataTable <- reactive({
    o2CurveTable <- o2CurveTable()
    co2CurveTable <- co2CurveTable()
    n2CurveTable <- n2CurveTable()
    expData <- expCurveData()
    baro <- expData$atm[1]
    expData <- expData[, c(11, 13, 14, 2:4, 15:17)]
    expData$predO2 <- (expData$o2 * as.numeric(o2CurveTable[1,2]) + as.numeric(o2CurveTable[3,2]))/100*(baro-49) 
    expData$predCO2 <- (expData$co2 * as.numeric(co2CurveTable[1,2]) + as.numeric(co2CurveTable[3,2]))/100*(baro-49) 
    expData$predN2 <- (expData$n2 * as.numeric(n2CurveTable[1,2]) + as.numeric(n2CurveTable[3,2]) )/100*(baro-49)
    expData$o2Stan <- expData$o2Stan/100*(baro-49)
    expData$co2Stan <- expData$co2Stan/100*(baro-49)
    expData$n2Stan <- expData$n2Stan/100*(baro-49) 
    expData[,-(4:6)]
  })
  output$resultsTable <- renderTable({expDataTable()}, rownames = TRUE)
  
  
  output$renderMarkdown <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(here::here("report.Rmd"), tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(o2Curve = o2Curve(), 
                     o2CurveTable = o2CurveTable(),
                     co2Curve = co2Curve(),
                     co2CurveTable = co2CurveTable(),
                     n2Curve = n2Curve(),
                     n2CurveTable = n2CurveTable(),
                     resultsTable = expDataTable())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
                        
      )
    }
  )
  
  

}

# create calibrated figures -----------------------------------------------


shinyApp(ui = ui, server = server)
