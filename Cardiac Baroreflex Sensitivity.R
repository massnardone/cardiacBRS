#Load Dependencies - packages being used-----------------
packages = c("tidyverse",
             "readxl",
             "openxlsx",
             "signal",
             "gsignal",
             "broom",
             "DescTools",
             "remotes",
             "shiny",
             "shinyBS",
             "shinythemes")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Package Dependency - Install plotly from github
if (!require("plotly", character.only = TRUE)) {
  remotes::install_github("ropensci/plotly")
  library("plotly", character.only = TRUE)
}

source("./Scripts/helper functions.R")
options(shiny.maxRequestSize = 800 * 1024^2,
        scipen = 999,
        shiny.launch.browser = .rs.invokeShinyWindowExternal)
reactlog::reactlog_enable()

#UI----
ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  ##Application title----
  titlePanel("Baroreflex Sensitivity"),
  
  ##Sidebar panel---- 
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 ###Inputs----
                 #Select Input
                 fileInput(inputId = "file", label = "Upload xlsx File", 
                           accept = ".xlsx", width = "80%"),
                 
                 #Horizontal line
                 tags$hr(),
                 
                 numericInput(inputId = "SBP_diff", label = "SBP Difference", 
                              value = 1, min = 0, max = 100),
                 bsTooltip("SBP_diff",
                           "Minimal threshold for SBP differences. Default: 1 mmHg",
                           "right"),
                 
                 numericInput(inputId = "RR_diff", label = "RR Difference", 
                              value = 5, min = 0, max = 100, step = 0.01),
                 bsTooltip("RR_diff",
                           "Minimal threshold for RR differences. Default: 5 msec",
                           "right"),
                 
                 #Horizontal line
                 tags$hr(),
                 
                 selectInput(inputId = "lag_lead", label = "Lag, Lead, or None",
                             choices = c("lag", "lead", "None"),
                             selected = "None"),
                 bsTooltip("lag_lead",
                           "Allows the user to determine lagging effects of SBP. 
                           Lag: Advances the SBP forward in time. 
                           Lead: Advanes the SBP backward in time. 
                           Default: No lag (None)",
                           "right"),
                 
                 numericInput(inputId = "cycles", label = "Number of Lead or Lag Cycles",
                              value = 0, min = 0, max = 100, step = 1),
                 bsTooltip("cycles",
                           "If Lag or Lead is selected, Select number of cardiac cycles to advance data.
                           Default: 0",
                           "right"),
                 
                 #Horizontal line
                 tags$hr(),
                 
                 numericInput(inputId = "vlf", label = "VLF value", value = 0.04,
                              min = 0, max = 100, step = 0.01),
                 bsTooltip("vlf",
                           "Boundary for Very low frequency (VLF)",
                           "right"),
                 
                 numericInput(inputId = "lf", label = "LF value", value = 0.15,
                              min = 0, max = 100, step = 0.01),
                 bsTooltip("lf",
                           "Boundary for Very low frequency (VLF)",
                           "right"),
                 
                 numericInput(inputId = "hf", label = "HF value", value = 0.50,
                              min = 0, max = 100, step = 0.01),
                 bsTooltip("hf",
                           "Boundary for High frequency (HF)",
                           "right"),
                 
                 #Horizontal line
                 tags$hr(),
                 
                 ###Download Button----
                 downloadButton(outputId = "download"),
                 bsTooltip("download",
                           "Download Analyzed Data!",
                           "right")
    ),
    
    ##Main Panel----
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Cardiac Sequence",
                           
                           ###Output: Sequence Table----
                           tags$h4("Sequence Table"),
                           
                           dataTableOutput("Sequence"),
                           
                           ###Output: Combined Sequences Table and Mean----
                           tags$h4("Table of All the Sequences"),
                           
                           dataTableOutput("Summary_combined"),
                           
                           ###Output: Means and Lengths Table----
                           tags$h4("Slope Means and Amount of Sequences Table"),
                           
                           dataTableOutput("means_and_lengths"),
                           
                           ###Output: Sequence plots----
                           plotlyOutput("Seq_plot1", height = "1000px"),
                           
                           plotlyOutput("Seq_plot2", height = "1000px")
                           
                  ), #End tab panel 1
                  
                  tabPanel("Cardiac Transfer Function",
                           
                           ###Output: Transfer function plot----
                           plotlyOutput("Transfer_plot1", height = "1000px"),
                           
                           ###Output: Transfer function summary----
                           dataTableOutput("Transfer_summary")
                           
                           )
                  
      )#end tabset panel
    )#end main panel
  )#end sidebar layout
)#end UI

#Server----
server <- function(input, output) {
  
  ##Initialize reactive values----
  values <- reactiveValues(
    df = NULL,
    shifted = NULL,
    df_interp = NULL,
    Seq_table = NULL,
    Summary_up = NULL,
    Summary_down = NULL,
    Summary_combined = NULL,
    slope_up = NULL,
    slope_down = NULL,
    slope_combined = NULL,
    TF = NULL,
    means_and_lengths = NULL
  )
  
  ##Listen ----
  loadlisten <- reactive({
    list(input$file$datapath)
  })
  
  ##Load data ----
  observeEvent(loadlisten(), {
    
    req(input$file$datapath)
    
    tryCatch( {
      df <- read.xlsx(input$file$datapath)
      
      values$df <- df
      
    }, error = function(e) {
      stop(safeError(e))
    }
    )
  })
  
  ##Sequence Table----
  observeEvent(values$df, {
      
    observeEvent(input$lag_lead, {
      
      observeEvent(input$cycles, {
        
        values$shifted <- SBP_shift(values$df, input$lag_lead, input$cycles)
        
      })
          
    })
        
    observeEvent(input$SBP_diff | input$RR_diff, {
          
      output$Sequence <- renderDataTable({
          
        SBP_diff <- as.numeric(input$SBP_diff)
        RR_diff <- as.numeric(input$RR_diff)
            
        values$Seq_table <- cBRS(values$shifted, RR_diff, SBP_diff)
            
        values$df_interp <- interpolate(values$df)
        
            
        return(values$Seq_table)
              
      })
        
    })

    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  observeEvent(values$Seq_table, {
    
    ##Sequence Models and Summaries----
    output$Summary_combined <- renderDataTable({
      
      ###Up Model and Summary----
      model_UP <- model_UP(values$Seq_table)
      values$Summary_UP <- Summary_UP(model_UP)
      
      
      ###Down Model and Summary----
      model_DOWN <- model_DOWN(values$Seq_table)
      values$Summary_DOWN <- Summary_DOWN(model_DOWN)
      
      
      ###Combined Model and Summary----
      model_combined <- rbind(model_UP, model_DOWN)
      values$Summary <- Summary_Combined(model_combined)
      
      
      ##Means----
      ###Sequence up----
      values$slope_up <- mean(values$Summary_UP$Slope, na.rm = TRUE)
      
      ###Sequence down----
      values$slope_down <- mean(values$Summary_DOWN$Slope, na.rm = TRUE)
      
      ###Combined Sequences----
      values$slope_combined <- mean(values$Summary$Slope, na.rm = TRUE)
      
      
      return(values$Summary)
      
    })
    
    ##Plots----
    output$Seq_plot1 <- renderPlotly({
      
      slope_plot(values$Seq_table)
      
    })
    
    output$Seq_plot2 <- renderPlotly({
      
      seq_plot(values$Seq_table)
      
    })
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  

  ##Creating the Means Table----
  observeEvent(values$slope_combined, {
    
    output$means_and_lengths <- renderDataTable({
      
      values$means_and_lengths <- cbind(means(values$slope_up, values$slope_down,
                                              values$slope_combined),
                                        lengths(values$Summary_UP$Sequence,
                                                values$Summary_DOWN$Sequence,
                                                values$Summary$Sequence))
      
      return(values$means_and_lengths)
      
    })
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  ##Transfer function plots----
  observeEvent(values$df_interp, {
    
    observeEvent(input$vlf | input$lf |input$hf, {
      
      values$TF <- transfer_fn(values$df_interp, input$hf)
      
      output$Transfer_plot1 <- renderPlotly({
        
        #Input
        Fig1A <- plot_ly() %>%
          add_lines(data = values$df_interp,
                    x = ~Time,
                    y = ~SBP,
                    text = ~"Input Time Series",
                    name = NULL,
                    line = list(color = "Red", width = 1.5),
                    hovertemplate = paste(
                      "<b>%{text} </b><br><br>",
                      "Time: %{x}<br>",
                      "SBP: %{y}<br>"))
        
        Fig1B <- plot_ly() %>%
          add_lines(data = values$TF,
                    x = ~Frequency,
                    y = ~Input_Power,
                    text = ~"Input Spectral Density",
                    name = "",
                    line = list(color = "Red", width = 1.5),
                    hovertemplate = paste(
                      "<b>%{text} </b><br><br>",
                      "Frequency: %{x}<br>",
                      "Power: %{y}<br>")) %>%
          layout(shapes = list(vline(input$vlf), vline(input$lf)))
        
        #Output
        Fig1C <- plot_ly() %>%
          add_lines(data = values$df_interp,
                    x = ~Time,
                    y = ~RR,
                    text = ~"Output Time Series",
                    name = "",
                    line = list(color = "Blue", width = 1.5),
                    hovertemplate = paste(
                      "<b>%{text} </b><br><br>",
                      "Time: %{x}<br>",
                      "RR: %{y}<br>"))
        
        Fig1D <- plot_ly() %>%
          add_lines(data = values$TF,
                    x = ~Frequency,
                    y = ~Output_Power,
                    text = ~"Output Spectral Density",
                    name = "",
                    line = list(color = "Blue", width = 1.5),
                    hovertemplate = paste(
                      "<b>%{text} </b><br><br>",
                      "Frequency: %{x}<br>",
                      "Power: %{y}<br>")) %>%
          layout(shapes = list(vline(input$vlf), vline(input$lf)))
        
        #Transfer Function
        Fig1E <- plot_ly() %>%
          add_lines(data = values$TF,
                    x = ~Frequency,
                    y = ~Gain,
                    text = ~"Transfer Function Gain",
                    name = "",
                    line = list(color = "Black", width = 1.5),
                    hovertemplate = paste(
                      "<b>%{text} </b><br><br>",
                      "Frequency: %{x}<br>",
                      "Gain: %{y}<br>")) %>%
          layout(shapes = list(vline(input$vlf), vline(input$lf)))
        
        Fig1F <- plot_ly() %>%
          add_lines(data = values$TF,
                    x = ~Frequency,
                    y = ~Coherence,
                    text = ~"Transfer Function Coherence",
                    name = "",
                    line = list(color = "Black", width = 1.5),
                    hovertemplate = paste(
                      "<b>%{text} </b><br><br>",
                      "Frequency: %{x}<br>",
                      "Coherence: %{y}<br>")) %>%
          layout(shapes = list(vline(input$vlf), vline(input$lf)))
        
        subplot(Fig1A, Fig1B, Fig1C, Fig1D, Fig1E, Fig1F,
                nrows = 3, heights = c(0.33, 0.33, 0.33), widths = c(0.5, 0.5)) %>% 
          layout(showlegend = FALSE)
        
      })
      
    })

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  ##Transfer function summary----
  observeEvent(values$TF, {

    observeEvent(input$vlf | input$lf |input$hf, {
      
      output$Transfer_summary <- renderDataTable({
        
        values$TF_summary <- TF_Summary(values$TF, input$vlf, input$lf, input$hf)
        
        return(values$TF_summary)
        
      })
      
    })
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  ##Download ----
  
  list_to_export <- reactive({
    list("Sequence Table" = values$Seq_table,
         "Summary COMBINED" = values$Summary,
         "Mean and Amount Table" = values$means_and_lengths,
         "Transfer Function Data" = values$TF,
         "Transfer Function Summary" = values$TF_summary)
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste(tools::file_path_sans_ext(input$file), "-processed.xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(list_to_export(), file, rowNames = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
