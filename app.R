#------------------------------------------------------------------------------
# Dataset Summariser
# @purpose R Shiny app to quickly view all the default datasets available in 
#          an R installation. This helps me to learn RShiny and provides a 
#          useful tool at the same time.
# @author  Jesse M. Blum (jmb)(pszjmb1)
# @date    2017-09
#------------------------------------------------------------------------------

#--SETUP ----------------------------------------------------------------------
library(shiny)
myDatasets <- as.data.frame(data()["results"])

#-- UI ------------------------------------------------------------------------
ui <- fluidPage(
  # Defines the UI for dataset viewer app
  #
  # Returns:
  #   A UI defintion that can be passed to the shinyUI function.
  
  # App title ----
  titlePanel("Dataset Summariser"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h2("Controller"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = sort(myDatasets$results.Title)),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 5),
      
      # Input: Selector for variable to plot ----
      selectInput(inputId = "variable1Sel", label = "Variable 1:",
                  choices = c()),
        
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 2,
                  max = 25,
                  value = 6),
      
      h2("Output Plots"),
      
      h2("Univariate Plots"),
      plotOutput("univariate"),
      
      # Input: Selector for 2nd variable to plot----
      selectInput(inputId = "variable2Sel", label = "Variable 2:",
                  choices = c()),
      
      h2("Bivariate Plot"),
      plotOutput("bivariate")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h1("Output Summary"),
      
      # Output: HTML table with requested number of observations ----
      h2("Class"),
      verbatimTextOutput("class"),
      
      # Output: Verbatim text for data summary ----
      h2("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      h2("Str"),
      verbatimTextOutput("str"),
      
      # Output: HTML table with requested number of observations ----
      h2("Data Sample"),
      tableOutput("sample")
      
    )
  )
)

#--SERVER LOGIC ---------------------------------------------------------------
## Summarise and view selected default datasets
server <- function(input, output, session) {
  # Server-side logic of your application.
  # Args:
  #   input: Reactive input variables
  #   output: Reactive output variables
  #   session: An environment that can be used to access information and 
  #            functionality relating to the session.
  
  datasetInput <- reactive({
    # Primary reactive object which determines the current dataset of interest
    x <- as.character(myDatasets[myDatasets$results.Title==input$dataset,]$results.Item[1])
    x <- strsplit(x, split="[\\(|\\)]")[[1]]
    x <- gsub(" ", "", x[1])
    if(exists(data(list=x))){
      get(data(list=x))
    } else{
      x <- gsub(" ", "", x[2])
      get(data(list=x))
    }
  })
  
  observe({
    # Set the label and select items
    updateSelectInput(session, "variable1Sel",
                      choices = colnames(as.data.frame(datasetInput())))
    updateSelectInput(session, "variable2Sel",
                      choices = colnames(as.data.frame(datasetInput())))
  })
  
  output$summary <- renderPrint({
    # Generate a summary of the dataset ----
    dataset <- datasetInput()
    tempdataset <- dataset
    myClass <- class(tempdataset)
    if(myClass == "list") {
      tempdataset <- as.data.frame(datasetInput())
    }
    summary(tempdataset)
  })
  
  output$class <- renderPrint({
    # Set the dataset class ----
    dataset <- datasetInput()
    class(dataset)
  })
  
  output$str <- renderPrint({  
    # Set the dataset str ----
    dataset <- datasetInput()
    tempdataset <- dataset
    myClass <- class(tempdataset)
    if(myClass == "list") {
      tempdataset <- as.data.frame(datasetInput())
    }
    str(tempdataset)
  })
  
  output$sample <- renderTable({
    # Show the first "n" observations in a table----
    set.seed(42)
    
    dataset <- datasetInput()
    tempdataset <- dataset
    myClass <- class(tempdataset)
    if(myClass == "list") {
      tempdataset <- as.data.frame(datasetInput())
    }
    if("data.frame" %in% class(tempdataset)){
      tempdataset$row <- rownames(tempdataset)
      tempcolnames <-  colnames(tempdataset)
      tempdataset[sample(nrow(tempdataset), input$obs), c(length(tempcolnames),(1:length(tempcolnames)-1))]
    } else {
      sample(tempdataset,input$obs)
    }
  },striped=TRUE, bordered = TRUE)
  
  formulaText <- reactive({
    # Compute the formula text ----
    ## This is in a reactive expression since it is shared by the
    ##   output$caption and output$mpgPlot functions
    paste(input$variable1Sel,"~", input$variable2Sel)
  })
  
  output$caption <- renderText({
    # Return the formula text for printing as a caption ----
    formulaText()
  })
  
  output$bivariate <- renderPlot({
    # Generate a plot of the requested variables
    dataset <- datasetInput()
    tempdataset <- dataset
    myClass <- class(tempdataset)
    if(input$variable1Sel == input$variable2Sel){
      return()
    }
    if(myClass == "list") {
      tempdataset <- as.data.frame(datasetInput())
    }
    if("data.frame" %in% class(tempdataset)){
      boxplot(as.formula(formulaText()),
              data = tempdataset,
              col = "#75AADB", pch = 19)
    }
  })
  
  output$univariate <- renderPlot({
    # Histogram of the given dataset ----
    ## with requested number of bins
    dataset <- datasetInput()
    x <- dataset
    myClass <- class(x)
    if(myClass == "list") {
      x <- as.data.frame(datasetInput())
      tempVar <- input$variable1Sel
      if(!(input$variable1Sel %in% colnames(x))){
         tempVar <- colnames(x)[1]
      }
      x <- x[,tempVar]
    }
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black")
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)