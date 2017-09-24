library(shiny)
myDatasets <- as.data.frame(data()["results"])

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
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

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  # Return the requested dataset ----
  datasetInput <- reactive({
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
  # Set the label and select items
  observe({
    updateSelectInput(session, "variable1Sel",choices = colnames(as.data.frame(datasetInput())))
    updateSelectInput(session, "variable2Sel",choices = colnames(as.data.frame(datasetInput())))
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    tempdataset <- dataset
    myClass <- class(tempdataset)
    if(myClass == "list") {
      tempdataset <- as.data.frame(datasetInput())
    }
    summary(tempdataset)
  })
  
  # Get the dataset class----
  output$class <- renderPrint({
    dataset <- datasetInput()
    class(dataset)
  })
  
  # Get the dataset str----
  output$str <- renderPrint({
    dataset <- datasetInput()
    tempdataset <- dataset
    myClass <- class(tempdataset)
    if(myClass == "list") {
      tempdataset <- as.data.frame(datasetInput())
    }
    str(tempdataset)
  })
  
  # Show the first "n" observations ----
  output$sample <- renderTable({
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
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$variable1Sel,"~", input$variable2Sel)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variables
  output$bivariate <- renderPlot({
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
  
  # Histogram of the discoveries Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$univariate <- renderPlot({
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