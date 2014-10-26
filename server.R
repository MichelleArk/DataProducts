library(shiny)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$sequence,
           "p53" = p53,
           "Glycophorin A" = glyA,
           "something else" = se)
  })
  # Return the hydrophobicity value of a primary sequence 11 amino acids long
  y <- function(x, primary_sequence)
    scores <- c(4.5, 4.2, 3.8, 2.8, 2.5, 1.9, 1.8, -0.4, -0.7, -0.9, -0.8, -1.3, -1.6, -3.2, -3.5, -3.5, -3.5, -3.5, -3.9, -4.5)
    names(scores) <- c('I', 'V', 'L', 'F','C','M','A', 'G', 'T',
                        'W', 'S', 'Y', 'P', 'H', 'E', 'Q', 'D', 'N', 'K', 'R')
    list_to_sum <- []
    for (n in (x-6):(x+5){
      list_to_sum.append(scores[substr(primary_sequence, n, n)])
    return round(sum(list_to_sum)/11,1)
    }

    
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})
