library(shiny)
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  sequence <- reactive({ sequence <- c(input$sequence)})
  sequence2 <- reactive({ sequence2 <- c(input$sequence2)})
  # Return the hydrophobicity value of a primary sequence 11 amino acids long
  iscore <- function(x, primary_sequence) {
    scores <- c(4.5, 4.2, 3.8, 2.8, 2.5, 1.9, 1.8, -0.4, -0.7, -0.9, -0.8, -1.3, -1.6, -3.2, -3.5, -3.5, -3.5, -3.5, -3.9, -4.5)
    names(scores) <- c('I', 'V', 'L', 'F','C','M','A', 'G', 'T','W', 'S', 'Y', 'P', 'H', 'E', 'Q', 'D', 'N', 'K', 'R')
    list_to_sum <- c()
    start <- x-6
    end <- x+4
    for (n in start:end){
      list_to_sum <- c(list_to_sum, scores[substr(primary_sequence, n, n)])
    }
    result <- round(sum(list_to_sum)/11,1)
    result
  }
  
  #Get y values of plot, ie the hydrophobicity scores
  gety <- function(primary_sequence) {
    end = nchar(primary_sequence)-5
    fn <- function(x) {iscore(x, primary_sequence)}
    L = c(sapply(7:end, fn))
    L
  }
  
  #Get x values of plot, ie the sequence number
  getx <- function(primary_sequence){
    L <- 7:(nchar(primary_sequence)-5)
    L
  }
  
  #Plot the values as a line graph, with straight line at y=0
  summary <- function(primary_sequence){
    plot(getx(primary_sequence), gety(primary_sequence), type = "n", xlab="Sequence Number", ylab = "Hydropathic Index", ylim = c(-4,4))
    lines(getx(primary_sequence), gety(primary_sequence), type = "l")
    abline(0,0)
  }
sequence <- reactive({
  if(input$sequence2 != ""){
    sequence <- gsub(" ", "",toupper(input$sequence2))
  }  else sequence <- input$sequence
  sequence
})
  
  output$summary <- renderPlot({
    summary(sequence())
    })
  
  # Generate a summary of the dataset
 # output$summary2 <- renderPlot({
  #  summary(gsub(" ", "",toupper(input$sequence2)))
  #}

  #output$summary <- renderPlot({
  #  summary(input$sequene)
  #})
})
