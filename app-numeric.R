# StatusGiziBalita
# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")

#User Interface
ui <- fluidPage(theme = shinytheme("superhero"),
  
  # Page header
  headerPanel('Deteksi Status Gizi Balita'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Isilah kolom dibawah ini dengan lengkap')),
    numericInput("Umur", 
                 label = "Umur (Bulan)", 
                 value = 7),
    selectInput("JK", 
                label = "Jenis Kelamin", 
                choices = list("Laki-Laki" = "L", "Perempuan" = "P"), 
                selected = "Laki-Laki"),
    numericInput("BB", 
                 label = "Berat Badan (Kg)", 
                 value = 9.4),
    numericInput("TB", 
                 label = "Tinggi Badan (Cm)", 
                 value = 70),
    sliderInput("Gaji", "Gaji Orang Tua Per Bulan (Rupiah)",
                min = 0, max = 15000000,
                value = 2200000,
                step = 100000),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Hasil Prediksi')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)



server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Umur",
               "JK",
               "BB",
               "TB",
               "Gaji"),
      Value = as.character(c(input$Umur,
                             input$JK,
                             input$BB,
                             input$TB,
                             input$Gaji)),
      stringsAsFactors = FALSE)
    
    Status <- 0
    df <- rbind(df, Status)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Berikut merupakan probabilitas status gizi balita dengan data yang Anda masukkan") 
    } else {
      return("Pastikan seluruh kolom telah terisi")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


shinyApp(ui = ui, server = server)
