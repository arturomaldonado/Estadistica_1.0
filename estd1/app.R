#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinyAce)
library(tidyverse)
library(rio)
lapop18 <- import("https://raw.github.com/lapop-central/materials_edu/main/LAPOP_AB_Merge_2018_v1.0.sav")
lapop18 <- subset(lapop18, pais<=35)
lapop18$genero <- as.factor(lapop18$q1)
levels(lapop18$genero) <- c("Hombre", "Mujer")
table(lapop18$genero)

lapop6 <- lapop18 %>% select(dem30, idio2, prot3, genero, ur, tamano)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #Título
    titlePanel("Chi cuadrado"),
    
    #Barra lateral
    sidebarLayout(
        
        #Inputs de la barra lateral
        sidebarPanel(
            
            #Input VD
            selectInput("var1", "Escoja una variable dependiente", choices=colnames(lapop6)),
            
            #Input VI
            selectInput("var2", "Escoja una variable independiente", choices=colnames(lapop6)),
            
            #Input de botón de refresco
            actionButton("update", "Update selección")
        ),
        
        #Panel central para resultados
        mainPanel(
            
            #Output de tabla de contingencia
            h4("Tabla"),
            verbatimTextOutput("tabla"),
            br(),
            
            #Output de resumen de prueba
            h4("Chi-cuadrado"),
            verbatimTextOutput("summary"),
            br()
        )
    )

)

server <- shinyServer(function(input, output) {
    data1 <- reactive({
        library(tidyverse)
        dat <- lapop6 %>% select(input$var1, input$var2)
        x <- prop.table(table(dat), 2)*100
        x <- addmargins(x, 1)
        
        print(x)
    })
    
    output$tabla <- renderPrint({
        data1()
    })
    
    test1 <- reactive({
        library(tidyverse)
        dat <- lapop6 %>% select(input$var1, input$var2)
        x <- table(dat)
        
        a <- chisq.test(x)
        
        print(a)
    })
    
    output$summary <- renderPrint({
        test1()
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
