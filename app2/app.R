#
# This is a Shiny web application.
#

library(shiny)
library(rio)
LL <- import("https://raw.github.com/arturomaldonado/Estadistica_1.0/main/LL.csv")
library(lsr)
library(gplots)

LL$tratamiento <- as.factor(LL$treated)
levels(LL$tratamiento) <- c("Control", "Tratamiento")

# Define UI for application that draws a histogram
shinyApp(
    ui = fluidPage(
        tabsetPanel(
            tabPanel("Estadística descriptiva", fluid = T,
                     titlePanel("Estadísticos descriptivos básicos"),
                     sidebarLayout(
                         sidebarPanel(selectInput("hist", "Seleccione una variable de ingresos (re)", 
                                                  choices=colnames(LL), selected="")),
                         mainPanel(
                             h4("Resumen"),
                             verbatimTextOutput("desc"),
                             br(),
                             h4("Histrograma"),
                             plotOutput("histograma"),
                             br(),
                             h4("Tabla de frecuencias"),
                             tableOutput("tabla"),
                             br()
                         )
                     )),
            tabPanel("Intervalos de confianza", fluid=T,
                     titlePanel("Intervalos de confianza"),
                     sidebarLayout(
                         sidebarPanel(selectInput("ic1", "Seleccione una variable", 
                                                  choices=colnames(LL), selected=""),
                                      selectInput("ic2", "Seleccione una variable", 
                                                  choices=colnames(LL), selected="")),
                         mainPanel(
                             h4("Resumen"),
                             verbatimTextOutput("resu"),
                             br(),
                             h4(""),
                             verbatimTextOutput("resu2"),
                             br()
                             )
                         )),
            tabPanel("Comparación de 2 grupos", fluid=T,
                     titlePanel("Comparación de 2 grupos"),
                     sidebarLayout(
                         sidebarPanel(selectInput("vd", "Seleccione una variable numérica",
                                                  choices=colnames(LL), selected=""),
                                      selectInput("vi", "Selecciones una variable independiente",
                                                  choices=colnames(LL), selected="")),
                         mainPanel(
                             h4("Resumen"),
                             verbatimTextOutput("pruebat"),
                             br(),
                             h4("Gráfico"),
                             plotOutput("difmedia"),
                             br()
                         )
                     ))
    )),


    # Define server logic required to draw a histogram
    server = function(input, output) {
        output$desc <- renderPrint({
            summary(LL[,input$hist])
        })
        
        output$histograma <- renderPlot({
            hist(LL[,input$hist], freq=F, col = 'darkgray', border = 'white', main="", breaks=15)
            abline(v=mean(LL[,input$hist]), col="red")
            abline(v=median(LL[,input$hist]), col="blue")
        })
        
        output$tabla <- renderTable({
            prop.table(table(LL[,input$hist]))*100
        })
        
        output$resu <- renderPrint({
            ciMean(LL[,input$ic1])
        })
        
        output$resu2 <- renderPrint({
            ciMean(LL[,input$ic2])    
        })
        
        output$pruebat <- renderPrint({
            t.test(LL[, input$vd]~LL[, input$vi], var.equal=F)
        })
        
        output$difmedia <- renderPlot({
            plotmeans(LL[, input$vd]~LL[, input$vi], 
                      xlab="Variable independiente", ylab="Variable dependiente",
                      mean.labels = T, digits = 1,
                      connect = F)
        })
        
    }
)