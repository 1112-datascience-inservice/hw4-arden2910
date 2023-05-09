library(shiny)
library(FactoMineR)
library(factoextra)
library(ggbiplot)

ui <- fluidPage(
  titlePanel("延康的 PCA and Correspondence Analysis Visualization"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("X-axis PCA:"),
      actionButton("x_pca1", "X : PCA1"),
      actionButton("x_pca2", "X : PCA2"),
      actionButton("x_pca3", "X : PCA3"),
      tags$h4("Y-axis PCA:"),
      actionButton("y_pca1", "Y : PCA1"),
      actionButton("y_pca2", "Y : PCA2"),
      actionButton("y_pca3", "Y : PCA3"),
      tags$h4("Correspondence Analysis:"),
      sliderInput("n_rows", "Number of rows:", min = 3, max = nrow(iris), value = nrow(iris), step = 1),
      img(src = "https://i.ibb.co/cy1g8wz/cat.jpg", height = "426px", width = "225px"),
      
      ),
    mainPanel(
      plotOutput("pcaPlot"),
      plotOutput("caPlot")
    )

  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(choices = c(1, 2))
  
  observeEvent(input$x_pca1, { rv$choices[1] <- 1 })
  observeEvent(input$x_pca2, { rv$choices[1] <- 2 })
  observeEvent(input$x_pca3, { rv$choices[1] <- 3 })
  
  observeEvent(input$y_pca1, { rv$choices[2] <- 1 })
  observeEvent(input$y_pca2, { rv$choices[2] <- 2 })
  observeEvent(input$y_pca3, { rv$choices[2] <- 3 })
  
  output$pcaPlot <- renderPlot({
    validate(
      need(rv$choices[1] != rv$choices[2], "Collinearity in the data, please click different PCA configure button.")
    )
    
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    
    g <- ggbiplot(ir.pca, choices = rv$choices, obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  output$caPlot <- renderPlot({
    ca <- CA(X = iris[1:input$n_rows, 1:4], graph = FALSE)
    fviz_ca_biplot(ca, repel = TRUE)
  })
}

shinyApp(ui = ui, server = server)
