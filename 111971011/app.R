
library(shiny)
library(ggbiplot)

ui <- fluidPage(
  titlePanel("羅延康的PCA Visualization "),
  sidebarLayout(
    sidebarPanel(
      tags$h4("X-axis PCA:"),
      actionButton("x_pca1", "X : PCA1"),
      actionButton("x_pca2", "X : PCA2"),
      actionButton("x_pca3", "X : PCA3"),
      tags$h4("Y-axis PCA:"),
      actionButton("y_pca1", "Y : PCA1"),
      actionButton("y_pca2", "Y : PCA2"),
      actionButton("y_pca3", "Y : PCA3")
    ),
    mainPanel(
      plotOutput("pcaPlot")
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
      need(rv$choices[1] != rv$choices[2], "An error occurred due to collinearity in the data")
    )
    
    # log transform
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    
    g <- ggbiplot(ir.pca, choices = rv$choices, obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE)
    g <- g + scale_color_discrete(name = 'arden')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
}

shinyApp(ui = ui, server = server)

