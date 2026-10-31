
# IAPM Shiny Lab
# Two-Asset Portfolio Calculator

library(shiny)

ui <- fluidPage(

  titlePanel("Portfolio Risk and Return Calculator"),

  sidebarLayout(

    sidebarPanel(

      h4("Portfolio Inputs"),

      sliderInput(
        "w1",
        "Weight of Asset A",
        min = 0,
        max = 1,
        value = 0.60,
        step = 0.05
      ),

      numericInput(
        "mu1",
        "Expected Return A (%)",
        value = 12
      ),

      numericInput(
        "mu2",
        "Expected Return B (%)",
        value = 8
      ),

      numericInput(
        "sd1",
        "Volatility A (%)",
        value = 20,
        min = 0
      ),

      numericInput(
        "sd2",
        "Volatility B (%)",
        value = 10,
        min = 0
      ),

      sliderInput(
        "rho",
        "Correlation",
        min = -1,
        max = 1,
        value = 0.30,
        step = 0.05
      )
    ),

    mainPanel(

      h3("Portfolio Results"),

      tableOutput("results"),

      h4("Diversification Analysis"),

      plotOutput("riskPlot")

    )
  )
)

server <- function(input, output, session) {

  portfolio <- reactive({

    w1 <- input$w1
    w2 <- 1 - w1

    mu1 <- input$mu1 / 100
    mu2 <- input$mu2 / 100

    sd1 <- input$sd1 / 100
    sd2 <- input$sd2 / 100

    rho <- input$rho

    validate(
      need(
        is.finite(mu1) && is.finite(mu2) &&
        is.finite(sd1) && is.finite(sd2),
        "Enter finite numeric inputs."
      ),
      need(
        sd1 >= 0 && sd2 >= 0,
        "Volatility cannot be negative."
      )
    )

    expected_return <- w1 * mu1 + w2 * mu2

    variance <- w1^2 * sd1^2 +
      w2^2 * sd2^2 +
      2 * w1 * w2 * sd1 * sd2 * rho

    volatility <- sqrt(max(variance, 0))

    list(
      return = expected_return,
      variance = variance,
      volatility = volatility
    )
  })

  output$results <- renderTable({

    p <- portfolio()

    data.frame(
      Measure = c(
        "Expected Return (%)",
        "Portfolio Variance",
        "Portfolio Volatility (%)"
      ),
      Value = c(
        round(p$return * 100, 2),
        round(p$variance, 5),
        round(p$volatility * 100, 2)
      )
    )

  })

  output$riskPlot <- renderPlot({

    w <- seq(0, 1, by = 0.01)

    sd1 <- input$sd1 / 100
    sd2 <- input$sd2 / 100
    rho <- input$rho

    risk <- sqrt(pmax(
      w^2 * sd1^2 +
      (1 - w)^2 * sd2^2 +
      2 * w * (1 - w) * sd1 * sd2 * rho,
      0
    ))

    plot(
      w,
      risk * 100,
      type = "l",
      lwd = 2,
      xlab = "Weight of Asset A",
      ylab = "Portfolio Volatility (%)",
      main = "Portfolio Risk and Asset Weights"
    )

    points(
      input$w1,
      portfolio()$volatility * 100,
      pch = 19,
      col = "red"
    )
  })
}

shinyApp(ui = ui, server = server)
