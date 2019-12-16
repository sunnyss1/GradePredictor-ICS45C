# Author: Sunny Singh

library(shiny)
library(ggplot2)

load("ICS45C_clmm.Rmodel")
alphas <- array(grade_model[["alpha"]])
betas <- array(grade_model[["beta"]])

model_pred <- function(data_vals, inv.link = plogis) {
  Yhat <- crossprod(betas, data_vals)
  Alphas <- c(-1e3, alphas, 1e3)
  sapply(seq(length(alphas) + 1), function(j) {
    inv.link(Alphas[j+1] - Yhat) - inv.link(Alphas[j] - Yhat)
  })
}

ui <- fluidPage(
  titlePanel("Grade Predictor for ICS 45C"),
  sidebarPanel(
    sliderInput(inputId = "Proj0Score",
                label = "Enter your Project #0 score",
                value = 0,
                min = 0,
                max = 2,
                step = .5),

    sliderInput(inputId = "Proj1Score",
                label = "Enter your Project #1 score",
                value = 0,
                min = 0,
                max = 30,
                step = .5),
    sliderInput(inputId = "Proj1Lateness",
                label = "Enter how many hours late Project #1 was",
                value = 0,
                min = 0,
                max = 100,
                step = 1),

    sliderInput(inputId = "Proj2Score",
                label = "Enter your Project #2 score",
                value = 0,
                min = 0,
                max = 30,
                step  = .5),
    sliderInput(inputId = "Proj2Lateness",
                label = "Enter how many hours late Project #2 was",
                value = 0,
                min = 0,
                max = 100,
                step = 1),

    sliderInput(inputId = "Proj3Score",
                 label = "Enter your Project #3 score",
                 value = 0,
                 min = 0,
                 max = 30),
    sliderInput(inputId = "Proj3Lateness",
                label = "Enter how many hours late Project #3 was",
                value = 0,
                min = 0,
                max = 100,
                step = 1),

    sliderInput(inputId = "Proj4Score",
                label = "Enter your Project #4 score",
                value = 0,
                min = 0,
                max = 30,
                step = .5),
    sliderInput(inputId = "Proj4Lateness",
                label = "Enter how many hours late Project #4 was",
                value = 0,
                min = 0,
                max = 100,
                step = 1),

    sliderInput(inputId = "MTScore",
                label = "Enter your Midterm percent",
                value = 0,
                min = 0,
                max = 100,
                step = .5),
    actionButton(inputId = "recalculateDataSet", label = "Calculate")
  ),
  mainPanel(
    plotOutput(outputId = "GradeDist"),
    p("I made this (unoffical) app using an ",
      a("ordered logistic regression model", href = "https://en.wikipedia.org/wiki/Ordered_logit"),
      " which models grades. It uses the grades from 2014 Fall, 2015 Fall, 2016 Fall, 2018 Fall, 2019 Winter, both sections of 2019 Spring and 2019 Fall taught by ",
      a("Alex Thornton", href = "https://www.ics.uci.edu/~thornton/"),
      "for ICS 45C. The model then generates the probability of getting that grade based the scores you gave it."),
    p("The model uses the Project #0 score, the sum of Project #1 thru Project #4 scores, and the Midterm score to predict the probabilities. The model will also take into account lateness and will refund the highest penalty for you. The highest probable grade will be highlighted in blue."),
    h5("History"),
    tags$ul(h6("Version 2019.12.16: Updated the model to include the 2019 Fall data"),
            h6("Version 2019.11.17.13: Updated the graph to use ggplot2"),
            h6("Version 2019.11.17: Added the other quarters data and rebuilt the model"),
            h6("Version 2019.03.30.12: Fixed the sliders to follow the grading policy"),
            h6("Version 2019.03.30: Added sliders for lateness"),
            h6("Version 2019.03.24: Changed the model to be loaded rather than to be built when a user launched the app"),
            h6("Version 2019.03.23.13: Model Fix"),
            h6("Version 2019.03.23: Initial public release"))
  )
)

server <- function(input, output)
{
  Proj0Value <- reactive({input$Proj0Score})
  Proj1Value <- reactive({input$Proj1Score})
  Proj2Value <- reactive({input$Proj2Score})
  Proj3Value <- reactive({input$Proj3Score})
  Proj4Value <- reactive({input$Proj4Score})
  MTValue <- reactive({input$MTScore})

  Proj1Penalty <- reactive({1 - (input$Proj1Lateness / 100)})
  Proj2Penalty <- reactive({1 - (input$Proj2Lateness / 100)})
  Proj3Penalty <- reactive({1 - (input$Proj3Lateness / 100)})
  Proj4Penalty <- reactive({1 - (input$Proj4Lateness / 100)})

  Proj1Adj <- reactive({Proj1Value() * Proj1Penalty()})
  Proj2Adj <- reactive({Proj2Value() * Proj2Penalty()})
  Proj3Adj <- reactive({Proj3Value() * Proj3Penalty()})
  Proj4Adj <- reactive({Proj4Value() * Proj4Penalty()})


  ScoreDifference <- eventReactive(input$recalculateDataSet, {
                                                                data.frame(Proj1 = if(input$Proj1Lateness < 100) Proj1Value() - Proj1Adj() else 0,
                                                                           Proj2 = if(input$Proj2Lateness < 100) Proj2Value() - Proj2Adj() else 0,
                                                                           Proj3 = if(input$Proj3Lateness < 100) Proj3Value() - Proj3Adj() else 0,
                                                                           Proj4 = if(input$Proj4Lateness < 100) Proj4Value() - Proj4Adj() else 0)
                                                             })

  dataValues <- eventReactive(input$recalculateDataSet, {
                                                          scores <- data.frame(Proj0 = Proj0Value(),
                                                                               Proj1 = Proj1Adj(),
                                                                               Proj2 = Proj2Adj(),
                                                                               Proj3 = Proj3Adj(),
                                                                               Proj4 = Proj4Adj(),
                                                                               MT = MTValue())

                                                          ProjToRefund <- names(which.max(as.data.frame(ScoreDifference())))
                                                          PointToRefund <- ScoreDifference()[ProjToRefund]
                                                          scores[ProjToRefund] <- scores[ProjToRefund] + as.numeric(PointToRefund)
                                                          scores
                                                        })

  output$GradeDist <- renderPlot({
                                    df <- dataValues()
                                    dataVal_matrix <- matrix(c(df[["Proj0"]], sum(df[["Proj1"]], df[["Proj2"]], df[["Proj3"]], df[["Proj4"]]), df[["MT"]] / 100))
                                    predictedGrade <- rev(model_pred(dataVal_matrix))
                                    ggplot(NULL, aes(x = seq(13), y = predictedGrade, fill = ifelse(seq(13) == which.max(predictedGrade), 1, 0))) +
                                      geom_bar(stat = "identity", show.legend = FALSE) +
                                      scale_x_discrete(limits= rev(c("F", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"))) +
                                      scale_y_continuous(limits = c(0, 1.05)) +
                                      geom_text(aes(label = paste(format(round(predictedGrade * 100, digits = 2), nsmall = 2), "%", sep = "")), nudge_y = 0.025) +
                                      xlab("Grade") +
                                      ylab("Probability of Getting This Grade") +
                                      scale_fill_gradient(low = "#454545") +
                                      theme_light()
                                })
}

shinyApp(ui = ui, server = server)
