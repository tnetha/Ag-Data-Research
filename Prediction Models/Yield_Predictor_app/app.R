library(shiny)
library(randomForest)
load('data/models.RData')
ui <- fluidPage(
  titlePanel('Corn Yield Prediction Models'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model",
                  label = 'Choose a model to use',
                  choices = c('Linear Regression', 'Random Forest'),
                  selected = 'Linear Regression'),
      sliderInput("pcpn",
                   h3("Total Summer Rainfall (in)"), min = 0, max = 65, value = 20),
      sliderInput("temp", 
                   h3("Average Temperature April-October (F)"), min=0, max=100, value = 50),
      sliderInput("year",
                   h3('Year'), min = 1970, max = 2030, value = 1970),
      selectInput("state", label = 'State',
                  choices = c('OH','IN','IL','IA','MO','KS','NE','SD','MN'))),
  
    mainPanel(
      textOutput("selected_var")
    )
  )
)

server <- function(input, output) {
  output$selected_var <- renderText({
    test_state <- as.factor(c('OH','IN','IL','IA','MO','KS','NE','SD','MN'))
    if (input$model == 'Linear Regression') {
      model.app <- model.lm
    } else {model.app <- yearlydf.rf}
    paste('Predicted Corn Yield (BU/AC):', predict(model.app, data.frame(year = input$year, tot_precip = input$pcpn, avgt = input$temp, state = test_state[test_state == input$state])))
  })
}

shinyApp(ui,server)