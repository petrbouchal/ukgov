# by Yihui in Shiny google group
# produces date field updatable by slider
runApp(list(
  ui = basicPage(
    sliderInput('foo1', 'How many quarters after today?', min = 0, max = 12, value = 0,animate=T),
    dateInput('foo2', 'Input a date:', value = Sys.Date(),format='M yy')
  ),
  server = function(input, output, session){
    observe({
      updateDateInput(session, 'foo2', value = as.Date('2010-09-30') + input$foo1*90)
    })
  }
))