library(ggvis)
library(dplyr)
deptchoices <- c('DCMS','HMT','DWP','CO','BIS','DfID','DECC')
ui = fluidPage(
  column(width=6,
  wellPanel(
    column(width = 3,
      selectInput(choices=deptchoices, inputId = 'select_dept',label = 'Department',
                  selected = 'HMT', width='90px')
                      ),
    column(width=3,
      sliderInput(min=2010, max=2014, step=1,animate = TRUE, value=2010,
                  inputId = 'slider_date', label='Year',format='####',
                  width='100px')
             )
    ),
  fluidRow(column(width=6,
    ggvisOutput('ggvis')
    )
  )
)
)