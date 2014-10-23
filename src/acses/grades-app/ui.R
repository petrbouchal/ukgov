library(ggvis)
library(dplyr)

ui = fixedPage(responsive = TRUE,
  fixedRow(
      column(width=3,
        sliderInput(min=2010, max=2014, step=1,animate = TRUE, value=2010,
                    inputId = 'slider_date', label='Year',format='####',
                    width='150px')
                        )

    ),
  fixedRow(
      column(width=4,ggvisOutput('ggvis1')),
      column(width=4,ggvisOutput('ggvis2')),
      column(width=4,ggvisOutput('ggvis3'))

    ),
  fixedRow(
      column(width=4,ggvisOutput('ggvis4')),
      column(width=4,ggvisOutput('ggvis5')),
      column(width=4,ggvisOutput('ggvis6'))

    ),
  fixedRow(
      column(width=4,ggvisOutput('ggvis7'))

    )
)
