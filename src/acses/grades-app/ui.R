library(ggvis)
library(dplyr)

ui = fluidPage(responsive = TRUE,
               title='The changing shape of government departments',
               titlePanel(title='The changing shape of the Civil Service',
                          windowTitle='The changing shape of the Civil Service'),
               HTML('<p>This is an interactive addendum to our 
                    <a href=\'\'>blog</a> on the structure of the 
                    Civil Service.'),
  fixedRow(
      column(width=12,
        div(style='width:180px;',
        sliderInput(min=2010, max=2014, step=1,
                    animate = animationOptions(interval=1600,
                                               playButton = HTML('<div style=\'margin-top:-25px; text-align: center; margin-left:150px;  font-size:22pt;\'>>></div>'),
                                               pauseButton = HTML('<div style=\'margin-top:-25px; text-align: center; margin-left:150px; font-size:22pt;\'>|| </div>')),
                    value=2010,
                    inputId = 'slider_date', label=NULL,format='####',
                    width='150px', ticks=NULL)
                        ))),
  fixedRow(
      column(width=3,ggvisOutput('ggvis_decc')),
      column(width=3,ggvisOutput('ggvis_dfid')),
      column(width=3,ggvisOutput('ggvis_hmt')),
      column(width=3,ggvisOutput('ggvis_dh'))
    ),
  fixedRow(
      column(width=3,ggvisOutput('ggvis_co')),
      column(width=3,ggvisOutput('ggvis_fco')),
      column(width=3,ggvisOutput('ggvis_defra')),
      column(width=3,ggvisOutput('ggvis_dfe'))

    ),
  fixedRow(
      column(width=3,ggvisOutput('ggvis_dcms')),
      column(width=3,ggvisOutput('ggvis_dft')),
      column(width=3,ggvisOutput('ggvis_dclg')),
      column(width=3,ggvisOutput('ggvis_bis'))
    ),
  fixedRow(
      column(width=3,ggvisOutput('ggvis_mod')),
      column(width=3,ggvisOutput('ggvis_hmrc')),
      column(width=3,ggvisOutput('ggvis_dwp')),
      column(width=3,ggvisOutput('ggvis_ho'))
    ),
  fixedRow(
      column(width=3,ggvisOutput('ggvis_moj'))
    )
)
