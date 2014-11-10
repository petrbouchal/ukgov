data <- data.frame(year=rep(2000:2002, each=23), 
                   x=rnorm(23*3,10), y=rnorm(23*3,10),
                   count=c(rnorm(23,2), rnorm(23,4), rnorm(23,6)))


year_lr <- left_right(1997, 2002, value=2000, step=1)
year_wrapper <- reactive({
  as.numeric(year_lr() == data$year) 
})

class(year_wrapper) <- c("broker", class(year_wrapper))
attr(year_wrapper, "broker") <- attr(year_lr, "broker")

data %>% 
  ggvis(~x, ~y, size=~count) %>% 
  layer_points(opacity:=year_wrapper)

data %>% 
  ggvis(~x, ~y, size=~count) %>% 
  layer_points(opacity=input_slider(min(data$year), max(data$year), step=1, 
                                    map=function(x) ifelse(data$year == x, 1, 0)))
