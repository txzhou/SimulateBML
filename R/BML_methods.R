# Appendix D
# functions in package

# Methods for class "BML" ####
summary.BML = function(object, ...) {
  BML.Grid = object
  s = (table(BML.Grid))
  return(list(c(num.cells = sum(s),
                num.red = s["red"][[1]],
                num.blue = s["blue"][[1]]),
              density = (s["red"][[1]] +s["blue"][[1]])/sum(s),
              velocity = velocityBMLGrid(BML.Grid)))
}

plot.BML = function(x, y, palette = c(red = "#ca0020", blue = "#92c5de", white = "#f7f7f7"), title = "", ...) {
  BML.Grid = x
  x_right = ncol(BML.Grid)+0.5
  y_top = nrow(BML.Grid)+0.5
  # Graphical display
  plot(x = c(0.5, x_right),
       y = c(0.5, y_top),
       type = "n",
       xlab = "",
       ylab = "",
       asp = 1, main = title)
  rect(xleft = as.vector(col(BML.Grid)-0.5),
       ybottom = as.vector(row(BML.Grid)-0.5),
       xright = as.vector(col(BML.Grid)+0.5),
       ytop = as.vector(row(BML.Grid)+0.5),
       col = palette[BML.Grid],
       border = "transparent")
  # print a outer boarder
  rect(xleft = 0.5,
       ybottom = 0.5,
       xright = x_right,
       ytop = y_top,
       border = TRUE)
}