\name{plot.BML}
\alias{plot.BML}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot a "BML" object.
}
\description{
A S3 method to plot a "BML" object.
}
\usage{
\method{plot}{BML}(x, y, palette = c(red = "#ca0020", blue = "#92c5de", white = "#f7f7f7"), title = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A "BML" object.
}
  \item{y}{
Argument that is not used
}
  \item{palette}{
A palette user can specify for the presentation of the grid.
}
  \item{...}{
Allows to put more command here.
}
  \item{title}{
A string can be used to specify the title of the plot.
}
}
\value{
Returns a plot.
}
\author{
Tianxia Zhou
}
\examples{
library("SimulateBML")

# Create 100*99 Grid, with 2500 red cars and 1500 blue cars 
g = createBMLGrid(r = 100, c = 99, c(red = 2500, blue = 1500)) 

# Run Simulation, for 500 steps
g.out = runBMLGrid(g, numSteps = 500)

# plot the grid
plot(g)
plot(g.out)

#Summarize the output

summary(g)
summary(g.out)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
