\name{summary.BML}
\alias{summary.BML}
\alias{summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to summarize a "BML" object.
}
\description{
A S3 method to summarize a "BML" object.
}
\usage{
  \method{summary}{BML}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A "BML" object.
}
  \item{...}{
Allows for more arguments.
}
}
\value{
Returns a list which contains:
  The number of red cars, the number of blue cars and the total number of cells in the grid;
  The density of the grid;
  The average velocity of the cars in the grid.
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
