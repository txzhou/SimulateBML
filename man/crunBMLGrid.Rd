\name{crunBMLGrid}
\alias{crunBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to run a BML smulation using C routines.
}
\description{
This function takes a BML grid and do the simulation according to the rules for several steps. The function returns a "BML" object as a result of the simulation. 
}
\usage{
crunBMLGrid(BML.Grid, numSteps = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BML.Grid}{
A "BML" object used as the starting point of the simulation.
}
  \item{numSteps}{
Number steps we want for the simulation.
}
}
\value{
The value returned is a "BML" object. The BML grid is presented using a matrix. We define a new "BML" class for the "BML" object. The class "matrix" is also preserved.
}
\references{
  \url{http://en.wikipedia.org/wiki/Biham%E2%80%93Middleton%E2%80%93Levine_traffic_model
}
}
\author{
Tianxia Zhou
}
\examples{
library("SimulateBML")

# Create 100*99 Grid, with 2500 red cars and 1500 blue cars 
g = createBMLGrid(r = 100, c = 99, c(red = 2500, blue = 1500)) 

# Run Simulation, for 500 steps
g.out = crunBMLGrid(g, numSteps = 500)

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
