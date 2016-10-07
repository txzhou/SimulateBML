\name{SimulateBML-package}
\alias{SimulateBML-package}
\alias{SimulateBML}
\docType{package}
\title{
Tools for BML simulation
}
\description{
This package provides tools to use in BML simulation.
It provides functions to create, plot, summarize and analyze "BML" object.
It also provides functions to run the simulation.
}
\details{

The BML model (Biham-Middleton-Levine model) is a traffic flow simulation model. In this implementation in this package, a grid represents streets containing red cars and blue cars. Initially, cars are distributed at random. At each odd-numbered time step, all the red cars attempt to move one unit East; at each even-numbered time step, all the blue cars simultaneously attempt to move one unit North. A car succeeds if there is an empty space at its destination.
}
\author{
Tianxia Zhou

Maintainer: Tianxia Zhou <tianxiazhou@gmail.com>
}
\references{
\url{https://www.jasondavies.com/bml/#0.50/128/128}
}

\keyword{ package }

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
