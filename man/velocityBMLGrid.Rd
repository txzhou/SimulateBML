\name{velocityBMLGrid}
\alias{velocityBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function measure the average velocity of the cars on a BML grid. 
}
\description{
This function takes a BML grid and do the simulation according to the rules for several steps. The function returns a "BML" object as a result of the simulation. 
}
\usage{
velocityBMLGrid(BML.Grid, move.first = "red")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BML.Grid}{
A "BML" object used in measuring the average velocity.
}
  \item{move.first}{
The argument should be either "red", or "blue" that indicate what color of cars is going to move next. It will affect how we want to calculate the average velocity.
}
}
\value{
The value returned is a numeric value (0-1). 1 means all cars are moving and 0 means no cars are moving, any state in between will get a number between 0 and 1.
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
g.out = runBMLGrid(g, numSteps = 500)

# Get the velocities of the cars

velocityBMLGrid(g)
velocityBMLGrid(g.out)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
