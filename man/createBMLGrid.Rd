\name{createBMLGrid}
\alias{createBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to initialize a BML grid. 
}
\description{
This function creates a BML grid for given parameters. The function returns a "BML" object for the use of running BML simulations. 
}
\usage{
createBMLGrid(r = 100, c = 99, ncars = c(red = 100, blue = 100), rho = NULL, p = 0.5, seed = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
Number of rows of the BML grid.
}
  \item{c}{
Number of columns of the BML grid.
}
  \item{ncars}{
A vector indicating the number of red cars and blue cars. See usage for example. The value of ncars is ignored if rho is specified.
}
  \item{rho}{
Alternatively, use this rho to specify the density of cars (0-1). When rho is used, the value of ncars is ignored.
}
  \item{p}{
Share of red cars in total cars (0-1). The default is 0.5. It is used when rho is specified. If rho is not specified the value of p is ignored.
}
  \item{seed}{
A random seed. Default value is 1.
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
