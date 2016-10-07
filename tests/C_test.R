# Appendix C
# Unit test code

library(SimulateBML)

g1 = SimulateBML::createBMLGrid(r = 100, c = 100, rho = 0.5)
stopifnot(identical(SimulateBML::runBMLGrid(BML.Grid = g1, numSteps = 100),
                    SimulateBML::crunBMLGrid(BML.Grid = g1, numSteps = 100)))

g2 = SimulateBML::createBMLGrid(r = 100, c = 99, rho = 0.2)
stopifnot(identical(SimulateBML::runBMLGrid(BML.Grid = g2, numSteps = 100),
                    SimulateBML::crunBMLGrid(BML.Grid = g2, numSteps = 100)))
