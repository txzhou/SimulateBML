# The performance of the code ####
timingGrid = function(r, c, density,
                      run.function = SimulateBML::runBMLGrid,
                      numSteps = 100) {
  # When testing the performance of "crunBMLGrid()" just set run.function to "SimulateBML::crunBMLGrid()"
  time = system.time({
    g = createBMLGrid(r = r, c = c, rho = density)
    run.function(BML.Grid = g, numSteps = numSteps)
  })
  return(time["elapsed"])
}

plotPerformanceDim = function(v.dimension = 1:30*10,
                                fixed.density = 0.5,
                                run.function = SimulateBML::runBMLGrid,
                                numSteps = 100) {

  time.dimension = sapply(X = v.dimension,
                          FUN = function(dim)
                            timingGrid(r = dim, c = dim, density = fixed.density,
                                       run.function = run.function,
                                       numSteps = numSteps))

  plot.dimension = plot(x = v.dimension^2,
                        y = time.dimension,
                        xlab = "size of the grid (total number of cells)",
                        ylab = "run time (seconds)",
                        main = paste0("Run time of simulation on BML grids of different sizes \n",
                                      "(run for ", numSteps, "steps;",
                                      " with density = ", fixed.density, ")" ))

  return(time.dimension)
}

plotPerformanceDen = function(v.density = 20:70 * 0.01,
                                fixed.nrow = 300,
                                fixed.ncol = 300,
                                run.function = SimulateBML::runBMLGrid,
                                numSteps = 100) {

  time.density = sapply(X = v.density,
                        FUN = function(den)
                          timingGrid(r = fixed.nrow, c = fixed.ncol, density = den,
                                     run.function = run.function,
                                     numSteps = numSteps))

  plot.density = plot(x = v.density,
                      y = time.density,
                      xlab = "density of the grid (proportion of the cars)",
                      ylab = "run time (seconds)",
                      main = paste0("Run time of simulation on BML grids of different densities \n",
                                    "(run for ", numSteps, "steps;",
                                    " with ", fixed.nrow, " rows and ", fixed.ncol, " columns)" ))

  return(time.density)
}
