# Phase transition and average velocities
plotBMLPhases = function(rows = 100, cols = 99, density, numSteps = 2000) {
  g = createBMLGrid(r = rows, c = cols, rho = density)
  g.out = runBMLGrid(g, numSteps = numSteps)
  v0 = format(x = velocityBMLGrid(g), digits = 3)
  vt = format(x = velocityBMLGrid(g.out), digits = 3)
  par(mfcol = c(1,2))
  plot(g,
       title = paste0("BML Grid, density: ", density,
                      "\n Initial Velocity : ", v0))
  plot(g.out,
       title = paste0("BML Grid density: ", density,
                      "\n After ", numSteps, " steps",
                      "\n End Velocity : ", vt))
  par(mfcol = c(1,1))
}

plotBMLVelocities = function(rows = 100, cols = 99, density, numSteps = 2000, measureEvery = 10) {
  g = createBMLGrid(r = rows, c = cols, rho = density)
  df.velocity = SimulateBML:::recordVelBML(BML.Grid = g, numSteps = numSteps, measureEvery = measureEvery)
  plot(df.velocity,
       type = "l",
       main = paste0("Velocity of BML Grid",
                     "\n density: ", density))
}
