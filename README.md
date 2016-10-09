# SimulateBML
`SimulateBML` is an R package to simulate the [Biham-Middleton-Levine (BML) traffic model](https://en.wikipedia.org/wiki/Biham%E2%80%93Middleton%E2%80%93Levine_traffic_model).

### Installation

To install, use the `devtools` package.

```R
install.packages("devtools")
library(devtools)
devtools::install_github("txzhou/SimulateBML")
```

### Functions in NAMESPACE

The package has S3 methods `plot` and `summary` for the `BML` class.

Other functions include:

* `createBMLGrid`: initialize a BML grid.
* `velocityBMLGrid`: evaluate the average velocity of a state of a BML grid.
* `crunBMLGrid`: run BML simulation using C routines.
* `plotBMLPhases`, `plotBMLVelocities`: functions used to analyze the phases of the BML grids.
* `timingGrid`, `plotPerformanceDim`, `plotPerformanceDen`: functions used to assess the performance of the code.

### Example usage

The BML traffic model displays three phases with different initial set up in the grid.
I showcased how to use my package to analyze these phases ([see this](./vignettes/Phases.pdf)), and how to use the provided tools to assess the performance of the code for different grid sizes and grid densities ([see this](./vignettes/Performance.pdf)).
