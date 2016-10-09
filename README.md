# SimulateBML
`SimulateBML` is an R package to simulate the [Biham-Middleton-Levine (BML) traffic model](https://en.wikipedia.org/wiki/Biham%E2%80%93Middleton%E2%80%93Levine_traffic_model).

### Installation

To install, use the `devtools` package.

```R
install.packages("devtools")
library(devtools)
devtools::install_github("txzhou/SimulateBML")
```

### Example usage

The BML traffic model displays three phases with different initial set up in the grid.
I showcased how to use my package to analyze these phases ([see this](./vignettes/Phases.pdf)), and how to use the provided tools to assess the performance of the code for different grid sizes and grid densities ([see this](./vignettes/Performance.pdf)).
