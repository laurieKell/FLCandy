# FLCandy

## Overview

FLCandy is an R package containing prototypes and candidate methods for the FLR (Fisheries Library for R) framework. It provides experimental implementations of various fisheries assessment and management tools.

## Installation

### From GitHub (Development Version)
```r
# Install devtools if not already installed
if (!require(devtools)) install.packages("devtools")

# Install FLCandy
devtools::install_github("flr/FLCandy")
```

### Dependencies

FLCandy requires the following packages:
- **FLCore** (>= 2.6.16) - Core FLR functionality
- **FLBRP** (>= 2.5.9.9025) - Biological reference points
- **FLife** (>= 3.0.0) - Life history parameters
- **mydas** (>= 1.2.2) - Additional FLR utilities
- **TMB** (>= 1.7.19) - Template Model Builder
- **ggplot2** - Plotting
- **ggplotFL** - FLR-specific plotting

## Key Features

### Biological Reference Points
- **Production Type (PT) Method**: Surplus production model parameter estimation
- **MSY and Virgin State Calculations**: Key fisheries metrics
- **Process Error Analysis**: Population dynamics validation

### Assessment Methods
- **JABBA Integration**: Joint Analysis of Biomass and Abundance
- **Length-Based Indicators**: Biological reference indicators
- **Spatial Analysis**: Geographic distribution functions

### Management Tools
- **Harvest Control Rules**: ICES-style management procedures
- **Rebuild Analysis**: Stock recovery assessment
- **Seasonal Analysis**: Time-series decomposition

## Usage Examples

### Basic Reference Point Calculation
```r
library(FLCandy)
library(FLCore)

# Load example data
data(ple4)
data(ple4brp)

# Calculate production type parameters
pt_params <- pt(ple4brp)

# Extract MSY and virgin state metrics
msy_virgin <- msyVirgin(ple4brp)
```

### Biological Indicators
```r
# Calculate biological indicators
indicators <- ind(ple4, ple4brp)

# View results
head(indicators)
```

### Harvest Control Rule
```r
# Apply ICES harvest control rule
tac <- hcrICES(ple4, ple4brp, 
               start = 2010, 
               end = 2020)
```

## Documentation

- **Vignettes**: See the `vignettes/` directory for detailed examples
- **Function Help**: Use `?function_name` for individual function documentation
- **Examples**: Check the `examples/` directory for usage examples

## Contributing

This package contains experimental methods. Contributions are welcome:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## Citation

If you use FLCandy in your research, please cite:

```r
citation("FLCandy")
```

## License

This package is licensed under GPL (>= 2). See the LICENSE file for details.

## Contact

- **Maintainer**: Laurence Kell <laurie@kell.es>
- **Issues**: Report bugs and feature requests on GitHub
- **Discussion**: Use GitHub Discussions for questions and ideas
