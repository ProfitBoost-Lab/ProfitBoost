# ProfitBoost

This project estimates the profit lift of targeted interventions and optimize 
their profitability. It is based on the article Lemmens and Gupta (2020), 
Managing Churn to Optimize Profits, Marketing Science.

Installation
------------

To install ProfitBoost from CRAN:

```R
install.packages('ProfitBoost')
```

To install the development version (requires the "devtools" package):

```R
install.packages("devtools")
devtools::install_github('AurelieLemmensRSM/ProfitBoost')
```

When working on or extending the package, clone its [GitHub repository](https://github.com/AurelieLemmensRSM/ProfitBoost), then do:

```R
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)
devtools::build()
devtools::reload()
```
