# ProfitBoost

This project estimates the profit lift of targeted interventions and optimize 
their profitability. It is based on the article Lemmens and Gupta (2020), 
Managing Churn to Optimize Profits, Marketing Science.
The paper can be downloaded here : https://pubsonline.informs.org/doi/10.1287/mksc.2020.1229

Installation
------------

To install the GitHub version (requires the "devtools" package):

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

To install ProfitBoost from CRAN (as soon as available there):

```R
install.packages('ProfitBoost')
```
