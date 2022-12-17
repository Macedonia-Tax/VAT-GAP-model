'Before starting the application, the following command installs the specified libraries. '

packageurl <- "https://cran.r-project.org/src/contrib/Archive/rccmisc/rccmisc_0.3.7.tar.gz"
install.packages(packageurl, repos=NULL, type="source")



list.of.packages <- c("DataEditR", "data.table","tidyr","readxl","dplyr","reshape2","xlsx",
                      "flexdashboard","ggplot2","plotly","tidyverse","knitr","DT","rpivotTable",
                      "forecast","openintro","highcharter","ggvis","DataCombine","viridisLite","orca","sass","vctrs","markdown",
                      "sm","ks","rlang","shades")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


