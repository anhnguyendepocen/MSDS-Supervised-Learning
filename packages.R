install.packages('tinytex')
tinytex::install_tinytex(force = T)

# verify install
tinytex:::is_tinytex()

# Alternative Solution
install.packages("devtools")
library(devtools)
install_version("rmarkdown", version = 1.8)

install.packages("corrplot")
install.packages("lattice")
install.packages("scatterplot3d")
install.packages("Rcmdr")
install.packages("reshape")
install.packages("GGally")
install.packages("MASS")
install.packages("data.table")
install.packages("ggrepel")
install.packages("formattable")
install.packages("skimr")
install.packages("summarytools")
install.packages("lessR")
install.packages("olsrr")
install.packages("dplyr")
install.packages("data.table")
install.packages("reshape2")

devtools::install_github("cardiomoon/ggiraphExtra")

install.packages('data.table')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggrepel')
install.packages('GGally')
install.packages('ggthemes')
install.packages('formattable')
install.packages('scales')
install.packages('reshape2')
install.packages('skimr')
install.packages('gridExtra')
install.packages('lessR')


remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages('rlang', dependencies = TRUE)
install.packages('lessR', dependencies = TRUE)
install.packages('zip', dependencies = TRUE)
install.packages('V8', dependencies = TRUE)
install.packages('dplyr', dependencies = TRUE)

install.packages('broom', dependencies = TRUE)
install.packages('sigr', dependencies = TRUE)
install.packages('WVPlots', dependencies = TRUE)

install.packages('sjPlot', dependencies = TRUE)
install.packages('sjmisc', dependencies = TRUE)
install.packages('car', dependencies = TRUE)

install.packages('lattice', dependencies = TRUE)

install.packages('ggiraphExtra', dependencies = TRUE)
install.packages('olsrr', dependencies = TRUE)
