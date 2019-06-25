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