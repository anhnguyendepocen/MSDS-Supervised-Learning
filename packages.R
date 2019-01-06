install.packages('tinytex')
tinytex::install_tinytex(force = T)

# verify install
tinytex:::is_tinytex()

# Alternative Solution
install.packages("devtools")
library(devtools)
install_version("rmarkdown", version = 1.8)