# CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Packages your analysis needs (add/remove as you like)
pkgs <- c(
  "IRkernel", "here", "readxl", "dplyr", "tidyr", "tibble", "knitr",
  "rmarkdown", "psych", "semTools", "MVN", "mice", "openxlsx",
  "simsem", "sjmisc", "semPlot"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

# Register the R kernel for Jupyter
IRkernel::installspec(user = FALSE)

