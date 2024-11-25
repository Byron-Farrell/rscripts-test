# List of required packages
required_packages <- c(
  "readr",
  "tidyverse",
  "DSI",
  "DSLite",
  "dsBase",
  "dsBaseClient",
  "dsSwissKnife",
  "dsSwissKnifeClient"
)
# Function to check if a package is installed, and if not, install it
install_missing_packages <- function(packages) {
  installed_packages <- installed.packages()[, "Package"]
  to_install <- setdiff(packages, installed_packages)
  
  if (length(to_install) > 0) {
    install.packages(to_install, repos = "https://cran.r-project.org")
    message("Installed the following packages: ", paste(to_install, collapse = ", "))
  } else {
    message("All packages are already installed.")
  }
}
# Install the required packages
install_missing_packages(required_packages)
# Ensure Bioconductor-based packages (like dsBase, dsSwissKnife) are available
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = "https://cran.r-project.org")
}
# Install Bioconductor-specific packages
bioc_packages <- c("dsBase", "dsSwissKnife")
BiocManager::install(setdiff(bioc_packages, installed.packages()[, "Package"]))
message("All packages installed.")
