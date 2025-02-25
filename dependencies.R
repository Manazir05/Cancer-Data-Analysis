# List of Required Packages
required_packages <- c("tidyverse", "ggplot2", "corrplot", "xtable", "dplyr", 
                       "reshape2", "ggrepel", "cluster", "scales", "tidyr")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

message("All required packages have been installed and loaded successfully.")