# ADHD New Zealand Research Data Dashboard - Package Installation Script

# Check and install required R packages
required_packages <- c(
  "shiny",
  "shinydashboard", 
  "readxl",
  "dplyr",
  "plotly",
  "DT",
  "leaflet",
  "sf",
  "ggplot2",
  "tidyr"
)

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  cat("Installing missing packages...\n")
  install.packages(missing_packages, dependencies = TRUE)
  cat("Package installation completed!\n")
} else {
  cat("All required packages are already installed.\n")
}

# Verify installation
cat("\nVerifying package installation status:\n")
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE)) {
    cat(paste("✓", pkg, "installed\n"))
  } else {
    cat(paste("✗", pkg, "installation failed\n"))
  }
}

cat("\nInstallation completed! You can now run shiny::runApp() to start the application.\n")
