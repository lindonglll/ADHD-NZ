# Simple network access script

# Get local IP address
get_ip <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    ip <- system("ipconfig", intern = TRUE)
    ip <- grep("IPv4", ip, value = TRUE)
    ip <- gsub(".*: ", "", ip)
    return(ip[1])
  } else {
    ip <- system("hostname -I", intern = TRUE)
    return(strsplit(ip, " ")[[1]][1])
  }
}

# Display access information
cat("=== ADHD New Zealand Research Data Dashboard ===\n")
cat("Starting application...\n\n")

local_ip <- get_ip()
cat("Local access address: http://localhost:3838\n")
cat("LAN access address: http://", local_ip, ":3838\n", sep = "")
cat("Other devices can access via LAN address\n\n")

cat("Press Ctrl+C to stop application\n")
cat("========================\n\n")

# Start application
shiny::runApp(host = "0.0.0.0", port = 3838)
