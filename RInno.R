# Require Package
require(RInno)
library(installr)
# Use RInno to get Inno Setup
RInno::install_inno()

# Build an installer
create_app(
  app_name  = "analyzer",
  app_dir   = "E:\\Dropbox\\R Projects\\analyzeR",
  dir_out   = "Standalone",
  include_R = TRUE,
  pkgs      = c("shiny", "plotly", "dplyr", "reshape2", "quantmod", "XLConnect", "DR", "lubridate"),
  remotes   = c("daattali/shinyalert")
  )
compile_iss()