
library(plumber)

xposome_api <- plumber::plumb("api_functions.R")
xposome_api$run(host='0.0.0.0', port=3838)