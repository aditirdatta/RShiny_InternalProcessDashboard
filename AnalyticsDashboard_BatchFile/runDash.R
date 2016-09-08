## this code allows to run shiny app in a batch mode
# use callR.bat to execute runDash.R in R (make sure R installation path is correct)
# use runDash.R to execute the shiny commands and load shiny library
# use run.bat to call the Shiny app in minimized mode

require(shiny)

## make sure your application path is correct, u can also use relative path
folder_address = 'U://ads//Rscripts'
runApp(folder_address, launch.browser=TRUE, host = "0.0.0.0", port = 6325)