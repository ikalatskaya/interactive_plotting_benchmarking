#' Helper functions for using Benchmarking app

spinner.colour = "#00AA51"
status = "success"


add_quotes <- function(variable) {
  return(paste0("'", variable, "'"))
}

## Visualization: ui side
# generate description box for all packages. This data is saved in sys.yml
# Arguments:
#   desc: the description of the package as a string
#   pros: a list of advantages or cool features
#   cons: a list of limitations
#   lib: R library
#   fixik: additional functions usually from shinyWidgets::pickerInput() for theme selection
# Return: shinydashboard::box
# 
descBox <- function(desc, pros, cons, lib, icon, fixik = NULL) {
  
             box(style='width:3; 
                 height:400px;
                 overflow-y: scroll;', 
                  width = 3, 
                  closable = TRUE, 
                  collapsible = TRUE,
                  solidHeader = TRUE,  
                  title = "Info box", 
                  status = status,
                  class="box",
                  p(desc),
                  h4("Pros"),
                  p(pros),
                  fixik,
                  h4("Cons"),
                  p(cons),
                  h4("Library"),
                  p(lib),
                  icon = icon
             )
}


## Visualization unit: ui side
# generates box with the plot for all packages. This data is generated in server()
# Arguments:
#   title: the name of the package stored in sysConfig$package
#   output: plot object
#   icon: used icon from https://chartio.com/learn/charts/stacked-bar-chart-complete-guide/
# Return: shinydashboard::box
# 
plotBox <- function(title, output, icon) {
  box(title = title,
                output %>% withSpinner(type = 8, color = spinner.colour),
                status = status, 
                solidHeader = TRUE,
                collapsible = TRUE, 
                class="box", 
                closable = TRUE, 
                width = 9, 
                height = 400,
                icon = icon
  )
}


## Visualization unit: ui side
# generates box with R code.
# Arguments:
#   title: the name of the package stored in sysConfig$package
#   plot: plot object
#   icon: used icon from https://chartio.com/learn/charts/stacked-bar-chart-complete-guide/
# Return: shinydashboard::box
# 
codeBox <- function(title, output) {
  box(title = title,
          output, 
          width = 12, 
          closable = TRUE, 
          id = "code_box"
      )
}

