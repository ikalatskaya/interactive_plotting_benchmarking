### NOTES: add plot description as tooltips
## Boxplot is a convenient way of graphically depicting groups of numerical data through their quartiles
# https://chartio.com/learn/charts/stacked-bar-chart-complete-guide/
# The main objective of a standard bar chart is to compare numeric values between levels of a categorical variable. 

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(bslib)
library(dplyr, warn.conflicts = FALSE)
library(shinyWidgets)
library(tippy)
# library(summaryBox) # cute summary box - not used here. 

# VISIALIZATION library
library(ggplot2, warn.conflicts = FALSE)
library(rbokeh)
library(plotly)
library(highcharter)
library(echarts4r)



# the dataset is coming from Highcharter R package
data(pokemon)
data(stars)

# clean pokemon dataset for easier visualization
pokemon = pokemon %>% na.omit() %>% dplyr::filter(!type_1 %in% c("steel", "psychic", "ghost", "fighting", "fairy", "normal", "bug", "ice", "electric"))

plots = c("scatter", "boxplot", "barchart", "piechart") # heatmap, density
num.var = select_if(pokemon, is.numeric) %>% colnames()
char.var = select_if(pokemon, is.character) %>% colnames()
remove = c("pokemon", "image_url" , "icon_url" , "detail_url" )
char.var = char.var [! char.var %in% remove]
opacity = 0.8

