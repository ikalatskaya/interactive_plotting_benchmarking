### NOTES: add plot description as tooltips
## Boxplot is a convenient way of graphically depicting groups of numerical data through their quartiles
# https://chartio.com/learn/charts/stacked-bar-chart-complete-guide/
# The main objective of a standard bar chart is to compare numeric values between levels of a categorical variable. 

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyfullscreen)
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
pokemon = pokemon %>% na.omit() %>% dplyr::filter(!type_1 %in% c("steel", "psychic", "ghost", "fairy", "normal", "bug", "ice", "electric"))

plots = c("scatter", "boxplot", "barchart", "piechart") # heatmap, density
num.var = select_if(pokemon, is.numeric) %>% colnames()
char.var = select_if(pokemon, is.character) %>% colnames()
remove = c("pokemon", "image_url" , "icon_url" , "detail_url" )
char.var = char.var [! char.var %in% remove]
opacity = 0.7

#highchart_theme = c("hc_theme_chalk()", "hc_theme_economist()", "hc_theme_elementary()", "hc_theme_538()", 
#  "hc_theme_flat()", "hc_theme_ffx()", "hc_theme_google()")

# ggplot_themes = c("theme_grey", "theme_bw", "theme_classic", "theme_dark", "theme_minimal", "theme_void", "theme_version")
# https://echarts4r.john-coene.com/articles/themes.html
echart_themes = c("auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk",
                  "cool", "dark-blue", "dark-bold", "dark-digerati", "dark-fresh-cut", "dark-mushroom",
                  "dark", "eduardo", "essos", "forest", "fresh-cut", "fruit", "gray", "green",
                  "halloween", "helianthus", "infographic", "inspired", "jazz", "london", "macarons",
                  "macarons2", "mint", "purple-passion", "red-velvet", "red", "roma", "royal",
                  "sakura", "shine", "tech-blue", "vintage", "walden", "wef", "weforum", "westeros",
                  "wonderland")