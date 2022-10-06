# https://chartio.com/learn/charts/stacked-bar-chart-complete-guide/
# The main objective of a standard bar chart is to compare numeric values between levels of a categorical variable. 


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(plotly)
library(highcharter)
library(echarts4r)
library(dplyr)
library(rbokeh)
library(shinyWidgets)
library(tippy)
library(ggplot2)
# library(summaryBox) # cute summary box - not used here. 
library(bslib)

# the dataset is coming from Highcharter R package
data(pokemon)
data(stars)

getConfig <- function(){
  config <- yaml::read_yaml(file.path(strSRC,"sys.yml"))
  return(config)
}
strSRC <- normalizePath(getwd())
sysConfig <- getConfig()


# clean pokemon dataset for easier visualization
pokemon = pokemon %>% na.omit() %>% dplyr::filter(!type_1 %in% c("steel", "psychic", "ghost", "fighting", "fairy", "normal", "bug", "ice", "electric"))


num.var = select_if(pokemon, is.numeric) %>% colnames()
char.var = select_if(pokemon, is.character) %>% colnames()
remove = c("pokemon", "image_url" , "icon_url" , "detail_url" )
char.var = char.var [! char.var %in% remove]
status = "success"
opacity = 0.8
spinner.colour = "#00AA51"


ui <- dashboardPage(
  
  dashboardHeader(
    disable = TRUE, # remove completely
    # controlbarIcon = shiny::icon("rectangle-list", verify_fa = FALSE),
    fixed = TRUE
  ),
  
  
  
  ## Footer ----
  footer = dashboardFooter(
        h5("Done by Irina Kalatskaya")
  ),
  
  sidebar <- dashboardSidebar(
    id = "sidebar",
    
    # this fluidPage is inside of Sidebar and needed to keep title tabed.
    fluidPage(
      # theme = bslib::bs_theme(bootswatch = "flatly"),
      h3("Attribute toolbox"),
      #sidebarMenu(),
      #sidebarUserPanel(),
      #sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
      collapsed = FALSE,
      disable = FALSE,
      width = 300,
      minified = TRUE,
      shinyWidgets::pickerInput(inputId = "set", 
                                label = h4("Select dataset"), 
                                choices = c("pokemon"), selected="pokemon") 
      %>% tippy::tippy(allowHTML = TRUE, "<span style='font-size:16px;'>This dataset contains information on all 802 Pokemon from all Seven Generations of Pokemon. The information was scraped from http://serebii.net/. The dataset is a part of highcharter R package.</span>"),
      br(),
      shinyWidgets::pickerInput(inputId = "type", 
                                label = h4("Select plot type"), 
                                choices = c("scatter", "boxplot", "barchart"), 
                                selected="scatter"),

      # c("density", "barplot")
      
      conditionalPanel( condition = "input.type == 'scatter'",
                      shinyWidgets::pickerInput(inputId = "varX", label = h5("Select x value"), choices = num.var, selected="height"),
      ),
      conditionalPanel( condition = "input.type == 'scatter' | input.type == 'boxplot'",
                      shinyWidgets::pickerInput(inputId = "varY", label = h5("Select y value"), choices = num.var, selected="weight"),
                      sliderInput(inputId = "dotsize", label = h5("Choose the size of the points"), value = 7, width = NULL, min = 1, max = 20, step = 1)
      ),
      
#     conditionalPanel( condition = "input.type == 'boxplot'",
#                      shinyWidgets::prettyCheckbox(inputId = "nestedBoxplot", label = "Generate nested boxplot?")),
    
#     conditionalPanel( condition = "input.nestedBoxplot == 1",
#                      shinyWidgets::pickerInput(inputId = "varZ1", label = h5("Select value for nesting"), choices = char.var, selected="type_1"),           
#     ),


      shinyWidgets::pickerInput(inputId = "varZ", label= h5("Group and colour by"), choices = char.var, selected="type_1"),
      
      conditionalPanel( condition = "input.type == 'barchart'",
                        shinyWidgets::pickerInput(inputId = "varA", label= h5("Add another cat value "), choices = char.var, selected="type_2")
                        # shinyWidgets::prettyCheckbox(inputId = "isPercentStackedBar", label = "To Percentage stacked bar chart?", value = FALSE)
      ),
                        
      shinyWidgets::prettyCheckbox(inputId = "isLegend", label = "Remove legend?"),
      br()
    ) # end of fluidPage
  ),




  dashboardBody(
    
    tags$head(
      includeCSS('style.css')
    ),
    
#    tags$style(HTML("
#      #code_box {
#          border: 3px solid grey;
#      }
#      #unit {
#          border: 4px double green;
#      }
#    ")),
    
    setShadow(class = "box"),
    setShadow(class = "infoBox"),
    theme = bs_theme(version = 4, bootswatch = "minty"),
    
    
    ##################################
    ## InfoBox
    #################################
    titlePanel(title = "Compare Five Interactive Visualization Tools in R:"),
    br(),
    br(),
    fluidRow(
      
      infoBox("Long lasting RShiny apps", value = NULL, subtitle = " should be interactive and self-explicit. R packages that support html widgets might help achive this goal pretty easily.", icon = shiny::icon("diagram-project", lib = "font-awesome"), 
              color = "navy", width = 6, href = NULL, fill = TRUE),
      infoBox("Visualizations grant users ", value = NULL, subtitle = " the ability to explore, manipulate, and interact with data by employing dynamic charts, changing colors, and shapes. ",
              icon = shiny::icon("circle-user", verify_fa = FALSE), color = "green", width = 6, href = NULL, fill = TRUE)
      
    ),
    
    fluidRow(
      infoBox("HTML widgets ", subtitle = "can be used at the R console as well as embedded in R Markdown reports and Shiny web applications.",
              icon = shiny::icon("file-image", verify_fa = FALSE), color = "green", width = 6, href = NULL, fill = TRUE),
      infoBox("HTML widgets ", value = NULL, subtitle = " work just like R plots except they produce interactive web visualizations. ",
              icon = shiny::icon("chart-area", verify_fa = FALSE), color = "navy", width = 6, href = NULL, fill = TRUE)
    ),
    
    ###################################
    ## PLOTLY
    ###################################
    
    fluidPage(width = 12, height = 400, id = "unit",
              br(),
              box(title = sysConfig$package$PLOTLY$name,
                  plotlyOutput("plotly") %>% withSpinner(type  = 1, color = spinner.colour),
                  status = status, solidHeader = TRUE,
                  collapsible = TRUE, class="box", closable = TRUE, width = 9, 
                  br(), height = 400
              ),
              
              box(closable = TRUE, width = 3, solidHeader = TRUE, height = 400,
                  title = "Info box", status = status,
                  collapsible = TRUE,
                  class="box",
                  p(sysConfig$package$PLOTLY$description),
                  h4("Pros"),
                  p(sysConfig$package$PLOTLY$pros),
                  h4("Cons"),
                  p(sysConfig$package$PLOTLY$cons),
                  h4("Library"),
                  p("library(plotly)")
              ),
              box(
                verbatimTextOutput("plotly_code"), width = 12, closable = TRUE, id = "code_box", title = "Code"),
              br()
    ),
    
    br(),
    
    ####################################
    ## GGPLOTLY
    ####################################
    fluidPage(width = 12, height = 400,  id = "unit",
              br(),
              box(
                title = sysConfig$package$GGPLOTLY$name,
                plotlyOutput(("myggplotly")) %>% withSpinner(type  = 1, color = spinner.colour),
                closable = TRUE,  width = 9, 
                title = "", status = status, 
                solidHeader = TRUE, 
                collapsible = TRUE, 
                class="box"
              ),
              
              box(closable = TRUE, width = 3, solidHeader = TRUE, height = 400,
                  title = "Info box", status = status,
                  collapsible = TRUE,
                  class="box",
                  p("Extension of the ggplo2 package."),
                  h4("Pros"),
                  p("(1) Free; 
                    (2) Worked with ggplot objects; 
                    (3) Tooltips for x, y, and colour are automatically added; (4) Interactive legends; (5) Many different themes are available:"),
                  shinyWidgets::pickerInput(inputId = "ggplot_theme", 
                                            label = h4("Select ggplot theme"), 
                                            choices = c("theme_grey", "theme_bw", "theme_classic", "theme_dark", "theme_minimal", "theme_void", "theme_version"), 
                                            selected="theme_bw"),
                  h4("Cons"),
                  p("(1) This package doesn't always generate a nice professional look; (2) Plot subsetting using legends doesn't lead to the rescaling of x- and y-axes."),
                  h4("Library"),
                  p("library(ggplot2)")
              ),
              box(
                verbatimTextOutput("ggplotly_code", placeholder = TRUE), width = 12, closable = TRUE, id = "code_box", title = "Code"
              )
    ),
    br(),
    
    ###################################
    ## HIGHCHART
    ###################################
    fluidPage( id = "unit", 
               br(),
               box(highchartOutput("highchart") %>% withSpinner(type  = 1, color = spinner.colour),  
                   title = "Highchart R package", 
                   closable = TRUE, width = 9, collapsible = TRUE, class="box",
                   status = status, solidHeader = TRUE, class="box", height = 400),
               
               box(style='width:3; height:400px;overflow-y: scroll;',

                 closable = TRUE, width = 3, solidHeader = TRUE, height = 420,
                   title = "Info box", status = status,
                   collapsible = TRUE,
                   class="box",
                   p("Highcharts (http://www.highcharts.com/) is a charting library written in pure JavaScript, 
          offering an easy way of adding interactive charts to your web site or web application with a simple configuration syntax. 
          Highcharts currently supports line, spline, area, areaspline, column, bar, pie, scatter, angular gauges, 
          arearange, areasplinerange, columnrange, bubble, box plot, error bars, funnel, waterfall and polar chart types. 
          Highcharter R package is a wrapper for the ‘Highcharts’ library including shortcut functions to plot R objects. 
          It was created by Highsoft in Norway (2009)."),
                   h4("Pros"),
                   p("(1) Very professionally looking plots; (2) The legends are filling available space in the most efficient way, (3) The legends are interactive; (4) Different themes are available:"),
                   
                   shinyWidgets::pickerInput(inputId = "highchart_theme", 
                                             label = h4("Select highchart theme"), 
                                             choices = c("hc_theme_chalk()", "hc_theme_economist()", "hc_theme_elementary()", "hc_theme_538()", "hc_theme_flat()", "hc_theme_ffx()", "hc_theme_google()"), 
                                             selected="hc_theme_google()"),
                   
                   h4("Cons"),
                   p(" (1) Highcharts is a Highsoft product which is not free for commercial and Governmental use; (2) Tooltips could be added using JS."),
                   h4("Library"),
                   p("library(highcharter)")
               ),
               box(
                 verbatimTextOutput("highcharter_code", placeholder = TRUE), width = 12, closable = TRUE, id = "code_box", title = "Code"),
               br()
    ),
    br(),
    


    ####################################
    ## RBOKEH
    ####################################
    fluidPage(width = 12, height = 400,  id = "unit",
              
              br(),
              box(rbokehOutput("rbokeh") %>% withSpinner(type  = 1, color = spinner.colour), 
                  width = 9, title = "Bokeh R package", status = status, 
                  solidHeader = TRUE, collapsible = TRUE, class="box"),
              
              box(closable = TRUE, width = 3, solidHeader = TRUE, height = 400,
                  title = "Info box", status = status,
                  collapsible = TRUE,
                  class="box",
                  p("A native R plotting library that provides a flexible declarative interface for creating interactive web-based graphics, 
            backed by the Bokeh visualization library. The Bokeh library is written and maintained by the Bokeh Core Team consisting of several members of Continuum Analytics and other members of the open source community. "),
                  h4("Pros"),
                  p("(1) Easy to plot; (2) Easy to add tooltips; (3) Free; (4) Overall nice professional look. "),
                  h4("Cons"),
                  p("(1) The legend is covering the plot and not movable (solution was posted only for python lib; (2) Legends are not interactive. "),
                  h4("Library"),
                  p("library(rbokeh)")
              ),
              box(
                verbatimTextOutput("brokeh_code", placeholder = TRUE), width = 12, closable = TRUE, id = "code_box", title = "Code"
              )
    ),
    
    br(),
    
    ####################################
    ## ECHART
    ####################################
    
    
    fluidPage(width = 12, height = 400,  id = "unit",
              br(),
              box(echarts4rOutput("echart") %>% withSpinner(type  = 1, color = spinner.colour), 
                  width = 9, title = "Echart R package", status = status, 
                  solidHeader = TRUE, collapsible = TRUE, class="box"),
              
              box(closable = TRUE, width = 3, solidHeader = TRUE, height = 400,
                  title = "Info box", status = status,
                  collapsible = TRUE,
                  class="box",
                  p("Easily create interactive charts by leveraging the 'Echarts Javascript' library which includes 36 chart types, themes, 'Shiny' proxies and animations."),
                  h4("Pros"),
                  p("(1) Professional look; (2) Good collection of default colours; (3) Easy to add tooltips; (4) Interactive legends; (5) Rich collection of themes."),
                  
                  # https://echarts4r.john-coene.com/articles/themes.html
                  shinyWidgets::pickerInput(inputId = "ehcart_theme", 
                                            label = h4("Select echart theme"), 
                                            choices = c("auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk",
                                                        "cool", "dark-blue", "dark-bold", "dark-digerati", "dark-fresh-cut", "dark-mushroom",
                                                        "dark", "eduardo", "essos", "forest", "fresh-cut", "fruit", "gray", "green",
                                                        "halloween", "helianthus", "infographic", "inspired", "jazz", "london", "macarons",
                                                        "macarons2", "mint", "purple-passion", "red-velvet", "red", "roma", "royal",
                                                        "sakura", "shine", "tech-blue", "vintage", "walden", "wef", "weforum", "westeros",
                                                        "wonderland"), 
                                            selected=NULL),
                  
                  h4("Cons"),
                  p("(1) Relatively hard to build; (2) Stacked barcharts are tricky. "),
                  h4("Library"),
                  p("library(echarts4r)")
              ),
              box(
                verbatimTextOutput("echart_code", placeholder = TRUE), width = 12, closable = TRUE, id = "code_box", title = "Code"
              )
              
    ),
    
    
    fluidPage(
      # theme  = bslib::bs_theme(bootswatch = "darkly"),
      br(),
      br(),
      column(12, solidHeader = TRUE, status = "success", 
             dataTableOutput('table'), extensions = c('Responsive')),
      br()
    )
  )
)





server <- function(input, output, session) {
  
  #thematic::thematic_shiny()
  
  observe({
    updatePickerInput(session, inputId = "type", choices = c("scatter", "boxplot", "barchart"), selected = "scatter")
    # c( "density", "barplot", "boxplot", "piechart")
  })
  
  observe({ updatePickerInput(session, inputId = "varX") })
  observe({ updatePickerInput(session, inputId = "varY")})
  observe({ updatePickerInput(session, inputId = "varZ") })
  observe({ updatePickerInput(session, inputId = "varA") })
  observe ({ updatePrettyCheckbox(session, inputId =  "isLegend")})
  
  
  
  output$table <- renderDataTable({
    pokemon %>% dplyr::select(id, pokemon, input$varX, input$varY, input$varZ)
  })
  
  ###################################
  ## GGPLOTLY
  ###################################
  output$myggplotly <- renderPlotly({
    
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    pokemon$aa = pokemon[[input$varA]]
    
    pokemon$zz = as.factor(pokemon$zz)
    
    if(input$type == "scatter") {
      plot = pokemon %>% ggplot(., aes(xx, yy, color=zz)) + geom_point(size=input$dotsize) + labs(x = input$varX, y = input$varY, title="ggplotly: scatter plot example")
      # plot = plot + geom_smooth()
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% ggplot(., aes(y = xx, x = zz, colour = zz)) + ggplot2::geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(y = input$varY, x = "", title = "ggplotly: boxplot" )
      # + coord_flip()
    }
    else if(input$type == "barchart") {
      plot = pokemon %>% dplyr::count(zz, aa) %>% ggplot(., aes(n, zz, fill=aa)) + geom_bar(stat="identity") + ggtitle("Barplot with ggplotly") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab(input$varZ)
      ggplotly(plot)
    }
    
    else if(input$type == "piechart") {
      plot = pokemon %>% dplyr::count(zz, aa) %>% ggplot(., aes(x="", y=n, fill=zz, colour = aa)) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + geom_text(aes(label=zz), position = position_stack(vjust=0.5), color = "white") + labs(title = "ggplotly: piechart")
      #                                                                           + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + geom_text(aes(label=type_1), position = position_stack(vjust=0.5), color = "white") + theme_void() %>% labs(title = "Pokemon types <i> (ggplot) </i>")
      
    }
    
    
    
    else if(input$type == "density") {
      plot = pokemon %>% ggplot(., aes(xx, colour = zz)) + geom_density(alpha=opacity) + labs(x = input$varX, title="ggplotly: boxplot example")
    }
    
    
    plot = plot + theme(legend.title= element_blank())
    
    if(input$ggplot_theme == "theme_light") {
      plot = plot + theme_light()
    }
    else if(input$ggplot_theme == "theme_grey") {
      plot = plot + theme_grey()
    }
    else if(input$ggplot_theme == "theme_void") {
      plot = plot + theme_void()
    }
    else if(input$ggplot_theme == "theme_minimal") {
      plot = plot + theme_minimal()
    }
    else if(input$ggplot_theme == "theme_dark") {
      plot = plot + theme_dark()
    }
    else if(input$ggplot_theme == "theme_bw") {
      plot = plot + theme_bw()
    }
    else if(input$ggplot_theme == "theme_classic") {
      plot = plot + theme_classic()
    }
    
    if(input$isLegend) {
      plot = plot + theme(legend.position = "none")  
    }
    
    plot
    
  })
  
  
  output$ggplotly_code <- renderText({
    if(input$type == "scatter") {
      # string = 'ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point(size=3) + labs(x = "length", y = "width", title="iris database"))'
      string = paste0('pokemon %>% ggplot(., aes(', input$varX, ', ', input$varY, ', color=', input$varZ, ')) + geom_point(size=', input$dotsize, ') + labs(x = ', input$varX, ', y = ', input$varY, ', title="ggplotly: scatter plot example")')
    }
    else if(input$type == "boxplot") {
      string = paste0(' pokemon %>% ggplot(., aes(y = ', input$varX, ', x = ', input$varZ, ', colour = ', input$varZ, ')) + ggplot2::geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(y =', input$varY, ', x = "", title = "ggplotly: boxplot" )')
      # string = 'ggplot(iris, aes(y = Sepal.Length, colour = Species, fill=Species)) + geom_boxplot(alpha=0.8) + labs(x = "Length")  + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(x = "Sepal length", title = " iris database" ) '
    }
    else if(input$type == "density") {
      string = 'ggplot(iris, aes(Sepal.Length, colour = Species, fill=Species)) + geom_density(alpha=0.8) + labs(x = "Length"))'
    }
    else if(input$type == "barchart") {
      string = paste0('pokemon %>% dplyr::count(', input$varZ, ', ', input$varA, ') %>% ggplot(., aes(n, ', input$varZ, ', fill=', input$varA, ')) + geom_bar(stat="identity") + ggtitle("Barplot with ggplotly") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab(', input$varZ, ') ')
    }
    else if(input$type == "piechart") {
      string = 'ggplot(iris %>% dplyr::count(Species), aes(x="", y=n, fill=Species)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)'
    }
    
    
    
    if(input$isLegend) {
      string = paste0(string, 'theme(legend.position = "none")')
    }
    paste0("ggplotly(", string, ")")
  })
  
  
  
  ###################################
  ## Plotly
  ###################################
  output$plotly <- renderPlotly({
    
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    pokemon$aa = pokemon[[input$varA]]
    
    if(input$type == "scatter") {
      plot = pokemon %>% plot_ly(colors = RColorBrewer::brewer.pal(8, "Set2")) %>% add_trace(x=~xx, y=~yy, color=~zz, type="scatter", mode="markers", opacity=opacity, marker = list(size = input$dotsize), text = ~paste(" ID", id, "\n", "pokemon", pokemon, "\n", "type: ", type_1,  "\n", "Egg group: ", egg_group_1), hoverinfo=c("text")) %>% layout(title = "Plotly: scatter plot example", xaxis = list(title = input$varX), yaxis = list(title = input$varY ))
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% plot_ly(colors = "Set3", type = "box", x =~xx, color=~zz, boxpoints="all", jitter=0.5, pointpos = 0, text = ~paste(" ID", id, "\n", "pokemon", pokemon, "\n", "type: ", type_1,  "\n", "Egg group: ", egg_group_1), hoverinfo=c("text")) %>% layout(title="Plotly: boxplot example") 
    }
    
    else if(input$type == "density") {
      plot = plot_ly(pokemon, x = ~xx,  type = 'violin', side = "positive") %>% layout(barmode="overlay") %>% layout(title = "type = 'violin'", xaxis = list(title = input$varX))
    }
    
    else if(input$type == "piechart") {
      plot = pokemon %>% dplyr::count(zz) %>% plot_ly(labels = ~zz, values=~n, type="pie", colors = RColorBrewer::brewer.pal(8, "Set2"), textposition = 'outside', textinfo = 'label+percent', hole = 0.2) %>% layout(title = "Plotly: piechart", xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    else if(input$type == "barchart") {
      plot = pokemon  %>% dplyr::count(zz, aa) %>% plot_ly(colors = RColorBrewer::brewer.pal(8, "Set2")) %>% add_trace(type='bar', x =~n, y=~zz, color=~aa, text = ~paste(zz, '\n', aa, '\n', 'Number ', n), hoverinfo=c('text')) %>% layout(barmode = 'stack', margin=list(l=240)) %>% layout(title = 'Barchart by <i>plot_ly</i>', yaxis = list(title = input$varZ))
    }
    
    
    if(input$isLegend) {
      plot = plot %>% layout(showlegend = FALSE)
    }
    plot
  })
  
  
  output$plotly_code <- renderText({
    if(input$type == "scatter") {
      string = paste0('pokemon %>% plot_ly %>% add_trace(x=~', input$varX, ', y=~', input$varY, ', color=~', input$varZ, ' , type="scatter", mode="markers", opacity=opacity, marker = list(size = ', input$dotsize, '), text = ~paste(" ID", id, "pokemon", pokemon, "type: ", type_1,  "Egg group: ", egg_group_1), hoverinfo=c("text")) %>% layout(title = "Plotly: scatter plot example", xaxis = list(title = ', input$varX, '))')
    }
    else if(input$type == "boxplot") {
      string = paste0('pokemon %>% plot_ly(type = "box", x =~', input$varX, ', color=~', input$varZ, ', boxpoints="all", jitter=0.5, pointpos = 0, text = ~paste(" ID", id, "pokemon", pokemon, "type: ", type_1,  "Egg group: ", egg_group_1), hoverinfo=c("text")) %>% layout(title="Plotly: boxplot example") ')
    }
#    else if(input$type == "density") {
#      #data = pokemon %>% dplyr::select(xx, yy, zz, pokemon) %>% reshape2::melt(., value.name = "score", id.var = "pokemon")
#      string = plot_ly(pokemon, x = ~xx,  type = 'violin', side = "positive") %>% layout(barmode="overlay") %>% layout(title = "type = 'violin'", xaxis = list(title = input$varX))
#    }
    else if(input$type == "piechart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ") %>% plot_ly(labels = ~", input$varZ, ", values=~n, type='pie', textposition = 'outside', textinfo = 'label+percent', hole = 0.2) %>% layout(title = 'Plotly: piechart', xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))")
    }
    else if(input$type == "barchart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ", ", input$varA, ") %>% plot_ly() %>% add_trace(type='bar', x =~n, y=~", input$varZ, ", color=~", input$varA, ", text = ~paste(", input$varZ, ", ", input$varA, ", 'Number ', n), hoverinfo=c('text')) %>% layout(barmode = 'stack', margin=list(l=240)) %>% layout(title = 'Barchart by <i>plot_ly</i>', yaxis = list(title = ", input$varZ, "))")
    }
    
    
    if(input$isLegend) {
      string = paste0(string, "%>% layout(showlegend = FALSE)")
    }
    string
    
  })
  
  
  ###################################
  ## HIGHCHART
  ###################################
  output$highchart <- renderHighchart2({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    pokemon$aa = pokemon[[input$varA]]
    
    if(input$type == "scatter") {
      hcplot = highchart(type = "chart") %>% hc_add_series(pokemon, "scatter", hcaes(x = xx, y = yy, group = zz)) %>% hc_title(text = 'Highcharter: scatter plot', style = list(fontWeight = 'bold')) %>% hc_yAxis(title = list(text = input$varY), allowDecimals = FALSE) %>% hc_tooltip(formatter = JS("function(){ return ('pokemon: ' + this.point.pokemon + ' <br> id: ' + this.point.id + '<br> base: ' + this.point.base_experience)}")) %>% hc_plotOptions(bubble = list(maxSize = '100%', marker =list( size=input$dotsize))) %>% hc_xAxis(title = list(text = input$varX), allowDecimals = FALSE)  
    }
    else if(input$type == "barchart") {
      hcplot = pokemon %>% dplyr::count(zz, aa) %>% hchart(type = 'bar', hcaes(x = zz, y = n, group=aa)) %>% hc_plotOptions(bar=list(stacking='stack')) %>% hc_title(text = 'Barplot by <i>Highcharter </i> ', margin = 20, align = "left", style = list(color = '#22A884', useHTML = TRUE)) %>% hc_xAxis(title=list(text=input$varZ)) 
    }
    else if(input$type == "boxplot") {
      plot = data_to_boxplot(pokemon,  variable = xx, group_var = zz, add_outliers = TRUE)
      hcplot = highchart() %>% hc_xAxis(type = 'category') %>% hc_add_series_list(plot) %>% hc_add_series(data = pokemon, type = "scatter", hcaes(x = zz, y = xx, group=zz)) %>%  hc_plotOptions(scatter=list(jitter=list(x=0.08, y=0))) %>% hc_title(text = 'Highchater: boxplots') %>% hc_plotOptions(scatter=list(marker=list(radius=2, symbol = 'circle', lineWidth=1)))  %>% hc_xAxis(title = list(text = input$varX), allowDecimals = FALSE) 
    }
#    else if(input$type == "piechart") {
#      hcplot = pokemon %>% dplyr::count(zz) %>% hchart(type ="pie", hcaes(zz, y = n)) %>% hc_title(text = "type = 'pie'")
#    }
#    else if(input$type == "density") {
#      hcplot = pokemon %>% hchart(density(xx, na.rm = TRUE), type = "area", name = xx ) %>% hc_title(text = " type ='area' ", stype=list(fontWeight = "bold"))
#    }
    
    
    if(input$isLegend) {
      hcplot =  hcplot %>% hc_legend(enabled = F)
    }
    
    hcplot = hcplot %>% hc_credits(enabled = TRUE, text = "Sources: highchater 2022", style = list(fontSize = "10px"), enabled = TRUE) 
    
    
    
    if(input$highchart_theme == "highchart_theme()") {
      hcplot = hcplot %>% hc_add_theme(highchart_theme())
    }
    else if(input$highchart_theme == "hc_theme_chalk()") {
      hcplot = hcplot %>% hc_add_theme(hc_theme_chalk())
    }
    else if(input$highchart_theme == "hc_theme_economist()") {
      hcplot = hcplot %>% hc_add_theme(hc_theme_economist())
    }
    else if(input$highchart_theme == "hc_theme_elementary()") {
      hcplot = hcplot %>% hc_add_theme(hc_theme_elementary())
    }
    else if(input$highchart_theme == "hc_theme_538()") {
      hcplot = hcplot %>% hc_add_theme(hc_theme_538())
    }
    else if(input$highchart_theme == "hc_theme_flat()") {
      hcplot = hcplot %>% hc_add_theme(hc_theme_flat())
    }
    else if(input$highchart_theme == "hc_theme_ffx()") {
      hcplot = hcplot %>% hc_add_theme(hc_theme_ffx())
    }
    else if(input$highchart_theme == "hc_theme_google()") {
      hcplot = hcplot %>% hc_add_theme(hc_theme_google())
    }
    
    hcplot
    
  })
  
  
  output$highcharter_code <- renderText({
    if(input$type == "scatter") {
      tooltips = "function(){ return ('pokemon: ' + this.point.pokemon + ' <br> id: ' + this.point.id + '<br> base: ' + this.point.base_experience)}"
      string = paste0 ( " highchart() %>% hc_add_series(pokemon, 'scatter', hcaes(x = ", input$xx, "y = ", input$varY,  " group = ", input$varZ, ")) %>% hc_title(text = 'Highcharter: scatter plot', style = list(fontWeight = 'bold')) %>% hc_yAxis(title = list(text =", input$varY, "), allowDecimals = FALSE) %>% hc_tooltip(formatter = JS(tooltip)) %>% hc_plotOptions(bubble = list(maxSize = '100%')) ")
    }
    else if(input$type == "boxplot") {
      string = paste0("plot = data_to_boxplot(pokemon,  variable = ", input$varX, "group_var =", input$varZ, ", add_outliers = TRUE)", 
                      "\n",
                      " highchart() %>% hc_xAxis(type = 'category') %>% hc_add_series_list(plot) %>% hc_add_series(data = pokemon, type = 'scatter', hcaes(x = zz, y = xx, group = zz)) 
                      %>% hc_plotOptions(scatter=list(jitter=list(x=0.08, y=0))) %>% hc_title(text = 'Highchater: boxplots') %>% hc_plotOptions(scatter=list(marker=list(radius=5, symbol = 'circle', lineWidth=1)))"
                      
     )
    }
    else if(input$type == "barchart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ", ", input$varA, ") %>% hchart(type = 'bar', hcaes(x = ", input$varZ, ", y = n, group=", input$varA, ")) %>% hc_plotOptions(bar=list(stacking='stack')) %>% hc_title(text = 'Barplot by <i>Highcharter </i> ', margin = 20, align = 'left', style = list(color = '#22A884', useHTML = TRUE)) %>% hc_xAxis(title=list(text=", input$varZ, ")) ") 
    }
    
    string = paste0(string,  " %>% ", input$highchart_theme)
    string
    
  })
  
  
  
  ###################################################
  ## rbokeh
  ###################################################
  
  output$rbokeh <- renderRbokeh({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    pokemon$aa = pokemon[[input$varA]]
    w = 1000
    h = 400
    
    if(input$type == "scatter") {
      if(input$isLegend) {
        plot = figure(title ="Bokeh: scatter plot example", width = w, height = h, legend_location = NULL) %>% ly_points(x = xx, y = yy, color = zz, data = pokemon, hover = char.var, size = input$dotsize) %>% x_axis(label = input$varX) %>% y_axis(label = input$varY)
      }
      else {
        plot = figure(title ="Bokeh: scatter plot example", width = w, height = h) %>% ly_points(x = xx, y = yy, color = zz, data = pokemon, hover = char.var, size = input$dotsize) %>% x_axis(label = input$varX) %>% y_axis(label = input$varY)
      }
    }
    
    else if(input$type == "boxplot") {
      if(input$isLegend) {
        plot = figure(title ="Bokeh: ly_boxplot", width = w, height = h, legend_location = NULL)
      }
      else {
        plot = figure(title ="Bokeh: ly_boxplot", width = w, height = h, legend_location = "top_right")
      }
      plot = plot %>% ly_boxplot(xx, zz, color = zz, data = pokemon, alpha = opacity) %>% y_axis(label = input$varY) %>% x_axis(label = "")
    }
    
    else if(input$type == "barchart") {
      data = pokemon %>% dplyr::count(zz, aa)
      if(input$isLegend) {
          plot = figure(title ='Barchart by Bokeh', width = w, height = h, legend_location = NULL) 
      }
      else {
          plot = figure(title ='Barchart by Bokeh', width = w, height = h, legend_location = "top_right") 
      }
      plot = plot %>% ly_bar(zz, n, color = aa,  data = data, hover = TRUE) %>% theme_axis("x", major_label_orientation = 90) %>% y_axis(label = input$varZ) %>% x_axis(label = "")
    }
    #else if(input$type == "density") {
    #   plot = figure(title ="Bokeh: ly_density", width = w, height = h) %>% ly_density(xx, zz, data = pokemon)
    #}
    plot 
    
  })
  
  output$brokeh_code <- renderText({
    if(input$type == "scatter") {
      string = paste0('figure(title = "Bokeh: ly_points", width = 1000, height = 400, legend_location = "top_right") %>% ly_points(x = ', input$varX, ', y = ', input$varY, ', color = ', input$varZ, ', data = pokemon, hover = list.of.var.to.show, size =',  input$dotsize, ') %>% x_axis(label = ', input$varX, ') %>% x_axis(label = ', input$varY, ')')
    }
    else if(input$type == "boxplot") {
      string = paste0('figure(title = "Bokeh: ly_boxplot", width = 1000, height = 400, legend_location = "top_right") %>% ly_boxplot(xx, ', input$varZ, ', color = zz, data = pokemon, alpha = 0.8) %>% y_axis(label = ', input$varY, ')')
    }
    else if(input$type == "barchart") {
      string = paste0("figure(title = 'Barchart by Bokeh', width = 1000, height = 400, legend_location = 'top_right') %>% ly_bar(", input$varZ, ", n, data = pokemon %>% dplyr::count(", input$varZ, ", sort=T), hover = TRUE) %>% theme_axis('x', major_label_orientation = 90) %>% x_axis(label = input$varZ)")
    }
    
    string
    
  })
  
  ################################################################
  ## Echart
  ###############################################################
  
  output$echart <- renderEcharts4r({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    pokemon$aa = pokemon[[input$varA]]
    
    if(input$type == "scatter") {
      plot = pokemon %>% group_by(zz) %>% e_charts(xx) %>% e_scatter(yy, symbol_size = input$dotsize) %>% e_axis_labels(x = input$varX, y = input$varY) %>% e_title(text = "Echart for R: scatter plot") %>% e_legend(orient = 'vertical', right = '5', top = '15%')
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% group_by(zz) %>% e_charts() %>% e_boxplot(xx, outliers = TRUE) %>% e_title(text = 'Echart: boxplot plot') 
      # %>% e_scatter(xx, symbol_size = input$dotsize, weightsymbol_size = 15) - add dots
    }
    ### https://echarts4r.john-coene.com/articles/advanced.html#stacked
    else if(input$type == "barchart") {
      plot = pokemon %>% dplyr::count(zz, aa) %>% e_charts(zz) %>% e_bar(n, stack = 'grp') %>% e_title(text = 'Echart: barplot example') 
    }
    # e_brush() adds selection tools
    plot = plot %>% e_theme(name = input$ehcart_theme) %>% e_tooltip() %>% e_brush()
  })
  
  output$echart_code <- renderText({
    if(input$type == "scatter") {
      string = paste0("pokemon %>% group_by(", input$varZ, ") %>% e_charts(", input$varX, ") %>% e_scatter(", input$varY, "symbol_size = ", input$dotsize, ") %>% e_axis_labels(x = ", input$varX, ", y = ", input$varY, ") %>% e_title(text = 'Echart: scatter plot') %>% e_legend(orient = 'vertical', right = '5', top = '15%') %>% e_theme(name = input$ehcart_theme) %>% e_tooltip() %>% e_brush()")
    }
    else if(input$type == "boxplot") {
      string = paste0("pokemon %>% group_by(", input$varZ, ") %>% e_charts() %>% e_boxplot(", input$varX, ", outliers = FALSE) %>% e_title(text = 'Echart: boxplot') %>% e_theme(name = input$ehcart_theme) %>% e_tooltip() %>% e_brush()")
    }
    else if(input$type == "barchart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ") %>% e_charts(", input$varZ, ") %>% e_bar(n, stack = 'grp') %>% e_theme(name = input$ehcart_theme) %>% e_tooltip() %>% e_brush()")
    }
    string
  })
  
}


shinyApp(ui = ui, server = server)

