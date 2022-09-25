## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(highcharter)
library(wesanderson)
library(echarts4r)
library(dplyr)
library(rbokeh)
library(shinyWidgets)
library(tippy)
library(ggplot2)
library(summaryBox)
library(bslib)

data(pokemon)



## initial global variables ----
getConfig <- function(){
  config <- yaml::read_yaml(file.path(strSRC,"Rshiny/sys.yml"))
  return(config)
}

strSRC <- normalizePath(getwd())
# sysConfig <- getConfig()

pokemon = pokemon %>% na.omit() %>% dplyr::filter(!type_1 %in% c("steel", "psychic", "ghost", "fighting", "fairy", "normal"))

num.var = select_if(pokemon, is.numeric) %>% colnames()
char.var = select_if(pokemon, is.character) %>% colnames()
remove = c("pokemon", "image_url" , "icon_url" , "detail_url"    )
char.var = char.var [! char.var %in% remove]
status = "success"
opacity=0.8


ui <- dashboardPage(
  
  dashboardHeader(
    disable = TRUE,
    controlbarIcon = shiny::icon("rectangle-list", verify_fa = FALSE),
    fixed = TRUE
  ),
  
  ## Footer ----
  footer = dashboardFooter(),

  sidebar <- dashboardSidebar(
    id = "sidebar",
    title = h2("  Attribute toolbox"),
    
    #sidebarUserPanel(),
    #sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
    collapsed = FALSE,
    disable = FALSE,
    width = 300,
    minified = TRUE,
    # sidebarMenu(),
    shinyWidgets::pickerInput(inputId = "set", label = h4("Select dataset"), choices = c("pokemon"), selected="pokemon"),
    br(),
    shinyWidgets::pickerInput(inputId = "type", label = h4("Select plot type"), choices = c("scatter", "boxplot"), selected="scatter"),
    # c("scatter", "density", "barplot", "boxplot", "piechart")
    br(),
    shinyWidgets::pickerInput(inputId = "varX", label = h5("Select x value"), choices = num.var, selected="height"),
    
    conditionalPanel( condition = "input.type == 'scatter'",
        shinyWidgets::pickerInput(inputId = "varY", label = h5("Select y value"), choices = num.var, selected="weight"),
        sliderInput(inputId = "dotsize", label = h5("Choose the size of the points"), value = 7, width = NULL, min = 1, max = 20, step = 1)
    ),
    conditionalPanel( condition = "input.type == 'boxplot'",
        shinyWidgets::prettyCheckbox(inputId = "nestedBoxplot", label = "Generate nested boxplot?"),
    ),
    br(),
    shinyWidgets::pickerInput(inputId = "varZ", label= h5("Group by"), choices = char.var, selected="type_1"),
    shinyWidgets::prettyCheckbox(inputId = "isLegend", label = "Remove legend?"),
    br()
  ),


  dashboardBody(
    
    tags$head(
      includeCSS('style.css')
    ),
    
    tags$style(HTML("
      #code_box {
          border: 3px solid grey;
      }
      #unit {
          border: 4px double green;
      }
    ")),
    
    theme = bs_theme(version = 4, bootswatch = "minty"),
    setShadow(class = "box"),
    setShadow(class = "infoBox"),
    
    ##################################
    ## InfoBox
    #################################
    h2("Compare Five Interactive Visualization Tools in R:"),
    br(),
    fluidRow(
    
      infoBox("Long lasting RShiny apps", value = NULL, subtitle = " should be interactive and self-explicit. R packages that support html widgets might help achive this goal pretty easily.", icon = shiny::icon("bar-chart", verify_fa = FALSE), 
              color = "navy", width = 6, href = NULL, fill = TRUE),
      infoBox("Visualizations grant users ", value = NULL, subtitle = " the ability to explore, manipulate, and interact with data by employing dynamic charts, changing colors, and shapes. ",
              icon = shiny::icon("user-circle", verify_fa = FALSE), color = "green", width = 6, href = NULL, fill = TRUE)
      
    ),
    
    fluidRow(
      infoBox("HTML widgets ", subtitle = "can be used at the R console as well as embedded in R Markdown reports and Shiny web applications.",
              icon = shiny::icon("file-image", verify_fa = FALSE), color = "green", width = 6, href = NULL, fill = TRUE),
      infoBox("HTML widgets ", value = NULL, subtitle = " work just like R plots except they produce interactive web visualizations. ",
              icon = shiny::icon("area-chart", verify_fa = FALSE), color = "navy", width = 6, href = NULL, fill = TRUE)
    ),
    
    
    ###################################
    ## PLOTLY
    ###################################
    
    fluidPage(width = 12, height = 400, id = "unit",
              br(),
          box(title = "Plotly R package", plotlyOutput("plotly"),
                status = status, solidHeader = TRUE,
                collapsible = TRUE, class="box", closable = TRUE, width = 9, 
                br(), height = 400
                
            ),
          
          box(closable = TRUE, width = 3, solidHeader = TRUE, height = 400,
                title = "Info box", status = status,
                collapsible = TRUE,
                class="box",
                p("Plotly is a technical computing company headquartered in Montreal, Quebec. 
                  It develops online data analytics and visualization tools. 
                  Plotly provides online graphing, analytics, and statistics tools for individuals and collaboration, 
                  as well as scientific graphing libraries for Python, R, MATLAB, Perl, Julia, Arduino, and REST."),
                h4("Pros"),
                p("-Free"), p("-Easy to use"), p("-Professional look"),
                h4("Cons"),
                p("-Tooltips have to be handled manually with JavaScript"),
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
          #plotlyOutput("ggplotly"), 
          plotOutput(("ggplotly")),
          closable = TRUE,  width = 9, 
          title = "Ggplot2 R package", status = status, 
          solidHeader = TRUE, 
          collapsible = TRUE, 
          class="box",
          br(),
          verbatimTextOutput("ggplotly_code", placeholder = TRUE)
          ),
      
      box(closable = TRUE, width = 3, solidHeader = TRUE, height = 400,
          title = "Info box", status = status,
          collapsible = TRUE,
          class="box",
          p("Extension of the ggplo2 package."),
          h4("Pros"),
          p("-Free"), p("-worked with ggplot objects"),
          p("-Tooltips for x, y, and colour are automatically added"),
          p("- Many different themes are available:"),
          shinyWidgets::pickerInput(inputId = "ggplot_theme", 
                                    label = h4("Select ggplot theme"), 
                                    choices = c("theme_grey", "theme_bw", "theme_classic", "theme_dark", "theme_minimal", "theme_void", "theme_version"), 
                                    selected="theme_grey()"),
          h4("Cons"),
          p("This package doesn't always generate a nice professional look."),
          h4("Library"),
          p("library(ggplot2)")
      )
    ),
    
    ###################################
    ## HIGHCHART
    ###################################
    fluidPage( id = "unit", 
      br(),
      box(highchartOutput("highchart"),  title = "Highchart R package", 
          closable = TRUE, width = 9, collapsible = TRUE, class="box",
                status = status, solidHeader = TRUE, 
          br(), 
          verbatimTextOutput("highcharter_code", placeholder = TRUE), 
          class="box"),
      
      box(closable = TRUE, width = 3, solidHeader = TRUE, height = 400,
          title = "Info box", status = status,
          collapsible = TRUE,
          class="box",
          p("Highcharts (http://www.highcharts.com/) is a charting library written in pure JavaScript, 
          offering an easy way of adding interactive charts to your web site or web application with a simple configuration syntax. 
          Highcharts currently supports line, spline, area, areaspline, column, bar, pie, scatter, angular gauges, 
          arearange, areasplinerange, columnrange, bubble, box plot, error bars, funnel, waterfall and polar chart types. 
          Highcharter R package is a wrapper for the ‘Highcharts’ library including shortcut functions to plot R objects. 
          It was created by Highsoft in Norway (2009) and has been regularly featured in the national media."),
          h4("Pros"),
          p("-Very professionally looking plots"),
          p("-The legends are filling available space in the most efficient way"),
          p("The legends are interactive"),
          p("Most ergnomically looking plots"),
          h4("Cons"),
          p("-The license is required for commercial usage. But the license is affordable. Usage for the personal usage is free."),
          h4("Library"),
          p("library(highcharter)")
      ),
      box(
        verbatimTextOutput("highcharter_code"), width = 12, closable = TRUE, id = "code_box", title = "Code"),
      br()
    ),
    
    
    
    
    fluidPage(
            box(rbokehOutput("rbokeh"), width = 12, title = "Bokeh R package", status = status, solidHeader = TRUE, collapsible = TRUE, class="box"),
            br(),
            box(echarts4rOutput("echart"),width = 12, title = "Echart R package", status = status, solidHeader = TRUE, collapsible = TRUE, class="box")
            
      ),
    
    fluidPage(
      br(),
      br(),
      column(12, solidHeader = TRUE, status = "success", dataTableOutput('table'), extensions = c('Responsive')),
      br()
    ),
     
    br()
  )
)

server <- function(input, output, session) {
  
  observe({
    updatePickerInput(session, inputId = "type", choices = c("scatter", "boxplot"), selected = "scatter")
    # c("scatter", "density", "barplot", "boxplot", "piechart")
  })
  
  observe({ updatePickerInput(session, inputId = "varX") })
  observe({ updatePickerInput(session, inputId = "varY")})
  observe({ updatePickerInput(session, inputId = "varZ") })
  
  
  output$table <- renderDataTable({
    pokemon 
  })
  
  
  ###################################
  ## GGPLOTLY
  ###################################
  output$ggplotly <- renderPlot({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    
    if(input$type == "scatter") {
      plot = pokemon %>% ggplot(., aes(xx, yy, color=zz)) + geom_point(size=input$dotsize) + labs(x = input$varX, y = input$varY, title="ggplotly: scatter plot example")
    }
    else if(input$type == "density") {
      plot = pokemon %>% ggplot(., aes(xx, colour = zz)) + geom_density(alpha=opacity) + labs(x = input$varX, title="ggplotly: boxplot example")
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% ggplot(., aes(y = xx, x = zz, color = zz)) + geom_boxplot(alpha=opacity) + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(x = input$varX, title = " + geom_boxplot()" ) 
      # + coord_flip()
      
    }
    else if(input$type == "barplot") {
      
    }
    else if(input$type == "piechart") {
      plot = pokemon %>% dplyr::count(zz) %>% ggplot(., aes(x="", y=n, fill=zz)) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + geom_text(aes(label=input$varZ), position = position_stack(vjust=0.5), color = "white") + theme_void() %>% labs(title = "Pokemon types <i> (ggplot) </i>")
    }
    
    # plot = plot + theme(legend.title= element_blank())
    
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
   
    if(input$isLegend) {
      plot = plot + theme(legend.position = "none")  
    }
      
   #   "theme_grey", "theme_bw", "theme_classic", "theme_dark", "theme_minimal", "theme_void", "theme_version"
    #ggplotly(plot)
    plot
    
  })
  
  
  
  
  output$ggplotly_code <- renderText({
    if(input$type == "scatter") {
      string = 'ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point(size=3) + labs(x = "length", y = "width", title="iris database"))'
    }
    else if(input$type == "density") {
      string = 'ggplot(iris, aes(Sepal.Length, colour = Species, fill=Species)) + geom_density(alpha=0.8) + labs(x = "Length"))'
    }
    else if(input$type == "boxplot") {
      string = 'ggplot(iris, aes(y = Sepal.Length, colour = Species, fill=Species)) + geom_boxplot(alpha=0.8) + labs(x = "Length")  + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(x = "Sepal length", title = " iris database" ) '
    }
    else if(input$type == "barplot") {
      
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
  ## HIGHCHART
  ###################################
  output$highchart <- renderHighchart2({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    if(input$type == "scatter") {
      hcplot = highchart() %>% hc_add_series(pokemon, "scatter", hcaes(x = xx, y = yy,  group = zz)) %>% hc_title(text = "hc_add_series(type = 'scatter')", style = list(fontWeight = "bold")) %>% hc_yAxis(title = list(text = input$varUY), allowDecimals = FALSE) %>% hc_tooltip(formatter = JS("function(){ return ('pokemon: ' + this.point.pokemon + ' <br> id: ' + this.point.id + '<br> base: ' + this.point.base_experience)}")) %>% hc_plotOptions(bubble = list(maxSize = "100%")) 
    }
    else if(input$type == "density") {
      hcplot = pokemon %>% hchart(density(xx, na.rm = TRUE), type = "area", name = xx ) %>% hc_title(text = " type ='area' ", stype=list(fontWeight = "bold"))
    }
    else if(input$type == "barplot") {
      hcplot = pokemon %>% dplyr::count(type_1, egg_group_1, type_2_colour) %>% hchart(type = 'bar', hcaes(x = type_1, y = n, group=egg_group_1)) %>% hc_title(text = "Barplot: ", margin = 20, align = "left", style = list(color = "#22A884", useHTML = TRUE))  %>% hc_xAxis(title=list(text="Pokemon type")) 
    }
    else if(input$type == "boxplot") {
      plot = data_to_boxplot(pokemon,  variable = xx, group_var = zz, add_outliers = TRUE)
      hcplot = highchart() %>% hc_xAxis(type = "category") %>% hc_add_series_list(plot) %>% hc_add_series(data = pokemon, type = "scatter", hcaes(x = zz, y = xx, group=zz)) %>%  hc_plotOptions(scatter=list(jitter=list(x=0.08, y=0))) %>% hc_title(text = "data_to_boxplot()") %>% hc_plotOptions(scatter=list(marker=list(radius=2, symbol = "circle", lineWidth=1)))  
    }
    else if(input$type == "piechart") {
      hcplot = pokemon %>% dplyr::count(zz) %>% hchart(type ="pie", hcaes(zz, y = n)) %>% hc_title(text = "type = 'pie'")
    }
    if(input$isLegend) {
      hcplot =  hcplot %>% hc_legend(enabled = F)
    }
    hcplot %>% hc_credits(enabled = TRUE, text = "Sources: highchater", style = list(fontSize = "10px")) %>% hc_xAxis(title = list(text = input$varX), allowDecimals = FALSE)
  })
  
  ###################################
  ## Plotly
  ###################################
  output$plotly <- renderPlotly({
    
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    
    if(input$type == "scatter") {
      plot = pokemon %>% plot_ly %>% add_trace(x=~xx, y=~yy, color=~zz, type="scatter", mode="markers", opacity=opacity, marker = list(size = input$dotsize)) %>% layout(title = "Plotly: scatter plot example", xaxis = list(title = input$varX), yaxis = list(title = input$varY ))
    }
    else if(input$type == "density") {
      #data = pokemon %>% dplyr::select(xx, yy, zz, pokemon) %>% reshape2::melt(., value.name = "score", id.var = "pokemon")
      plot = plot_ly(pokemon, x = ~xx,  type = 'violin', side = "positive") %>% layout(barmode="overlay") %>% layout(title = "type = 'violin'", xaxis = list(title = input$varX))
    }
    else if(input$type == "barplot") {
      plot =  pokemon  %>% dplyr::count(type_1, egg_group_1) %>% plot_ly() %>% add_trace(type="bar", x =~n, y=~type_1, color=~egg_group_1, text = ~paste("Type ", type_1, "\n", "Number ", n, "\n", "Egg group ", egg_group_1), hoverinfo=c("text")) %>% layout(barmode = "stack", margin=list(l=240)) %>% layout(title = " ") %>% layout(title="Plotly: barplot example'") 
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% plot_ly(type = "box", x =~xx, color=~zz, boxpoints="all", jitter=0.5, pointpos = 0) %>% layout(title="Plotly: boxplot example") 
    }
    
    if(input$isLegend) {
      plot = plot %>% layout(showlegend = FALSE)
    }
    plot
  })
  
  
  output$plotly_code <- renderText({
    if(input$type == "scatter") {
      string = paste0('pokemon %>% plot_ly %>% add_trace(x=~', input$varX, ', y=~', input$varY, ', color=~', input$varZ, ' , type="scatter", mode="markers", opacity=opacity, marker = list(size = ', input$dotsize, ')) %>% layout(title = "Plotly: scatter plot example", xaxis = list(title = ', input$varX, '))')
    }
    else if(input$type == "boxplot") {
      string = paste0('pokemon %>% plot_ly(type = "box", x =~', input$varX, ', color=~', input$varZ, ', boxpoints="all", jitter=0.5, pointpos = 0) %>% layout(title="Plotly: boxplot example") ')
    }else if(input$type == "density") {
      #data = pokemon %>% dplyr::select(xx, yy, zz, pokemon) %>% reshape2::melt(., value.name = "score", id.var = "pokemon")
      string = plot_ly(pokemon, x = ~xx,  type = 'violin', side = "positive") %>% layout(barmode="overlay") %>% layout(title = "type = 'violin'", xaxis = list(title = input$varX))
    }
    else if(input$type == "barplot") {
      pokemon  %>% dplyr::count(type_1, egg_group_1) %>% plot_ly() %>% add_trace(type="bar", x =~n, y=~type_1, color=~egg_group_1, text = ~paste("Type ", type_1, "\n", "Number ", n, "\n", "Egg group ", egg_group_1), hoverinfo=c("text")) %>% layout(barmode = "stack", margin=list(l=240)) %>% layout(title = "Plotly:: barplot example")  %>% layout(title="type = 'bar'") 
    }
    
    if(input$isLegend) {
      string = paste0(string, layout(showlegend = FALSE))
    }
    string
  })
  
  ###################################################
  ## rbokeh
  ###################################################
  
  output$rbokeh <- renderRbokeh({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    
    if(input$type == "scatter") {
        figure(title ="Bokeh: ly_points", width = 1200, height = 1000) %>% ly_points(x = xx, y = yy, color = zz, data = pokemon, hover = char.var, size = input$dotsize)
    }
    else if(input$type == "density") {
        figure(title ="Bokeh: ly_density", width = 1200, height = 1000) %>% ly_density(xx, zz, data = pokemon)
    }
    else if(input$type == "boxplot") {
        figure(title ="Bokeh: ly_boxplot", width = 1200, height = 1000) %>% ly_boxplot(xx, zz, data = pokemon)
    }
  })
  
  ####################################################
  ## Echart
  ###################################################
  output$echart <- renderEcharts4r({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    
    if(input$type == "scatter") {
      plot = pokemon %>% group_by(zz) %>% e_charts(xx) %>% e_scatter(yy, symbol_size = input$dotsize) %>% e_axis_labels(x = input$varX, y = input$varY) %>% e_title(
            text = "e_charts() %>% e_scatter()") %>% e_brush(throttleDelay = 1000) %>% e_legend(orient = 'vertical', right = '5', top = '15%') %>% e_tooltip()
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% group_by(zz) %>% e_charts() %>% e_boxplot(xx, outliers = FALSE) %>% e_brush(throttleDelay = 1000) 
    }
    else if(input$type == "density") {
      plot = plot
    }
    #plot = plot %>% e_tooltip() %>% e_grid(right = '15%') 
    plot
  })
  
  output$echart_code <- renderText({
    
    if(input$type == "scatter") {
      string = paste0("pokemon %>% group_by(", input$varZ, ") %>% e_charts(", input$varX, ") %>% e_scatter(", input$varY, "symbol_size = ", input$dotsize, ") %>% e_axis_labels(x = ", input$varX, "y = ", input$varY, ", ) %>% e_title(text = 'Echart: scatter plot') %>% e_brush(throttleDelay = 1000) 
        %>% e_legend(orient = 'vertical', right = '5', top = '15%')")
    }
    else if(input$type == "boxplot") {
      string = paste0("pokemon %>% group_by(", input$varZ, ") %>% e_charts() %>% e_boxplot(", input$varX, ", outliers = FALSE) %>% e_brush(throttleDelay = 1000)")
    }
    else if(input$type == "density") {
      
    }
    string = paste0(string, "%>% e_grid(right = '15%') %>% e_tooltip()")
    string
  })
  
  
  
}


shinyApp(ui = ui, server = server)

