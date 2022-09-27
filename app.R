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

strSRC <- normalizePath(getwd())
# sysConfig <- getConfig()

pokemon = pokemon %>% na.omit() %>% dplyr::filter(!type_1 %in% c("steel", "psychic", "ghost", "fighting", "fairy", "normal", "bug", "ice", "electric"))


num.var = select_if(pokemon, is.numeric) %>% colnames()
char.var = select_if(pokemon, is.character) %>% colnames()
remove = c("pokemon", "image_url" , "icon_url" , "detail_url"    )
char.var = char.var [! char.var %in% remove]
status = "success"
opacity=1.2

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
    title = h3("  Attribute toolbox"),
    
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
    # c("density", "barplot", "piechart")
    shinyWidgets::pickerInput(inputId = "varX", label = h5("Select x value"), choices = num.var, selected="height"),
    
    conditionalPanel( condition = "input.type == 'scatter'",
                      shinyWidgets::pickerInput(inputId = "varY", label = h5("Select y value"), choices = num.var, selected="weight"),
                      sliderInput(inputId = "dotsize", label = h5("Choose the size of the points"), value = 7, width = NULL, min = 1, max = 20, step = 1)
    ),
#    conditionalPanel( condition = "input.type == 'boxplot'",
#                      shinyWidgets::prettyCheckbox(inputId = "nestedBoxplot", label = "Generate nested boxplot?")),
    
#    conditionalPanel( condition = "input.nestedBoxplot == 1",
#                      shinyWidgets::pickerInput(inputId = "varZ1", label = h5("Select value for nesting"), choices = char.var, selected="type_1"),           
#    ),
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
              box(title = "Plotly R package", 
                  plotlyOutput("plotly"),
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
                  p("(1) Free, (2) Easy to use; (3) Professional look"),
                  h4("Cons"),
                  p("(1) Tooltips have to be handled manually with JavaScript"),
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
                plotlyOutput(("myggplotly")),
                closable = TRUE,  width = 9, 
                title = "Ggplot2 R package", status = status, 
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
                    (3) Tooltips for x, y, and colour are automatically added; (4) Many different themes are available; (5) Interactive legends."),
                  shinyWidgets::pickerInput(inputId = "ggplot_theme", 
                                            label = h4("Select ggplot theme"), 
                                            choices = c("theme_grey", "theme_bw", "theme_classic", "theme_dark", "theme_minimal", "theme_void", "theme_version"), 
                                            selected="theme_bw"),
                  h4("Cons"),
                  p("This package doesn't always generate a nice professional look."),
                  h4("Library"),
                  p("library(ggplot2)")
              ),
              box(
                verbatimTextOutput("ggplotly_code", placeholder = TRUE), width = 12, closable = TRUE, id = "code_box", title = "Code"
              )
    ),
    br(),
    
    ####################################
    ## RBOKEH
    ####################################
    fluidPage(width = 12, height = 400,  id = "unit",
              br(),
              
              box(rbokehOutput("rbokeh"), 
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
                  p("(1) Strange not resizable dimentions of the plot; (2) Legend is covering the plot and not movable (solution was posted only for python lib; (3) legends are not interactive. "),
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
              box(echarts4rOutput("echart"), 
                  width = 9, title = "Echart R package", status = status, 
                  solidHeader = TRUE, collapsible = TRUE, class="box"),
              
              box(closable = TRUE, width = 3, solidHeader = TRUE, height = 400,
                  title = "Info box", status = status,
                  collapsible = TRUE,
                  class="box",
                  p("Easily create interactive charts by leveraging the 'Echarts Javascript' library which includes 36 chart types, themes, 'Shiny' proxies and animations."),
                  h4("Pros"),
                  p("(1) Professional look; (2) Good combinations of default colours; (3) Easy to add tooltips; (4) Interactive legends."),
                  h4("Cons"),
                  p("(1) Relatively hard to build; "),
                  h4("Library"),
                  p("library(echarts4r)")
              ),
              box(
                verbatimTextOutput("echart_code", placeholder = TRUE), width = 12, closable = TRUE, id = "code_box", title = "Code"
              )
              
    ),
    
    
    fluidPage(
      br(),
      br(),
      column(12, solidHeader = TRUE, status = "success", 
             dataTableOutput('table'), extensions = c('Responsive')),
      br()
    )
    
    
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
    
    pokemon$zz = as.factor(pokemon$zz)
    
    if(input$type == "scatter") {
      plot = pokemon %>% ggplot(., aes(xx, yy, color=zz)) + geom_point(size=input$dotsize) + labs(x = input$varX, y = input$varY, title="ggplotly: scatter plot example")
      # plot = plot + geom_smooth()
    }
    else if(input$type == "density") {
      plot = pokemon %>% ggplot(., aes(xx, colour = zz)) + geom_density(alpha=opacity) + labs(x = input$varX, title="ggplotly: boxplot example")
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% ggplot(., aes(y = xx, x = zz, colour = zz)) + ggplot2::geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(y = input$varY, x = "", title = "ggplotly: boxplot" )
      # + coord_flip()
    }
    else if(input$type == "barplot") {
      
    }
    else if(input$type == "piechart") {
      plot = pokemon %>% dplyr::count(zz) %>% ggplot(., aes(x="", y=n, fill=zz)) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + geom_text(aes(label=input$varZ), position = position_stack(vjust=0.5), color = "white") + theme_void() %>% labs(title = "Pokemon types <i> (ggplot) </i>")
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
    
    #    plot = ggplotly(plot)
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
    
    else if(input$type == "barplot") {
      string = paste0('pokemon %>% ggplot(., aes(y = ', input$varX, ', x = ', input$varZ, 'color = ', input$varZ, ')) + geom_boxplot(alpha=opacity) + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(y = ', input$varY, ', title = "ggplotly: boxplot" ) '   )
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
    
    if(input$type == "scatter") {
      plot = pokemon %>% plot_ly %>% add_trace(x=~xx, y=~yy, color=~zz, type="scatter", mode="markers", opacity=opacity, marker = list(size = input$dotsize), text = ~paste(" ID", id, "\n", "pokemon", pokemon, "\n", "type: ", type_1,  "\n", "Egg group: ", egg_group_1), hoverinfo=c("text")) %>% layout(title = "Plotly: scatter plot example", xaxis = list(title = input$varX), yaxis = list(title = input$varY ))
    }
    else if(input$type == "density") {
      #data = pokemon %>% dplyr::select(xx, yy, zz, pokemon) %>% reshape2::melt(., value.name = "score", id.var = "pokemon")
      plot = plot_ly(pokemon, x = ~xx,  type = 'violin', side = "positive") %>% layout(barmode="overlay") %>% layout(title = "type = 'violin'", xaxis = list(title = input$varX))
    }
    else if(input$type == "barplot") {
      plot =  pokemon  %>% dplyr::count(type_1, egg_group_1) %>% plot_ly() %>% add_trace(type="bar", x =~n, y=~type_1, color=~egg_group_1, text = ~paste("Type ", type_1, "\n", "Number ", n, "\n", "Egg group ", egg_group_1), hoverinfo=c("text")) %>% layout(barmode = "stack", margin=list(l=240)) %>% layout(title = " ") %>% layout(title="Plotly: barplot example'") 
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% plot_ly(type = "box", x =~xx, color=~zz, boxpoints="all", jitter=0.5, pointpos = 0, text = ~paste(" ID", id, "\n", "pokemon", pokemon, "\n", "type: ", type_1,  "\n", "Egg group: ", egg_group_1), hoverinfo=c("text")) %>% layout(title="Plotly: boxplot example") 
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
      string = paste0('pokemon %>% plot_ly(type = "box", x =~', input$varX, ', color=~', input$varZ, ', boxpoints="all", jitter=0.5, pointpos = 0) %>% layout(title="Plotly: boxplot example") ')
    }else if(input$type == "density") {
      #data = pokemon %>% dplyr::select(xx, yy, zz, pokemon) %>% reshape2::melt(., value.name = "score", id.var = "pokemon")
      string = plot_ly(pokemon, x = ~xx,  type = 'violin', side = "positive") %>% layout(barmode="overlay") %>% layout(title = "type = 'violin'", xaxis = list(title = input$varX))
    }
    else if(input$type == "barplot") {
      pokemon  %>% dplyr::count(type_1, egg_group_1) %>% plot_ly() %>% add_trace(type="bar", x =~n, y=~type_1, color=~egg_group_1, text = ~paste("Type ", type_1, "\n", "Number ", n, "\n", "Egg group ", egg_group_1), hoverinfo=c("text")) %>% layout(barmode = "stack", margin=list(l=240)) %>% layout(title = "Plotly:: barplot example")  %>% layout(title="type = 'bar'") 
    }
    
    if(input$isLegend) {
      string = paste0(string, "%>% layout(showlegend = FALSE)")
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
      if(input$isLegend) {
        plot = figure(title ="Bokeh: ly_points", width = 600, height = 400, legend_location = NULL) %>% ly_points(x = xx, y = yy, color = zz, data = pokemon, hover = char.var, size = input$dotsize)%>% x_axis(label = input$varX) %>% y_axis(label = input$varY)
      }
      else {
        plot = figure(title ="Bokeh: ly_points", width = 600, height = 400) %>% ly_points(x = xx, y = yy, color = zz, data = pokemon, hover = char.var, size = input$dotsize) %>% x_axis(label = input$varX) %>% y_axis(label = input$varY)
      }
    }
    
    else if(input$type == "density") {
      plot = figure(title ="Bokeh: ly_density", width = 1200, height = 1000) %>% ly_density(xx, zz, data = pokemon)
    }
    
    
    else if(input$type == "boxplot") {
      if(input$isLegend) {
        plot = figure(title ="Bokeh: ly_boxplot", width = 600, height = 400, legend_location = NULL) %>% ly_boxplot(xx, zz, color = zz, data = pokemon) %>% y_axis(label = input$varY)
      }
      else {
        plot = figure(title ="Bokeh: ly_boxplot", width = 1200, height = 1000) %>% ly_boxplot(xx, zz, color = zz, data = pokemon) %>% y_axis(label = input$varY)
      }
    }
    
    plot 
    
  })
  
  output$brokeh_code <- renderText({
    if(input$type == "scatter") {
      string = paste0('figure(title ="Bokeh: ly_points", width = 600, height = 400) %>% ly_points(x = ', input$varX, ', y = ', input$varY, ', color = ', input$varZ, ', data = pokemon, hover = list.of.var.to.show, size =',  input$dotsize, ', ) %>% x_axis(label = input$varX) %>% y_axis(label = input$varY)')
    }
    else if(input$type == "boxplot") {
      string = paste0('')
    }
    
    string
    
  })
  
  output$echart <- renderEcharts4r({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    
    if(input$type == "scatter") {
      plot = pokemon %>% group_by(zz) %>% e_charts(xx) %>% e_scatter(yy, symbol_size = input$dotsize) %>% e_axis_labels(x = input$varX, y = input$varY) %>% e_title(
        text = "Echart for R: scatter plot") %>% e_brush(throttleDelay = 1000) %>% e_legend(orient = 'vertical', right = '5', top = '15%') %>% e_tooltip()
    }
    else if(input$type == "boxplot") {
      plot = pokemon %>% group_by(zz) %>% e_charts() %>% e_boxplot(xx, outliers = TRUE) %>% e_scatter(xx, symbol_size = input$dotsize) %>% e_brush(throttleDelay = 1000) %>% e_title(text = 'Echart: boxplot plot') %>% e_scatter(weightsymbol_size = 15)
    }
    else if(input$type == "density") {
      plot = plot
    }
    # plot = plot %>% e_tooltip() %>% e_grid(right = '15%') 
    plot
  })
  
  output$echart_code <- renderText({
    
    if(input$type == "scatter") {
      string = paste0("pokemon %>% group_by(", input$varZ, ") %>% e_charts(", input$varX, ") %>% e_scatter(", input$varY, "symbol_size = ", input$dotsize, ") %>% e_axis_labels(x = ", input$varX, "y = ", input$varY, ", ) %>% e_title(text = 'Echart: scatter plot') %>% e_brush(throttleDelay = 1000) 
        %>% e_legend(orient = 'vertical', right = '5', top = '15%')")
    }
    
    else if(input$type == "boxplot") {
      string = paste0("pokemon %>% group_by(", input$varZ, ") %>% e_charts() %>% e_boxplot(", input$varX, ", outliers = FALSE) %>% e_brush(throttleDelay = 1000)  %>% e_title(text = 'Echart: boxplot')")
    }
    
    else if(input$type == "density") {
      
    }
    string
  })
  
  
  
  
}



shinyApp(ui = ui, server = server)