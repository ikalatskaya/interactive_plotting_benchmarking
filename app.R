
source("global.R")
source("functions.R")

colour = "#00AA51"

getConfig <- function(){
  config <- yaml::read_yaml(file.path(strSRC,"sys.yml"))
  return(config)
}
strSRC <- normalizePath(getwd())
sysConfig <- getConfig()


# this function is required for bookmarks generation. 
# Based on Hadley Wickham book Mastering Shiny 
ui <- function(request) {
  dashboardPage(
  
  dashboardHeader(
    disable = TRUE, # remove completely
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
                                choices = c("pokemon dataset" = "pokemon" ), selected="pokemon") 
      %>% tippy::tippy(allowHTML = TRUE, "<span style='font-size:16px;'>
      This dataset contains information on all 802 Pokemon from all Seven Generations of Pokemon. 
      The information was scraped from http://serebii.net/. 
      The dataset is a part of highcharter R package.</span>"),
      
      br(),
      
      shinyWidgets::pickerInput(inputId = "type", 
                                label = h4("Select plot type"), 
                                choices = plots,
                                selected="scatter"),
      
      
      conditionalPanel( condition = "input.type == 'scatter' | input.type == 'density'",
                  shinyWidgets::pickerInput(inputId = "varX", label = h5("Select x value"), choices = num.var, selected="height"),
      ),
      conditionalPanel( condition = "input.type == 'scatter' | input.type == 'boxplot'",
                  shinyWidgets::pickerInput(inputId = "varY", label = h5("Select y value"), choices = num.var, selected="weight"),
                  sliderInput(inputId = "dotsize", label = h5("Choose the size of the points"), value = 6, width = NULL, min = 1, max = 20, step = 1)
      ),
      conditionalPanel( condition = "input.type != 'density'",
                  shinyWidgets::pickerInput(inputId = "varZ", label= h5("Group and colour by"), choices = char.var, selected="type_1"),
      ),
      conditionalPanel( condition = "input.type == 'barchart' | input.type == 'heatmap'",
                  shinyWidgets::pickerInput(inputId = "varA", label= h5("Add another cat value "), choices = char.var, selected="type_2")
                  # shinyWidgets::prettyCheckbox(inputId = "isPercentStackedBar", label = "To Percentage stacked bar chart?", value = FALSE)
      ),
      br(),                  
      shinyWidgets::prettyCheckbox(inputId = "isLegend", label = "Remove legend?"),
      br()
      # bookmarkButton()
      #     conditionalPanel( condition = "input.type == 'boxplot'",
      #                      shinyWidgets::prettyCheckbox(inputId = "nestedBoxplot", label = "Generate nested boxplot?")),
      
      #     conditionalPanel( condition = "input.nestedBoxplot == 1",
      #                      shinyWidgets::pickerInput(inputId = "varZ1", label = h5("Select value for nesting"), choices = char.var, selected="type_1"),           
      #     ),
      
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
      
      shinydashboard::infoBox("Long lasting RShiny apps", value = NULL, subtitle = " should be interactive and self-explicit. R packages that support html widgets might help achive this goal pretty easily.", icon = shiny::icon("diagram-project", lib = "font-awesome"), 
              color = "navy", width = 6, href = NULL, fill = TRUE),
      shinydashboard::infoBox("Visualizations grant users ", value = NULL, subtitle = " the ability to explore, manipulate, and interact with data by employing dynamic charts, changing colors, and shapes. ",
              icon = shiny::icon("circle-user", verify_fa = FALSE), color = "green", width = 6, href = NULL, fill = TRUE)
      
    ),
    
    fluidRow(
      shinydashboard::infoBox("HTML widgets ", subtitle = "can be used at the R console as well as embedded in R Markdown reports and Shiny web applications.",
              icon = shiny::icon("file-image", verify_fa = FALSE), color = "green", width = 6, href = NULL, fill = TRUE),
      shinydashboard::infoBox("HTML widgets ", value = NULL, subtitle = " work just like R plots except they produce interactive web visualizations. ",
              icon = shiny::icon("chart-area", verify_fa = FALSE), color = "navy", width = 6, href = NULL, fill = TRUE)
    ),
    
    ############################################################
    ## PLOTLY
    #############################################################
    
    fluidPage(width = 12, height = 400, id = "unit",
              br(),
              
              plotBox(title = sysConfig$package$PLOTLY$name, 
                      output = fullscreen_this(plotlyOutput("plotly")), 
                      icon = icon(sysConfig$package$PLOTLY$icon)
                      ),
              
              descBox(
                      desc = sysConfig$package$PLOTLY$description,
                      pros = sysConfig$package$PLOTLY$pros,
                      con = sysConfig$package$PLOTLY$cons,
                      lib = sysConfig$package$PLOTLY$lib,
                      icon = icon(sysConfig$title$infoicon)
                      ),
              
              codeBox(
                      title = sysConfig$title$codebox, 
                      output = verbatimTextOutput("plotly_code", placeholder = TRUE)
                      ),
              br()
    ),
    
    br(),
    
    ################################################################
    ## GGPLOTLY
    ################################################################
    fluidPage(width = 12, height = 400,  id = "unit",
              br(),
             
              plotBox(title = sysConfig$package$GGPLOTLY$name, 
                      output = fullscreen_this(plotlyOutput("myggplotly")), 
                      icon = icon(sysConfig$package$GGPLOTLY$icon)),
              
              
              descBox(
                      desc = sysConfig$package$GGPLOTLY$description,
                      pros = sysConfig$package$GGPLOTLY$pros,
                      fixik = shinyWidgets::pickerInput(inputId = "ggplot_theme", 
                              label = h4("Select ggplot_theme"), 
                              choices = sysConfig$package$GGPLOTLY$theme , 
                              selected="theme_bw"), # ggplot_themes
                      con = sysConfig$package$GGPLOTLY$cons,
                      lib = sysConfig$package$GGPLOTLY$lib,
                      icon = icon(sysConfig$title$infoicon)
              ),
              
              codeBox(title = sysConfig$title$codebox, 
                      output = verbatimTextOutput("ggplotly_code", placeholder = TRUE)),
              br()
    ),
    br(),
    
    ###################################
    ## HIGHCHART
    ###################################
    fluidPage( id = "unit", height = 400,
               
               br(),
               
               plotBox(title = sysConfig$package$HIGHCHARTER$name, 
                       output = fullscreen_this(highchartOutput("highchart")), 
                       icon = icon(sysConfig$package$HIGHCHARTER$icon)),
               
              box(style='width:3; height:400px;overflow-y: scroll;',

                  closable = TRUE, 
                  collapsible = TRUE,
                  solidHeader = TRUE,  
                  width = 3,
                  title = "Info box", 
                  status = status,
                  class="box",
                  p(sysConfig$package$HIGHCHARTER$description),
                  h4("Pros"),
                  p(sysConfig$package$HIGHCHARTER$pros),
      
                  shinyWidgets::pickerInput(inputId = "highchart_theme", 
                                             label = h4("Select highchart theme"), 
                                             choices = sysConfig$package$HIGHCHARTER$theme, 
                                             selected="hc_theme_google()"),
                  
                  conditionalPanel( condition = "input.type == 'scatter'",
                        shinyWidgets::prettySwitch("toBubble", "Switch to bubble?", value = FALSE, slim = TRUE, fill = TRUE),
                  ),
                  h4("Cons"),
                  p(sysConfig$package$HIGHCHARTER$cons),
                  h4("Library"),
                  p(sysConfig$package$HIGHCHARTER$lib),
                  
                  icon = icon(sysConfig$title$infoicon)
               ),
              
                codeBox(title = sysConfig$title$codebox, 
                      output =  verbatimTextOutput("highcharter_code", placeholder = TRUE)),
               br()
    ),
    br(),
    


    ####################################################
    ## RBOKEH
    #####################################################
    fluidPage(width = 12, height = 400,  id = "unit",
  
              br(),
              plotBox(title = sysConfig$package$BOKEH$name, 
                      output = fullscreen_this(rbokehOutput("rbokeh")), 
                      icon = icon(sysConfig$package$BOKEH$icon)),
              
              descBox(desc = sysConfig$package$BOKEH$description,
                                  pros = sysConfig$package$BOKEH$pros,
                                  cons = sysConfig$package$BOKEH$cons,
                                  lib = sysConfig$package$BOKEH$lib,
                                  icon = icon(sysConfig$title$infoicon)
              ),
              
              codeBox(title = sysConfig$title$codebox, 
                      output = verbatimTextOutput("brokeh_code", placeholder = TRUE)),
              br()
    ),
    
    br(),
    
    ####################################################
    ## ECHART
    ####################################################
    
    fluidPage(width = 12, height = 400,  id = "unit",
            
              br(),
              plotBox(title = sysConfig$package$ECHART$name, 
                      output = fullscreen_this(echarts4rOutput("echart")), 
                      icon = icon(sysConfig$package$ECHART$icon)),
              
              
              box(width = 3, 
                  title = "Info box", 
                  status = status,
                  closable = TRUE, 
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  class="box",
                  p(sysConfig$package$ECHART$description),
                  h4("Pros"),
                  p(sysConfig$package$ECHART$pros),
                 
                  shinyWidgets::pickerInput(inputId = "ehcart_theme", 
                                            label = h4("Select echart theme"), 
                                            choices = echart_themes, 
                                            selected="forest"),
                  
                  h4("Cons"),
                  p(sysConfig$package$ECHART$cons),
                  h4("Library"),
                  p(sysConfig$package$ECHART$lib),
                  icon = icon(sysConfig$title$infoicon)
              ),
              
              codeBox(title = sysConfig$title$codebox, 
                      output = verbatimTextOutput("echart_code", placeholder = TRUE)),
              
              br()
              
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


}











server <- function(input, output, session) {
  
  observe({
    updatePickerInput(session, inputId = "type", choices = plots, selected = "scatter") # "density",  "heatmap"
  })
  
  observe({ updatePickerInput(session, inputId = "varX") })
  observe({ updatePickerInput(session, inputId = "varY")})
  observe({ updatePickerInput(session, inputId = "varZ") })
  observe({ updatePickerInput(session, inputId = "varA") })
  observe ({ updatePrettyCheckbox(session, inputId =  "isLegend")})
  
  poisson <- reactive({
    df1 = data.frame(events = seq(input$n), density = dpois(seq(input$n), input$lambda1), group = "poisson1")
    df2 = data.frame(events = seq(input$n), density = dpois(seq(input$n), input$lambda2), group = "poisson2")
    df = rbind(df1, df2)
    df
  })
  
  output$table <- renderDataTable({
      pokemon %>% dplyr::select(id, pokemon, input$varX, input$varY, input$varZ, input$varA)
  })
  
  data <- reactive({
    pokemon$xx = pokemon[[input$varX]]
    pokemon$yy = pokemon[[input$varY]]
    pokemon$zz = pokemon[[input$varZ]]
    pokemon$aa = pokemon[[input$varA]]
    pokemon$zz = as.factor(pokemon$zz)
    pokemon
  })
  
  
  
  ###################################
  ## GGPLOTLY
  ###################################
  output$myggplotly <- renderPlotly({
    
    if(input$type == "scatter") {
      plot = data() %>% ggplot(., aes(xx, yy, color=zz)) + geom_point(size=input$dotsize, alpha=opacity) + labs(x = input$varX, y = input$varY)
      # plot = plot + geom_smooth()
    }
    else if(input$type == "boxplot") {
      plot = data() %>% ggplot(., aes(y = xx, x = zz, colour = zz)) + ggplot2::geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2), size=input$dotsize) + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(y = input$varY, x = "" ) + coord_flip()
    }
    else if(input$type == "barchart") {
      plot = data() %>% dplyr::count(zz, aa) %>% ggplot(., aes(n, zz, fill=aa)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab(input$varZ)
    }
    else if(input$type == "piechart") {
      text = paste0("\n   Piecharts can't be rendered in ggplotly. \n",
                   "    Static piechart could be built using ggplot.\n")
      plot = ggplot() + annotate("text", x = 3, y = 25, size=5, label = text, color = colour)
    }
    else if(input$type == "density") {
      plot = data() %>% ggplot(., aes(xx)) + geom_histogram(aes(y = ..density..), bins = 50, colour = 1, fill = "green") + geom_density() + labs(x = input$varX) + geom_rug()
    }
    
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
    else if(input$ggplot_theme == "theme_linedraw") {
      plot = plot + theme_linedraw()
    }
    else if(input$ggplot_theme == "theme_light") {
      plot = plot + theme_light()
    }
    
    title = paste0(stringr::str_to_title(input$type), " plot example using <i>ggplotly</i>.")
    
    if(input$isLegend) { # it has to be AFTER ggplot_theme
      plot = plot + theme(legend.position = "none")
    }
    plot = plot + ggtitle(title) 
    # + theme(legend.title = element_blank()) - works only in static ggplot
    
    plot
  })
  
  
  
  output$ggplotly_code <- renderText({
    if(input$type == "scatter") {
      string = paste0('pokemon %>% ggplot(., aes(', input$varX, ', ', input$varY, ', color=', input$varZ, ')) + geom_point(size=', input$dotsize, ', alpha=', opacity, ') + labs(x = ', add_quotes(input$varX), ', y = ', add_quotes(input$varY), ')')
    }
    else if(input$type == "boxplot") {
      string = paste0(' pokemon %>% ggplot(., aes(y = ', input$varX, ', x = ', input$varZ, ', colour = ', input$varZ, ')) + geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) + theme(axis.text.x = element_text(angle=60, hjust=1)) + coord_flip() + labs(y =', add_quotes(input$varY), ', x = "")')
    }
    else if(input$type == "density") {
      string = paste0('pokemon %>% ggplot(., aes(', input$varX, ')) + geom_histogram(aes(y = ..density..), bins = 30, colour = 1, fill = "green") + geom_density() + geom_rug() + labs(x = ', add_quotes(input$varX), ')')
    }
    else if(input$type == "barchart") {
      string = paste0('pokemon %>% dplyr::count(', input$varZ, ', ', input$varA, ') %>% ggplot(., aes(n, ', input$varZ, ', fill=', input$varA, ')) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab(', add_quotes(input$varZ), ') ')
    }
    else if(input$type == "piechart") {
      string = paste0(' pokemon %>% dplyr::count(', input$varZ, '), aes(x="", y = n, fill=', input$varZ, ')) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)')
    }
    
    string = paste0(string, " + ", input$ggplot_theme, "()")
    
    if(input$isLegend) {
      string = paste0(string, ' + theme(legend.position = "none")')
    }
    
    title = paste0(stringr::str_to_title(input$type), " plot example using <i>ggplotly</i>.")
    
    if(input$type != "piechart") {
      paste0("ggplotly(", string, " + ggtitle(", add_quotes(title), ") )")
    }
    else {
      ""
    }
   
  })
  
  
  
  ###################################
  ## Plotly
  ###################################
  output$plotly <- renderPlotly({
    
    title = paste0(stringr::str_to_title(input$type), " plot example by <i>Plotly</i>.")
    
    if(input$type == "scatter") {
      plot = data() %>% plot_ly(colors = RColorBrewer::brewer.pal(8, "Set2")) %>% add_trace(x=~xx, y=~yy, color=~zz, type="scatter", mode="markers", opacity=opacity, marker = list(size = input$dotsize), text = ~paste(" ID", id, "\n", "pokemon", pokemon, "\n", "type: ", type_1,  "\n", "Egg group: ", egg_group_1), hoverinfo=c("text")) 
      plot = plot %>% layout( xaxis = list(title = input$varX), yaxis = list(title = input$varY ))
    }
    else if(input$type == "boxplot") {
      plot = data() %>% plot_ly(colors = "Set3", type = "box", x =~xx, color=~zz, boxpoints="all", jitter=0.5, pointpos = 0, marker = list(size = input$dotsize), text = ~paste(" ID", id, "\n", "pokemon", pokemon, "\n", "type: ", type_1,  "\n", "Egg group: ", egg_group_1), hoverinfo=c("text"))
    }
    else if(input$type == "density") {
      fit <- density(data()$xx)
      plot = plot_ly(data = data(), x = ~xx, type = "histogram", name = "histogram") %>% add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "density") %>% layout(yaxis2 = list(overlaying = "y", side = "right")) 
      plot = plot %>% layout(xaxis = list(title = input$varX))
    }
    else if(input$type == "piechart") {
      plot = data() %>% dplyr::count(zz) %>% plot_ly(labels = ~zz, values=~n, type="pie", colors = RColorBrewer::brewer.pal(8, "Set2"), textposition = 'outside', textinfo = 'label+percent', hole = 0.2)
      plot = plot %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    else if(input$type == "barchart") {
      plot = data()  %>% dplyr::count(zz, aa) %>% plot_ly(colors = RColorBrewer::brewer.pal(8, "Set2")) %>% add_trace(type='bar', x =~n, y=~zz, color=~aa, text = ~paste(zz, '\n', aa, '\n', 'Number ', n), hoverinfo=c('text')) %>% layout(barmode = 'stack', margin=list(l = 240)) 
      plot = plot %>% layout(yaxis = list(title = input$varZ), xaxis = list(title = "total count"))
    }
    
    if(input$isLegend) {
      plot = plot %>% layout(showlegend = FALSE)
    }
    
    plot = plot %>% layout(title = title)
    plot 
  })
  
  
  output$plotly_code <- renderText({
    title = paste0(stringr::str_to_title(input$type), " plot example by <i>Plotly</i>.")
   
    if(input$type == "scatter") {
      string = paste0('pokemon %>% plot_ly %>% add_trace(x=~', input$varX, ', y=~', input$varY, ', color=~', input$varZ, ' , type="scatter", mode="markers", opacity=', opacity, ', marker = list(size = ', input$dotsize, '), text = ~paste(" ID", id, "pokemon", pokemon, "type: ", type_1,  "Egg group: ", egg_group_1), hoverinfo=c("text")) %>% layout( xaxis = list(title = ', add_quotes(input$varX), '))')
    }
    else if(input$type == "boxplot") {
      string = paste0('pokemon %>% plot_ly(type = "box", x =~', input$varX, ', color=~', input$varZ, ', boxpoints="all", jitter=0.5, pointpos = 0, marker = list(size = ', input$dotsize, '), text = ~paste(" ID", id, "pokemon", pokemon, "type: ", type_1,  "Egg group: ", egg_group_1), hoverinfo=c("text")) ')
    }
    else if(input$type == "piechart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ") %>% plot_ly(labels = ~", input$varZ, ", values=~n, type='pie', textposition = 'outside', textinfo = 'label+percent', hole = 0.2) %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))")
    }
    else if(input$type == "barchart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ", ", input$varA, ") %>% plot_ly() %>% add_trace(type='bar', x =~n, y=~", input$varZ, ", color=~", input$varA, ", text = ~paste(", input$varZ, ", ", input$varA, ", 'Number ', n), hoverinfo=c('text')) %>% layout(barmode = 'stack', margin=list(l=240)) %>% layout( yaxis = list(title = ", add_quotes(input$varZ), "))")
    }
    else if(input$type == "density") {
      string = paste0(' 
        fit <- density(pokemon[["', input$varX, '"]])
        plot_ly(data = pokemon, x = ~', input$varX, ', type = "histogram", name = "histogram") %>% add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "density") %>% layout(yaxis2 = list(overlaying = "y", side = "right")) %>% layout(xaxis = list(title =', add_quotes(input$varX), ')) ')
    }
    
    if(input$isLegend) {
      string = paste0(string, "%>% layout(showlegend = FALSE)")
    }
    string = paste0(string, " %>% layout(title=", add_quotes(title), ")")
    
  })
  
  
  ###################################
  ## HIGHCHART
  ###################################
  output$highchart <- renderHighchart2({
    # !!!!!!!!!! ISSUE JS function should be a string
    if(input$type == "scatter") {
      if(input$toBubble) {
          hcplot = highchart(type = "chart") %>% hc_add_series(data(), "bubble", hcaes(x = xx, y = yy, group = zz)) %>% hc_title(text = 'Highcharter: bubble plot', style = list(fontWeight = 'bold')) %>% hc_yAxis(title = list(text = input$varY), allowDecimals = FALSE) %>% hc_tooltip(formatter = JS("function(){ return ('pokemon: ' + this.point.pokemon + ' <br> id: ' + this.point.id + '<br> base: ' + this.point.base_experience)}")) %>% hc_xAxis(title = list(text = input$varX), allowDecimals = FALSE)
      }
      else {
         hcplot = highchart(type = "chart") %>% hc_add_series(data(), "scatter", hcaes(x = xx, y = yy, group = zz)) %>% hc_title(text = 'Highcharter: scatter plot', style = list(fontWeight = 'bold')) %>% hc_yAxis(title = list(text = input$varY), allowDecimals = FALSE) %>% hc_tooltip(formatter = JS("function(){ return ('pokemon: ' + this.point.pokemon + ' <br> id: ' + this.point.id + '<br> base: ' + this.point.base_experience)}")) %>% hc_xAxis(title = list(text = input$varX), allowDecimals = FALSE) %>% hc_plotOptions(buuble = list( marker = list(fillColor = NULL, lineWidth = 2, lineColor = NULL, radius = input$dotsize)))
      }
    }
    else if(input$type == "barchart") {
      hcplot = data() %>% dplyr::count(zz, aa) %>% hchart(type = 'bar', hcaes(x = zz, y = n, group=aa)) %>% hc_plotOptions(bar=list(stacking='stack')) %>% hc_title(text = 'Barplot by <i>Highcharter </i> ', margin = 20, align = "left", style = list(color = '#22A884', useHTML = TRUE)) %>% hc_xAxis(title=list(text=input$varZ)) 
    }
    else if(input$type == "boxplot") {
      plot = data_to_boxplot(data(),  variable = xx, group_var = zz, add_outliers = TRUE)
      hcplot = highchart() %>% hc_xAxis(type = 'category') %>% hc_add_series_list(plot) %>% hc_add_series(data = data(), type = "scatter", hcaes(x = zz, y = xx, group=zz))  %>% hc_xAxis(title = list(text = input$varX), allowDecimals = TRUE) %>% hc_plotOptions(scatter=list(jitter=list(x=0.08, y=0), marker=list(radius = input$dotsize)))
    }
    else if(input$type == "piechart") {
      hcplot = data() %>% dplyr::count(zz) %>% hchart(type ="pie", hcaes(zz, y = n))
    }
    else if(input$type == "density") {
      hcplot = hchart(data()[[input$varX]], yAxis = 0, breaks = 50) %>% hc_add_series(density(pokemon[[input$varX]], na.rm = TRUE), type = "area", opacity=0.7, yAxis = 1) %>% hc_yAxis_multiples(create_axis(naxis = 2, heights = c(2, 1)))
    }
    
    title = paste0('Using Highcharter for ', input$type, ' plot')
    
    hcplot = hcplot %>% hc_title(text = title, style = list(fontWeight = 'bold'))
    
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
    
    title = paste0("Using Highcharter for ", input$type, " plot.")
    title = stringr::str_c(title)
    
    if(input$type == "scatter") {
      string = paste0 ( 
        " tooltips = function(){ return ('pokemon: ' + this.point.pokemon + ' <br> id: ' + this.point.id + '<br> base: ' + this.point.base_experience)}", 
        "\n", 
        "highchart() %>% hc_add_series(pokemon, 'scatter', hcaes(x = ", input$varX, ", y = ", input$varY,  ", group = ", input$varZ, ")) %>% hc_yAxis(title = list(text =", add_quotes(input$varY), "), allowDecimals = FALSE) 
        %>% hc_tooltip(formatter = JS( tooltips )) %>% hc_plotOptions(scatter = list( marker = list(fillColor = NULL,  lineWidth = 2, lineColor = NULL, radius =", input$dotsize, ")))")
    } # not ok, check for zz, xx
    else if(input$type == "boxplot") {
      string = paste0(" plot = data_to_boxplot(pokemon, variable = ", input$varX, ", group_var =", input$varZ, ", add_outliers = TRUE)", "\n",
      "highchart() %>% hc_xAxis(type = 'category') %>% hc_add_series_list(plot) %>% hc_add_series(data = pokemon, type = 'scatter', hcaes(x = ", input$varZ, ", y = ", input$varX, ", group = ", input$varZ, ")) %>% hc_plotOptions(scatter=list(jitter=list(x = 0.08, y = 0))) %>% hc_plotOptions(scatter=list(jitter=list(x=0.08, y=0), marker=list(radius =", input$dotsize, ")))
      "
     )
    }
    else if(input$type == "barchart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ", ", input$varA, ") %>% hchart(type = 'bar', hcaes(x = ", input$varZ, ", y = n, group=", input$varA, ")) %>% hc_plotOptions(bar=list(stacking='stack')) %>% hc_xAxis(title=list(text=", add_quotes(input$varZ), ")) ") 
    }
    else if(input$type == "piechart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ") %>% hchart(type ='pie', hcaes(", input$varZ, ", y = n))  ")
    } # OK
    else if(input$type == "density") {
      string = paste0('hchart(pokemon[[', add_quotes(input$varX), ']], yAxis = 0, breaks = 50) %>% hc_add_series(density(pokemon[[', add_quotes(input$varX), ']], na.rm = TRUE), type = "area", opacity=0.7, yAxis = 1) %>% hc_yAxis_multiples(create_axis(naxis = 2, heights = c(2, 1)))')
    }
    
    string = paste0(string,  " %>% hc_add_theme(", input$highchart_theme, ")")
    
    string = paste0(string, " %>% hc_title(text = '", title, "' , margin = 20, align = 'left', style = list(color = '#22A884', fontWeight = 'bold', useHTML = TRUE))")
    
    paste0(string, "  %>% hc_credits(enabled = TRUE, text = 'Sources: Highchater 2022', style = list(fontSize = '10px'), enabled = TRUE) ")
    
  })
  
  
  
  ###################################################
  ## rbokeh
  ###################################################
  
  output$rbokeh <- renderRbokeh({
   
    w = 1000
    h = 400
    title = paste0(stringr::str_to_title(input$type), " plot example by rbokeh.")
    
    if(input$isLegend) {
      plot = figure(title =  title, width = w, height = h, legend_location = NULL, alpha = opacity) 
    }
    else { # position = c("stack", "fill", "dodge")
      plot = figure(title = title, width = w, height = h, alpha = opacity, legend_location = "top_right") 
    }
    
    if(input$type == "scatter") {
      plot = plot %>% ly_points(x = xx, y = yy, color = zz, data = data(), hover = char.var, size = input$dotsize) %>% x_axis(label = input$varX) %>% y_axis(label = input$varY)
    }
    else if(input$type == "boxplot") {
      plot = plot %>% ly_boxplot(xx, zz, color = zz, data = data(), alpha = opacity) %>% y_axis(label = input$varY) %>% x_axis(label = "")
    }
    else if(input$type == "barchart") {
      data = data() %>% dplyr::count(zz, aa)
      plot = plot %>% ly_bar(y = zz, n, color = aa,  data = data, hover = TRUE) %>% theme_axis("x", major_label_orientation = "horizontal") %>% y_axis(label = input$varZ) %>% x_axis(label = "")
    }
    else if(input$type == "piechart") {
      plot = plot %>% rbokeh::ly_text("This type of plot is not available in Bokeh.", x = 0, y = 2, font_size = "15pt", color = "green", baseline = "middle", align = "center", font_style = "bold")
    }
    else if(input$type == "density") {
      plot = plot %>% ly_hist(xx, data = data(), breaks = 50, freq = FALSE) %>% ly_density(xx, data = data()) %>% x_axis(label = input$varX)
    }
    plot 
    
  })
  
  output$brokeh_code <- renderText({
  
    title = paste0(stringr::str_to_title(input$type), " plot example by rbokeh.")
    
    if(input$isLegend) {
      string = paste0(' figure(title = "',  title, '" width = 1000, height = 400, legend_location = NULL, alpha = ', opacity, ') ')
    }
    else {
      string = paste0('figure(title = "',  title, '", width = 1000, height = 400, alpha = ', opacity, ', legend_location = "top_right") ')
    }
    
    
    if(input$type == "scatter") {
      string = paste0(string,  ' %>% ly_points(x = ', input$varX, ', y = ', input$varY, ', color = ', input$varZ, ', data = pokemon, hover = c(', as.character(stringr::str_c(char.var, collapse = ", ")), '), size =',  input$dotsize, ') %>% x_axis(label = ', add_quotes(input$varX), ') %>% y_axis(label = ', add_quotes(input$varY), ')')
    }
    else if(input$type == "boxplot") {
      string = paste0(string, ' %>% ly_boxplot(', input$varX, ', ', input$varZ, ', color = ', input$varZ, ', data = pokemon, alpha = 0.8) %>% y_axis(label = ', add_quotes(input$varY), ')')
    }
    else if(input$type == "barchart") {
      string = paste0(string, " %>% ly_bar(", input$varZ, ", n, data = pokemon %>% dplyr::count(", input$varZ, ", sort=T), hover = TRUE) %>% theme_axis('x', major_label_orientation = 90) %>% x_axis(label = ", add_quotes(input$varZ), ")")
    }
    else if(input$type == "piechart") {
      string = paste0("", "\n")
    }
    else if(input$type == "density") {
      string = paste0(string, ' %>% ly_hist(', input$varX, ', data = pokemon, breaks = 50, freq = FALSE) %>% ly_density(', input$varX, ', data = pokemon) %>% x_axis(label =', add_quotes(input$varX), ') ')
    }
    
    string
    
  })
  
  ################################################################
  ## Echart
  ###############################################################
  
  output$echart <- renderEcharts4r({
    
    title = paste0(stringr::str_to_title(input$type), " plot example by Echart.")
    
    if(input$type == "scatter") { # to view only one subset of points: e_legend(selectedMode = "single")
      plot = data() %>% group_by(zz) %>% e_charts(xx) %>% e_scatter(yy, symbol_size = input$dotsize) %>% e_axis_labels(x = input$varX, y = input$varY) 
    }
    else if(input$type == "boxplot") {
      plot = data() %>% group_by(zz) %>% e_charts() %>% e_boxplot(xx, outliers = TRUE) %>% e_scatter(xx, symbol_size = input$dotsize, color = input$varZ, jitter_factor = 0.8)
    }
    ### https://echarts4r.john-coene.com/articles/advanced.html#stacked
    else if(input$type == "barchart") {
      plot = data() %>% dplyr::count(zz) %>% e_charts(zz) %>% e_bar(n, stack = 'grp', itemStyle = list(borderColor = "green", borderWidth = '3')) %>% e_flip_coords() 
    }
    else if(input$type == "piechart") {
      plot = data() %>% dplyr::count(zz) %>% e_charts(zz) %>% e_pie(n, radius = c('10%', '70%'))
    }
    # https://echarts4r.john-coene.com/articles/chart_types.html
    else if(input$type == "heatmap") {
      plot = data() %>% e_chart(zz) %>% e_heatmap(aa, xx) %>% e_visual_map(xx)
    }
    else if(input$type == "density") {
      plot = data() %>% e_chart() %>% e_histogram(serie = xx, y_index = 1, breaks = 50, name = "histogram") %>% e_density( serie = xx, smooth = FALSE,  name = "density", opacity = 0.4)
    }
    
    # additional common attributes
    # e_brush() adds selection tools
    plot = plot %>% e_theme(name = input$ehcart_theme) %>% e_tooltip() %>% e_brush() %>% e_title(text = title)
    
    if(input$isLegend) {
      plot = plot %>% e_legend(show = FALSE)
    }
    else {
      plot = plot %>% e_legend(orient = 'vertical', right = '5', top = '15%', type="scroll") 
    }
    plot
  })
  
  output$echart_code <- renderText({
    
    title = paste0(stringr::str_to_title(input$type), " plot example by Echart.")
    
    if(input$type == "scatter") {
      string = paste0("pokemon %>% group_by(", input$varZ, ") %>% e_charts(", input$varX, ") %>% e_scatter(", input$varY, ", symbol_size = ", input$dotsize, ") %>% e_axis_labels(x = ", add_quotes(input$varX), ", y = ", add_quotes(input$varY), ") ")
    }
    else if(input$type == "boxplot") {
      string = paste0("pokemon %>% group_by(", input$varZ, ") %>% e_charts() %>% e_boxplot(", input$varX, ", outliers = FALSE)  ")
    }
    else if(input$type == "barchart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ") %>% e_charts(", input$varZ, ") %>% e_bar(n, stack = 'grp')  ")
    }
    else if(input$type == "piechart") {
      string = paste0("pokemon %>% dplyr::count(", input$varZ, ") %>% e_charts(", input$varZ, ") %>% e_pie(n, radius = c('10%', '70%')) ")
    }
    else if(input$type == "density") {
      string = paste0("pokemon %>% e_chart() %>% e_histogram(serie = ", input$varX, ", y_index = 1, breaks = 50, name = 'histogram') %>% e_density( serie = ", input$varX, ", smooth = FALSE,  name = 'density', opacity = 0.4)") 
    }
    
    # take care of the legends
    if(input$isLegend) {
      string = paste0(string, "%>% e_legend(show = FALSE)")
    }
    else{
      string = paste0(string, "%>% e_legend(orient = 'vertical', right = '5', top = '15%', type='scroll')")
    }
    
    # additional common attributes
    paste0(string, " %>% e_theme(name = '", input$ehcart_theme, "') %>% e_tooltip() %>% e_brush() %>% e_title(text =", add_quotes(title)," )")

  })
  
}


shinyApp(ui = ui, server = server, enableBookmarking = "server")

