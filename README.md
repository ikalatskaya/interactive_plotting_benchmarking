# Interactive plotting benchmarking

## Intro

Visualizations grant users the ability to explore, manipulate, and interact with data by employing dynamic charts, changing colors, and shapes based on queries or interactions. 

What is interactive visualization?

Interactive visualizations are produced using data visualization tools which allow for direct modification of elements on a graphical plot.

HTML widgets work just like R plots except they produce interactive web visualizations. HTML widgets can be used at the R console as well as embedded in R Markdown reports and Shiny web applications. 

## Objectives

This dashboard presents and compares five most popular html widget tools for building interactive plots in Rshiny. 

Interactivity: long lasting RShiny apps should be interactive and self-explicit. R packages that support html widgets might help achive this goal pretty easily: plot_ly, ggplotly, highcharter, bokeh or echart? 

## Product

I have built an Rshiny dashboard that shows and compares five different R packages that help to build interactive plots for Rshiny dashboards and Rmarkdowns.

It could be seen here: https://minijen.shinyapps.io/benchmarking/


<img width="1354" alt="Screen Shot 2022-09-26 at 8 29 18 PM" src="https://user-images.githubusercontent.com/20693710/192404342-eeba23cc-cb94-4ca6-b20e-6787e0f1baad.png">


## List of the tested packages:

<b> Plotly </b>
Plotly provides online graphing, analytics, and statistics tools for individuals and collaboration, as well as scientific graphing libraries for Python, R, MATLAB, Perl, Julia, Arduino, and REST. Plotly is a technical computing company headquartered in Montreal, Quebec. It develops online data analytics and visualization tools. 

<b> GGPLOTLY </b>

<b> HIGHCHARTER </b>

<b> BOKEH </b>
A native R plotting library that provides a flexible declarative interface for creating interactive web-based graphics, backed by the Bokeh visualization library. The Bokeh library is written and maintained by the Bokeh Core Team consisting of several members of Continuum Analytics and other members of the open source community. 

<b> ECHART4R </b>

### Very short list of the available R packages (~100 in total):

- DT tables
- plotly and ggplotly
- highcharter
- leaflet (interactive geo plots, scatter geoplots)
- visNetwork (interactive networks)
- wordcloud2
- d3heatmap
- networkD3
- wordcloud2

### Resources

HTML widgets in R
https://www.htmlwidgets.org/showcase_plotly.html

Gallary of widgets
http://gallery.htmlwidgets.org/

#### Full manual for highcharter
https://cran.r-project.org/web/packages/highcharter/highcharter.pdf
https://www.datacamp.com/community/tutorials/data-visualization-highcharter-r
https://www.datacamp.com/tutorial/data-visualization-highcharter-r
https://www.kaggle.com/code/nulldata/beginners-guide-to-highchart-visual-in-r/report#scatter-plot

#### Manual for plotly
https://plotly.com/r/

#### Good intro to echart4r
https://www.infoworld.com/article/3607068/plot-in-r-with-echarts4r.html
https://echarts4r.john-coene.com/articles/chart_types.html


#### Rbokeh 
https://hafen.github.io/rbokeh/articles/rbokeh.html


<br/>
<br/>

                  
