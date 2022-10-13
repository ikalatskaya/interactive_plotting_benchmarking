#' Helper functions for using Benchmarking app

spinner.colour = "#00AA51"
status = "success"

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



codeBox <- function(title, output) {
  box(title = title,
          output, 
          width = 12, 
          closable = TRUE, 
          id = "code_box"
      )
}