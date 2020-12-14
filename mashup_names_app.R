library(stringr)
library(babynames)
library(rebus)
library(dplyr)
library(Hmisc)
library(shinythemes)

babynames1 <- babynames
# Extracting vector of names only and from 2015
babynames_2015 <- filter(babynames1, year == 2015)
all_names <- tolower(babynames_2015$name)

#create last n letters pattern
last_n_pattern <- function(name1, n) {
  name1 = tolower(name1)
  if(n == 2) {
    str_extract(name1, pattern = LOWER %R% LOWER %R% END)
  } else if(n == 3) {
    str_extract(name1, pattern = LOWER %R% LOWER %R% LOWER %R% END)
  } else if(n == 4) {
    str_extract(name1, pattern = LOWER %R% LOWER %R% LOWER %R% LOWER %R% END)
  }
}
#create first n letters pattern
first_n_pattern <- function(name1, n) {
  name1 = tolower(name1)
  if(n == 2) {
    str_extract(name1, pattern = START %R% LOWER %R% LOWER)
  } else if(n == 3) {
    str_extract(name1, pattern = START %R% LOWER %R% LOWER %R% LOWER)
  } else if(n == 4) {
    str_extract(name1, pattern = START %R% LOWER %R% LOWER %R% LOWER %R% LOWER)
  }
}
#make names by calling the pattern functions
pattern_names <- function(startname, first_or_last, n) {
  startname <- tolower(startname)
  if(first_or_last == "first") {
    pttrn = first_n_pattern(startname, n)
    match_names = str_subset(all_names, pattern = pttrn)
    names_splt = str_split(match_names, pattern = pttrn, simplify = TRUE)
    name_start = names_splt[,1]
    name_start = names_splt[str_length(name_start) > 0, 1]
    return(capitalize(unique(str_c(name_start, startname))))
  } else if(first_or_last == "last") {
    pttrn = last_n_pattern(startname, n)
    match_names = str_subset(all_names, pattern = pttrn)
    names_splt = str_split(match_names, pattern = pttrn, simplify = TRUE)
    name_end = names_splt[,2]
    name_end = names_splt[str_length(name_end) > 0, 2]
    return(capitalize(unique(str_c(startname, name_end))))
  }
}
#curate names depending on name length
mashup_names <- function(startname) {
  if(str_length(startname) > 4) {
    unique(c(pattern_names(startname, "last", 4), pattern_names(startname, "last", 3),
             pattern_names(startname, "first", 4), pattern_names(startname, "first", 3)))
  }
  else if(str_length(startname) > 3){
    unique(c(pattern_names(startname, "last", 3), pattern_names(startname, "first", 3)))
  }
  else if(str_length(startname) > 2) {
    unique(c(pattern_names(startname, "last", 2), pattern_names(startname, "first", 2)))
  }
} 

my_css <- "
#get_mashups {
/* Change the background colour of the download button
to orange. */
background: magenta;
font-size: 20px;
}
"

#UI
ui <- fluidPage(
  tags$style(my_css),
  theme = shinytheme("united"),
  
  # App title
  headerPanel("Name Mashups"),
  p("Enter a name and 
                I'll combine it with other names 
                that have similar letter patterns"),
  sidebarLayout(
    #Input
    sidebarPanel(
      # Enter name to mash up
      textInput(inputId = "og_name", 
                label = "Enter name:", 
                placeholder = "Letters only, please!"),
      
      # Action button get mashups
      actionButton(inputId = "get_mashups", 
                   label = "Get mashups")
    ),
    #Output
    mainPanel(
      h3("Mashups:"),
      verbatimTextOutput("result")
    )
    )
  )
#Server
server <- function(input, output) {
  # name_results <- reactive({
  #   mashup_names(input$og_name)
  # })

  name_results <- eventReactive(
    eventExpr = input$get_mashups,
    valueExpr = { mashup_names(input$og_name) },
    ignoreNULL = FALSE
  )
  output$result <- renderPrint({
    name_results()
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)