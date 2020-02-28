#
# This is the user-interface definition of a Shiny web application.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries
library('shiny')
library('ggplot2')


# Functions for creating fluid page layouts. A fluid page layout consists of rows which
# in turn include columns. Rows exist for the purpose of making sure their elements appear on the 
# same line (if the browser has adequate width). Columns exist for the purpose of defining how much 
# horizontal space within a 12-unit wide grid it's elements should occupy. Fluid pages scale their 
# components in realtime to fill all available browser width.
#
# theme is reference to .ccs file, to make page look like you describe it in that file
fluidPage(theme = "bootstrap.css",
  
  # this is analogy to HTML: 
  # <head>
  #   <style>
  #   ...
  #   </style>
  # </head>
  # we just adjusted color and background color of validation\warning messages
  tags$head(tags$style("#validation_error_year{color: rgb(214, 0, 89); background: rgba(214, 0, 89, 0.2)}",
                       "#validation_error_area{color: rgb(214, 0, 89); background: rgba(214, 0, 89, 0.2)}")),
    
  # Create a panel containing an application title.
    titlePanel("Real estate price estimator"),
  
  # Create a sidebar panel containing input controls that can in turn be passed to sidebarLayout.  
    sidebarPanel(
        # Input: dropdown for neighborhood
        selectInput(inputId = "neighborhood",
                    label = "Choose a neighborhood:",
                    choices = c("Any neighborhood",
                                "Bloomington Heights",
                                "Bluestem",
                                "Briardale",
                                "Brookside",
                                "Clear Creek",
                                "College Creek",
                                "Crawford",
                                "Edwards",
                                "Gilbert",
                                "Iowa DOT and Rail Road",
                                "Meadow Village",
                                "Mitchell",
                                "North Ames",
                                "Northridge",
                                "Northpark Villa",
                                "Northridge Heights",
                                "Northwest Ames",
                                "Old Town",
                                "South & West of Iowa State University",
                                "Sawyer",
                                "Sawyer West",
                                "Somerset",
                                "Stone Brook",
                                "Timberland",
                                "Veenker")),
        
        # extra space between elements
        br(),
        
        # Input: House quality interval
        sliderInput("House_quality", "House quality",
                    min = 1, max = 10,
                    value = 5),
        
        # description of the slider
        helpText("1 is 'Very Poor' and 10 is 'Very Excellent'"),
        
        # extra space between elements
        br(),
        
        # Input: Numeric entry for living area square feet
        numericInput(inputId = "living_area",
                     label = "Living area in square feet:",
                     value = 1500),
        
        # description of the numeric input
        helpText("Note: the valid number is positive integer number less than 1 billion"),
        
        # Failing validation message
        verbatimTextOutput("validation_error_area"),
        
        # extra space between elements
        br(),
        
        # Input: Quality of the material on the exterior
        sliderInput("ExterQual", "Quality of the material on the exterior:",
                    min = 1, max = 5,
                    value = 2),
        
        # description of the slider
        helpText("1 is 'Poor' and 5 is 'Excellent'"),
        
        # extra space between elements
        br(),
        
        # Input: Numeric entry for Original construction date
        numericInput(inputId = "year_built",
                     label = "Original construction date:",
                     value = 1970),
        
        # description of the numeric input
        helpText("Note: the valid year is a year between 1879 and current year"),
        
        # Failing validation message
        verbatimTextOutput("validation_error_year"),
    
        # end of sidebarPanel    
    ),
    
  
    # Create a main panel containing output elements that can in turn be passed to sidebarLayout
    mainPanel(
    # Create a tabset that contains tabPanel elements. Tabsets are useful for dividing output into multiple independently viewable sections.
      tabsetPanel(type = "tabs",
                  tabPanel("Historical data", verbatimTextOutput("choice"), plotOutput("plot"), plotOutput("plot2")),
                  tabPanel("Prediction", verbatimTextOutput("prediction"))
      ))
  
  
  
)