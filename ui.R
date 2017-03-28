library(shiny)
library(leaflet)
library(maptools)
library(sp)

# Read in everything
la <- read.csv("LA_acc_coeff_sig.csv", stringsAsFactors = F)
or <- read.csv("OC_acc_coeff_sig.csv", stringsAsFactors = F)
rv <- read.csv("RV_acc_coeff_sig.csv", stringsAsFactors = F)
sb <- read.csv("SB_acc_coeff_sig.csv", stringsAsFactors = F)
vn <- read.csv("VN_acc_coeff_sig.csv", stringsAsFactors = F)
dest <- read.csv("cat_descr.csv", stringsAsFactors = F)
sub <- readShapePoly("SoCal_place_2010_UA")
dfsub <- data.frame(sub)
names = as.character(unique(unlist(dfsub$NAME10)))
est = c('Apparel Retailing', 'Auto Services', 'Beer, Wine, and Liquor Stores', 'Child Care Services', 'Convenience Stores', 'Deposit-taking Institutions', 'Drinking Places', 'Drug Stores', 'Elementary and Secondary Schools', 'Full-Service Restaurants', 'Gas Stations', 'General Merchandise Retailing', 'Groceries', 'Hair Care Services', ' ', 'Home Products Retailing', 'Hospitals', 'Laundry', 'Limited-Service Food and Beverage', 'Medical Laboratories', 'Open Space', 'Other Learning', 'Other Personal Services', 'Personal Financial', 'Personal Products Retailing', 'Recreational Facilities and Instruction', 'Religious Organizations', 'Repair Services', 'Social Service Organizations', 'Specialty Food', 'Specialty Retailing')

shinyUI(navbarPage("Accessibility to everyday destinations in SoCal Cities.", id="nav",
  
  tabPanel("Access Across Cities", div(class="outer",
          tags$head(includeCSS("styles.css")),
          leafletOutput("map", width="100%", height="100%"),
          absolutePanel(id="controls", class="panel panel-default", fixed=T, draggable=T, 
                        top=60, left="auto", right=20, bottom="auto", width=330, height="auto",
                        selectInput("cent", label=strong("Zoom to City:"), selected="Irvine", choices=names[order(names)]),
                        actionButton("recenter", label="Re-center map"),
                        br(""),
                        radioButtons("topic", label=strong("Select Measure:"), selected="Abundance",
                                     choices = c("Abundance", "Percent serviced", "Total # of Destinations in city")),
                        selectInput("estabs1", label=strong("Select Destination Type:"), choices=est, selected="Apparel Retailing"),
                        actionButton("go1", label=strong("Click to Refresh after re-selecting", style="color:red")),
                        selectInput("city1", label=strong("Select city for details:"), selected="Agoura Hills", choices=names[order(names)]),
                        plotOutput("hist", height=235)),
          absolutePanel(id="controls", class="panel panel-default", fixed=T, draggable=T,
                        top=110, left=10, right="auto", bottom="auto", width=175, height="auto",
                        p(strong("Data notes:")),
                        h6("-- This map shows how accessible homes in a city are to a variety of everyday destinations."),
                        h6("-- ", strong("Abundance"), "shows how many of that destination type are within one mile of the average home in the selected city."),
                        h6("-- ", strong("Percent serviced"), "displays the % of homes in a city which have at least one of that destination in one mile."),
                        h6("-- All distances are calculated along a street network."),
                        h6("-- 1/5 of the region's cities are each color on the map."),
                        h6("-- Make sure to click Refresh after changing the selection."),
                        h6("-- By the UCI", a("Metropolitan Futures Initiative.", href="http://mfi.soceco.uci.edu", target="_blank"),
                           " See", a("the full report here.", href="http://mfi.soceco.uci.edu/category/quarterly-report/", target="_blank")))
          )),

  
  tabPanel("What Impacts Access?",
  sidebarLayout(
    sidebarPanel(
      selectInput("estabs2", label=strong("Select Destination Type:", style="color:blue"), choices=est, selected="Groceries"),
      radioButtons("county", label=strong("Select County:", style="color:blue"), selected=2, 
                   choices = list("Los Angeles"=1, "Orange"=2, "Riverside"=3, "San Bernardino"=4, "Ventura"=5)),
      conditionalPanel("input.county == 1",
                       selectInput("cityLA", label=strong("Select City:", style="color:blue"), selected="", choices=as.character(la$name)[order(as.character(la$name))]),
                       actionButton("goLA", label=strong("Click to Start & to Refresh", style="color:red"))),
      conditionalPanel("input.county == 2",
                       selectInput("cityOR", label=strong("Select City:", style="color:blue"), selected="Aliso Viejo", choices=as.character(or$name)[order(as.character(or$name))]),
                       actionButton("goOR", label=strong("Click to Start & to Refresh", style="color:red"))),
      conditionalPanel("input.county == 3",
                       selectInput("cityRV", label=strong("Select City:", style="color:blue"), selected="", choices=as.character(rv$name)[order(as.character(rv$name))]),
                       actionButton("goRV", label=strong("Click to Start & to Refresh", style="color:red"))),
      conditionalPanel("input.county == 4",
                       selectInput("citySB", label=strong("Select City:", style="color:blue"), selected="", choices=as.character(sb$name)[order(as.character(sb$name))]),
                       actionButton("goSB", label=strong("Click to Start & to Refresh", style="color:red"))),
      conditionalPanel("input.county == 5",
                       selectInput("cityVN", label=strong("Select City:", style="color:blue"), selected="", choices=as.character(vn$name)[order(as.character(vn$name))]),
                       actionButton("goVN", label=strong("Click to Start & to Refresh", style="color:red"))),
      br(), 
      p(strong("Destination Type Notes:", style="color:blue")),
      textOutput("var_desc"),
      br(),
      h6("-- By the UCI", a("Metropolitan Futures Initiative.", href="http://mfi.soceco.uci.edu", target="_blank"),
         " See", a("the full report here.", href="http://mfi.soceco.uci.edu/category/quarterly-report/", target="_blank"))),
    mainPanel(
      h2(strong("What Impacts How Close Things Are to Me?", style="color:blue")),
      h6(em("-- The bars below correspond to characteristics of the ", strong("home ", style="color:blue"), "or the ", strong("neighborhood.", style="color:blue"), "For housing types, e.g. Condo, we show the effect of Condo status on access versus a detached, single-family home.")),
      h6(em("-- The height of each bar shows how much impact that characteristic has on accessibility to the chosen destination.")),
      plotOutput("bar", height = 500))
    ))
))
    
 