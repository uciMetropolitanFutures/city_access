library(shiny)
library(leaflet)
library(maptools)
library(sp)

# Read in everything
dest <- read.csv("cat_descr.csv", stringsAsFactors = F)
la <- read.csv("LA_acc_coeff_sig.csv")
or <- read.csv("OC_acc_coeff_sig.csv")
rv <- read.csv("RV_acc_coeff_sig.csv")
sb <- read.csv("SB_acc_coeff_sig.csv")
vn <- read.csv("VN_acc_coeff_sig.csv")

sub <- readShapePoly("SoCal_place_2010_UA")
dfsub <- data.frame(sub)
names = as.character(unique(unlist(dfsub$NAME10)))
est = c('Apparel Retailing', 'Auto Services', 'Beer, Wine, and Liquor Stores', 'Child Care Services', 'Convenience Stores', 'Deposit-taking Institutions', 'Drinking Places', 'Drug Stores', 'Elementary and Secondary Schools', 'Full-Service Restaurants', 'Gas Stations', 'General Merchandise Retailing', 'Groceries', 'Hair Care Services', 'SKIP', 'Home Products Retailing', 'Hospitals', 'Laundry', 'Limited-Service Food and Beverage', 'Medical Laboratories', 'Open Space', 'Other Learning', 'Other Personal Services', 'Personal Financial', 'Personal Products Retailing', 'Recreational Facilities and Instruction', 'Religious Organizations', 'Repair Services', 'Social Service Organizations', 'Specialty Food', 'Specialty Retailing', 'Rail Stations')

shinyUI(navbarPage("Accessibility to Retail and Services in SoCal Cities.", id="nav",
  
  tabPanel("Proximity Across Cities", div(class="outer",
          tags$head(includeCSS("styles.css")),
          leafletOutput("map", width="100%", height="100%"),
          absolutePanel(id="controls", class="panel panel-default", fixed=T, draggable=T, 
                        top=60, left="auto", right=20, bottom="auto", width=330, height="auto",
                        selectInput("cent", label=strong("Zoom to City:"), selected="Irvine", choices=names[order(names)]),
                        actionButton("recenter", label="Re-center map"),
                        br(""),
                        radioButtons("topic", label=strong("Select Topic:"), selected="Number of Businesses",
                                     choices = c("Number of Businesses", "Avg. Number within 1-mile", "Pct. serviced within 1-mile")),
                        selectInput("estabs1", label=strong("Select Estabs:"), choices=est, selected=""),
                        actionButton("go1", label="Go/Refresh"),
                        selectInput("city1", label=strong("Show value for city:"), selected="Agoura Hills", choices=names[order(names)]),
                        verbatimTextOutput("data")),
          absolutePanel(id="controls", class="panel panel-default", fixed=T, draggable=T,
                        top=110, left=10, right="auto", bottom="auto", width=160, height="auto",
                        p("Data notes:"),
                        h6("Please be patient while map loads!"),
                        h6("This is..."),
                        h6("That is..."),
                        h6("-- See", a("our website", href="http://mfi.soceco.uci.edu", target="_blank"), "for details."))
          )),

  
  
  tabPanel("What Impacts Proximity?",
  sidebarLayout(
    sidebarPanel(
      selectInput("estabs2", label=strong("Select Store Type"), choices=est, selected="Groceries"),
      radioButtons("county", label=strong("Select County"), selected=2,
                   choices = list("Los Angeles"=1, "Orange"=2, "Riverside"=3, "San Bernardino"=4, "Ventura"=5)),
      conditionalPanel("input.county == 1",
                       selectInput("cityLA", label=strong("Select City"), selected="", choices=as.character(la$name)[order(as.character(la$name))]),
                       actionButton("goLA", label="Go/Refresh")),
      conditionalPanel("input.county == 2",
                       selectInput("cityOR", label=strong("Select City"), selected="", choices=as.character(or$name)[order(as.character(or$name))]),
                       actionButton("goOR", label="Go/Refresh")),
      conditionalPanel("input.county == 3",
                       selectInput("cityRV", label=strong("Select City"), selected="", choices=as.character(rv$name)[order(as.character(rv$name))]),
                       actionButton("goRV", label="Go/Refresh")),
      conditionalPanel("input.county == 4",
                       selectInput("citySB", label=strong("Select City"), selected="", choices=as.character(sb$name)[order(as.character(sb$name))]),
                       actionButton("goSB", label="Go/Refresh")),
      conditionalPanel("input.county == 5",
                       selectInput("cityVN", label=strong("Select City"), selected="", choices=as.character(vn$name)[order(as.character(vn$name))]),
                       actionButton("goVN", label="Go/Refresh")),
      br(), 
      p(strong("Data Notes:")),
      textOutput("var_desc"),
      br(),
      a("UCI Metropolitan Futures Initiative", href="http://mfi.soceco.uci.edu"),
      img(src="mfi.png", height=200, width=200)),
    mainPanel(
      h2("What Impacts How Close Things Are to Me?"),
      p(em("Factors which influence accessibility are displayed below. ")),   #em is used for italics
      plotOutput("bar", height = 400))
    ))
))
    
 