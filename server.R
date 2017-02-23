library(shiny)
library(leaflet)
library(maptools)
library(sp)
#setwd("C:/Users/Kevin/Documents/a_UCI/webmap/access")

la <- read.csv("LA_acc_coeff_sig.csv")
or <- read.csv("OC_acc_coeff_sig.csv")
rv <- read.csv("RV_acc_coeff_sig.csv")
sb <- read.csv("SB_acc_coeff_sig.csv")
vn <- read.csv("VN_acc_coeff_sig.csv")
dest <- read.csv("cat_descr.csv")
sub <- readShapePoly("SoCal_place_2010_UA")
dfsub <- data.frame(sub)
est = c('Apparel Retailing', 'Auto Services', 'Beer, Wine, and Liquor Stores', 'Child Care Services', 'Convenience Stores', 'Deposit-taking Institutions', 'Drinking Places', 'Drug Stores', 'Elementary and Secondary Schools', 'Full-Service Restaurants', 'Gas Stations', 'General Merchandise Retailing', 'Groceries', 'Hair Care Services', 'SKIP', 'Home Products Retailing', 'Hospitals', 'Laundry', 'Limited-Service Food and Beverage', 'Medical Laboratories', 'Open Space', 'Other Learning', 'Other Personal Services', 'Personal Financial', 'Personal Products Retailing', 'Recreational Facilities and Instruction', 'Religious Organizations', 'Repair Services', 'Social Service Organizations', 'Specialty Food', 'Specialty Retailing', 'Rail Stations')

shinyServer(function(input, output) {

  #### MAP PANEL ####
  center <- reactiveValues(x_coord=-117.7736, y_coord=33.67801)
  observeEvent(input$recenter, {
    center$x_coord = dfsub$x_coord[dfsub$NAME10==input$cent]
    center$y_coord = dfsub$y_coord[dfsub$NAME10==input$cent]
  })
  
  options = reactiveValues(choose = "et1")
  observeEvent(input$go1, {   
    link1 = switch(input$topic, "Number of Businesses"="et", "Avg. Number within 1-mile"="Avg_", "Pct. serviced within 1-mile"="Nonzero_")
    options$choose <- paste("^", link1, grep(input$estabs1, est), "$", sep="")
  })
  
 finalMap <- reactive ({
    choice = dfsub[,grep(options$choose, colnames(dfsub))]
    print(options$choose)
    print(head(choice))
    print(summary(choice))
    if(input$topic=="Number of Businesses"){
      br1 = as.numeric(quantile(choice, 0.25, na.rm=T))+1
      br2 = as.numeric(quantile(choice, 0.5, na.rm=T))
      br3 = as.numeric(quantile(choice, 0.75, na.rm=T))
      pal <- colorBin("Blues", choice, bins=c(0, br1, br2, br3, as.numeric(summary(choice)[6])), na.color="#B0171F")}
    else if(input$topic=="Avg. Number within 1-mile"){
      br1 = as.numeric(quantile(choice, 0.25, na.rm=T))+0.01
      br2 = as.numeric(quantile(choice, 0.5, na.rm=T))+0.02
      br3 = as.numeric(quantile(choice, 0.75, na.rm=T))+0.03
      pal <- colorBin("Blues", choice, bins=c(0, br1, br2, br3, as.numeric(summary(choice)[6])), na.color="#B0171F")}
    else{
      br1 = as.numeric(quantile(choice, 0.25, na.rm=T))+0.01
      br2 = as.numeric(quantile(choice, 0.5, na.rm=T))+0.02
      br3 = as.numeric(quantile(choice, 0.75, na.rm=T))+0.03
      pal <- colorBin("Blues", choice, bins=c(0, br1, br2, br3, 1.0), na.color="#B0171F")}
    
    #pal <- colorQuantile("Blues", choice, n=5, na.color="#B0171F")
    m = leaflet(sub) %>% setView(lng=center$x_coord, lat=center$y_coord , zoom=10) %>% addTiles() %>%
      addPolygons(data=sub, stroke=T, weight=1.1, fillColor=~pal(choice), color="black", fillOpacity=0.5,
                  opacity=1, popup=~paste(NAME10)) %>%
      addLegend("bottomleft", pal=pal, values=~choice, opacity=0.75, na.label=~paste("No", input$estabs1, "in city"),
                title=paste(input$estabs1, input$topic))
  })
  output$map = renderLeaflet(finalMap())
  
  output$data <- renderPrint({
    dfsub[,grep(paste("^", options$choose, "$", sep=""), colnames(dfsub))][dfsub$NAME10==input$city1]
  })
  
  
  #### HISTOGRAM PANEL ####
  options = reactiveValues(choose2="b_13", city="", use="") 
  
  observeEvent(input$goLA,{
    options$choose2 <- paste("b_", grep(input$estabs2, est), sep="")
    options$finalcity = input$cityLA
    options$use = la
  })
  observeEvent(input$goOR,{
    options$choose2 <- paste("b_", grep(input$estabs2, est), sep="")
    options$finalcity = input$cityOR
    options$use = or
  })
  observeEvent(input$goRV,{
    options$choose2 <- paste("b_", grep(input$estabs2, est), sep="")
    options$finalcity = input$cityRV
    options$use = rv
  })
  observeEvent(input$goSB,{
    options$choose2 <- paste("b_", grep(input$estabs2, est), sep="")   
    options$finalcity = input$citySB
    options$use = sb
  })
  observeEvent(input$goVN,{
    options$choose2 <- paste("b_", grep(input$estabs2, est), sep="")   
    options$finalcity = input$cityVN
    options$use = vn
  })
  
    #print(options$choose2)
    #print(dest$descr[dest$name==input$estabs2])
    
    output$bar <- renderPlot({
      d <- options$use
      c2 = subset(cbind(d$name, d[,grep(options$choose2, colnames(d))]), d$name==options$finalcity)
      c3 = c2[,2:ncol(c2)]
      barplot(as.numeric(c3), names.arg=substr(colnames(c3),6,20), las=2,  
              main=paste(options$finalcity, ": ", input$estabs2), xlab="",
              ylab="Impact of Factors on Accessibility")
      abline(h=0)
      
    })
    output$var_desc <- renderText({
      print(options$finalcity)
      print(options$choose2)
      as.character(dest$descr[dest$name==input$estabs2])
    })



})  

