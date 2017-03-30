library(shiny)
library(leaflet)
library(maptools)
library(sp)

la <- read.csv("LA_acc_coeff_sig.csv", stringsAsFactors = F)
or <- read.csv("OC_acc_coeff_sig.csv", stringsAsFactors = F)
rv <- read.csv("RV_acc_coeff_sig.csv", stringsAsFactors = F)
sb <- read.csv("SB_acc_coeff_sig.csv", stringsAsFactors = F)
vn <- read.csv("VN_acc_coeff_sig.csv", stringsAsFactors = F)
dest <- read.csv("cat_descr.csv")
sub <- readShapePoly("SoCal_place_2010_UA")
dfsub <- data.frame(sub)
est = c('Apparel Retailing', 'Auto Services', 'Beer, Wine, and Liquor Stores', 'Child Care Services', 'Convenience Stores', 'Deposit-taking Institutions', 'Drinking Places', 'Drug Stores', 'Elementary and Secondary Schools', 'Full-Service Restaurants', 'Gas Stations', 'General Merchandise Retailing', 'Groceries', 'Hair Care Services', ' ', 'Home Products Retailing', 'Hospitals', 'Laundry', 'Limited-Service Food and Beverage', 'Medical Laboratories', 'Open Space', 'Other Learning', 'Other Personal Services', 'Personal Financial', 'Personal Products Retailing', 'Recreational Facilities and Instruction', 'Religious Organizations', 'Repair Services', 'Social Service Organizations', 'Specialty Food', 'Specialty Retailing')

shinyServer(function(input, output) {

  #### MAP PANEL ####
  center <- reactiveValues(x_coord=-117.7736, y_coord=33.67801)
  observeEvent(input$recenter, {
    center$x_coord = dfsub$x_coord[dfsub$NAME10==input$cent]
    center$y_coord = dfsub$y_coord[dfsub$NAME10==input$cent]
  })
  
  options = reactiveValues(choose = "^Avg_1$")
  observeEvent(input$go1, {   
    link1 = switch(input$topic, "Total # of Destinations in city"="et", "Abundance"="Avg_", "Percent serviced"="Nonzero_")
    options$choose <- paste("^", link1, grep(input$estabs1, est), "$", sep="")
  })
  
 finalMap <- reactive ({
    selected = max("^Avg_1$", options$choose)
    choice = dfsub[,grep(selected, colnames(dfsub))]
    if(input$topic=="Total # of Destinations in city"){
      br1 = as.numeric(quantile(choice[choice>0], 0.25, na.rm=T))
      br2 = as.numeric(quantile(choice[choice>0], 0.5, na.rm=T))
      br3 = as.numeric(quantile(choice[choice>0], 0.75, na.rm=T))
      pal <- colorBin("Blues", choice, bins=c(0, 0.99, br1, br2, br3, as.numeric(summary(choice)[6])), na.color="#B0171F")}
    else if(input$topic=="Abundance"){
      br1 = as.numeric(quantile(choice[choice>0], 0.25, na.rm=T))
      br2 = as.numeric(quantile(choice[choice>0], 0.5, na.rm=T))
      br3 = as.numeric(quantile(choice[choice>0], 0.75, na.rm=T))
      pal <- colorBin("Blues", choice, bins=c(0, 0.01, br1, br2, br3, as.numeric(summary(choice)[6])), na.color="#B0171F")}
    else if(input$topic=="Percent serviced"){
      br1 = as.numeric(quantile(choice, 0.25, na.rm=T))
      br2 = as.numeric(quantile(choice, 0.5, na.rm=T))
      br3 = as.numeric(quantile(choice, 0.75, na.rm=T))
      pal <- colorBin("Blues", choice, bins=c(0, br1, br2, br3, 100), na.color="#B0171F")}
    m = leaflet(sub) %>% setView(lng=center$x_coord, lat=center$y_coord , zoom=9) %>% addTiles() %>%
      addPolygons(data=sub, stroke=T, weight=1.1, fillColor=~pal(choice), color="black", fillOpacity=0.5,
                  opacity=1, popup=~paste(NAME10)) %>%
      addLegend("bottomleft", pal=pal, values=~choice, opacity=0.75, na.label="Data unavailable",
                title=paste(input$estabs1, input$topic))
  })
  output$map = renderLeaflet(finalMap())

  output$hist <- renderPlot({
        par(mar=c(2.5,4,4,2), oma=c(1.5,0,0,0))
        selected = max("^Avg_1$", options$choose)  # must 'trick' function into a default selection
        datause = dfsub[,grep(selected, colnames(dfsub))]
        datause = datause[!is.na(datause)]
        lab <- input$estabs1
        p1 = as.numeric(quantile(datause, 0.01))
        p99 = as.numeric(quantile(datause, 0.99))
        hist(datause, xlab=NULL, col="dodgerblue", breaks=((max(datause)-min(datause))/(p99-p1))*12, xlim=c(p1, p99),
             ylab="# of SoCal Cities", border="white", main=lab, font=2)
        abline(v=mean(datause), lty=2)
        val = round(dfsub[,grep(paste("^", selected, "$", sep=""), colnames(dfsub))][dfsub$NAME10==input$city1],2)
        abline(v=val, lwd=2)
        legend("topright", c(input$city1), lwd=2, box.col="white")
        legend("topright", c(input$city1, "Avg"), lwd=c(2,1), lty=c(1,2), box.col="white")
        mtext(paste(input$topic, " (", input$city1, " = ", val, ")"), side=1, cex=0.85, font=3, outer=TRUE)
    })
    
  
  #### HISTOGRAM PANEL ####
  options = reactiveValues(choose2="b_13", finalcity="Aliso Viejo", use="or") 
  
  observeEvent(input$goLA,{
    if(grep(input$estabs2, est)==1 | grep(input$estabs2, est)==2 | grep(input$estabs2, est)==3){
      options$choose <- paste("b_0", grep(input$estabs2, est), sep="")}   # prevents multiple estab types from being selected for 1, 2, and 3
      else {options$choose2 <- paste("b_", grep(input$estabs2, est), sep="") }  
    options$finalcity = input$cityLA
    options$use = la
  })
  observeEvent(input$goOR,{
    if(grep(input$estabs2, est)==1 | grep(input$estabs2, est)==2 | grep(input$estabs2, est)==3){
      options$choose <- paste("b_0", grep(input$estabs2, est), sep="")}   # prevents multiple estab types from being selected for 1, 2, and 3
    else {options$choose2 <- paste("b_", grep(input$estabs2, est), sep="") }  
    options$finalcity = input$cityOR
    options$use = or
  })
  observeEvent(input$goRV,{
    if(grep(input$estabs2, est)==1 | grep(input$estabs2, est)==2 | grep(input$estabs2, est)==3){
      options$choose <- paste("b_0", grep(input$estabs2, est), sep="")}   # prevents multiple estab types from being selected for 1, 2, and 3
    else {options$choose2 <- paste("b_", grep(input$estabs2, est), sep="") }  
    options$finalcity = input$cityRV
    options$use = rv
  })
  observeEvent(input$goSB,{
    if(grep(input$estabs2, est)==1 | grep(input$estabs2, est)==2 | grep(input$estabs2, est)==3){
      options$choose <- paste("b_0", grep(input$estabs2, est), sep="")}   # prevents multiple estab types from being selected for 1, 2, and 3
    else {options$choose2 <- paste("b_", grep(input$estabs2, est), sep="") }  
    options$finalcity = input$citySB
    options$use = sb
  })
  observeEvent(input$goVN,{
    if(grep(input$estabs2, est)==1 | grep(input$estabs2, est)==2 | grep(input$estabs2, est)==3){
      options$choose <- paste("b_0", grep(input$estabs2, est), sep="")}   # prevents multiple estab types from being selected for 1, 2, and 3
    else {options$choose2 <- paste("b_", grep(input$estabs2, est), sep="") }  
    options$finalcity = input$cityVN
    options$use = vn
  })
  
    output$bar <- renderPlot({
      d <- options$use
      c2 = subset(cbind(d$name, d[,grep(options$choose2, colnames(d))]), d$name==options$finalcity)
      c3 = c2[,2:ncol(c2)]
      par(oma=c(3,0,3,0))
      barplot(as.numeric(c3), names.arg=substr(colnames(c3),6,20), las=2, col="gold",
              main=paste(options$finalcity, ": ", input$estabs2), xlab="", ylab="Impact of Characteristics on Accessibility")
      abline(h=0)
      topcatpos <- substr(colnames(c3)[grep(max(c3[c3>0], na.rm=T), c3)],6,20)
      topcatneg <- substr(colnames(c3)[grep(max(abs(c3[c3<0]), na.rm=T), c3)],6,20)
      mtext(paste("In", options$finalcity, topcatpos, "has the strongest positive impact on access to", input$estabs2, "(meaning that locations \n with high values of", topcatpos, "have more nearby", input$estabs2, ").",
                  topcatneg, "has the strongest negative impact, \n meaning that locations with high values of", topcatneg, "have fewer", input$estabs2, "nearby."), side=3, cex=0.95, font=3, outer=TRUE)
    })
    output$var_desc <- renderText({
      as.character(dest$descr[dest$name==input$estabs2])
    })
})  

