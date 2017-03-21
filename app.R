library(shinydashboard)
nozzle <- read.csv("data.csv")

sidebar<-sidebarMenu(
  menuItem("Ground speed (mph)", tabName="speedmph", icon=icon("dashboard")),
  menuItem("Gallons per Acre", tabName="gallonsPerAcre", icon=icon("tint")),
  menuItem("Flow rate per Nozzle", tabName="flowRatePerNozzle", icon=icon("flask"))  
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "speedmph",
            h2("Ground speed in mph"),
            fluidRow(
            box(
              title = "Distance traveled (ft.)", status = "primary", solidHeader = TRUE,
              collapsible = FALSE, width = 10,
              (sliderInput(inputId = "distance", 
                           label = "", 200, min=100, max=300, step=10))),
            box(
              title = "Time (seconds) taken to travel", status = "primary", solidHeader = TRUE,
              collapsible = FALSE, width = 10,
              (sliderInput(inputId = "time1", 
                           label = "First pass", 34, min=10, max=60, step=1)),
              (sliderInput(inputId = "time2", 
                           label = "Second pass", 34, min=10, max=60, step=1)),
              (sliderInput(inputId = "time3", 
                           label = "Third pass", 34, min=10, max=60, step=1))
              ),
      infoBoxOutput("speedBox"))),
        tabItem(tabName = "gallonsPerAcre",
            h2("Gallons per acre"),
            
   box(checkboxInput("twoNozzles", "Different nozzles?"), status = "primary", solidHeader = TRUE),
   box(title="Total nozzles per bed",
     sliderInput(inputId = "n.per.bed", label = "", 10, min=6, max=20), status = "primary", 
     solidHeader = TRUE),
              box(title = "Nozzle", selectInput(inputId = "nozzle", 
                              label = "", 
                              choices = levels(nozzle$nozzle)),
                  conditionalPanel(
                    condition = "input.twoNozzles == true",
                    uiOutput("n1.per.bed")), status = "primary", solidHeader = TRUE),
              
              conditionalPanel(
                condition = "input.twoNozzles == true",box(title="Nozzle 2",
                selectInput("nozzle2", "",
                            choices = levels(nozzle$nozzle)),
                uiOutput("n2.per.bed")), status = "primary", solidHeader = TRUE),
              box(title= "Pressure (psi)",
                  sliderInput(inputId = "pressure", 
                              label = "", 150, min=40, max=400, step=10), status = "primary", solidHeader = TRUE),
              uiOutput("speed"),
              box (title="Bed width (inches)", sliderInput(inputId = "bed.width", 
                               label = "", 48, min=36, max=72, step=4), status = "primary", solidHeader = TRUE),
              infoBoxOutput("gpaBox"),
              infoBoxOutput("pressureBox"), 
              conditionalPanel(
                    condition = "input.twoNozzles == true",
                    infoBoxOutput("pressureBox2"))),
   
   tabItem(tabName = "flowRatePerNozzle",
           h2("Flow rate per nozzle"),
           infoBoxOutput("flowRateBox"),
           conditionalPanel(
             condition = "input.twoNozzles == true",
             infoBoxOutput("flowRateBox2")),
           box(title="", width=10, background="light-blue",
           p(strong("Replace nozzle if flow rate is outside the 10% range")))
  )
))


ui <- dashboardPage(
  dashboardHeader(title = "Sprayer calibration"),
  dashboardSidebar(sidebar),
  dashboardBody(body))

server <- function(input, output) {
    output$n1.per.bed<-renderUI({
    sliderInput("n1", "Nozzle per bed", input$n.per.bed-1, min=1, max=input$n.per.bed, step=1)
  })
  output$n2.per.bed<-renderUI({
    sliderInput("n2", "Nozzle 2 per bed", input$n.per.bed-input$n1, min=1, max=input$n.per.bed, step=1)
  })
  
  data <- reactive({
    nozzle[which(nozzle$nozzle == input$nozzle),]
  })
  data2 <- reactive({
    nozzle[which(nozzle$nozzle == input$nozzle2),]
  })
  
  
  output$speed<-renderUI({box(title= "Speed (mph)",
    sliderInput("speed", "", min = 0.1,
                  max = 6.0, value = input$distance/((input$time1+input$time2+input$time3)/3)*.682), 
    status = "primary", solidHeader = TRUE)})

    output$pressureBox<- renderInfoBox({
    a<-data()
    max.psi<-a[1,4]
    infoBox(
      input$nozzle, paste0(ifelse(input$pressure>max.psi, 
                                "Pressure exceeds manufacturer recommendation",
                                "Pressure is within recommended range")),
      icon = icon(ifelse(input$pressure>max.psi, "thumbs-o-down", "thumbs-o-up")),
      color = ifelse(input$pressure>max.psi, "red", "green"), width = 10)})
  
  
  output$pressureBox2<- renderInfoBox({
    a2<-data2()
    max2.psi<-a2[1,4]
    infoBox(
      input$nozzle2, paste0(ifelse(input$pressure>max2.psi, 
                                "Pressure exceeds manufacturer recommendation",
                                "Pressure is within recommended range")),
      icon = icon(ifelse(input$pressure>max2.psi, "thumbs-o-down", "thumbs-o-up")),
      color = ifelse(input$pressure>max2.psi, "red", "green"), width = 10)})
  
  output$speedBox<- renderInfoBox({
    infoBox(
      "GROUND SPEED", paste0(
        round(input$distance/((input$time1+input$time2+input$time3)/3)*.682, digits=2), "mph"),
      icon = icon("dashboard", lib = "glyphicon"),
      color = "green", width = 10)})

  output$gpaBox <- renderInfoBox({
    a<-data()
    gpm.40<-a[1,2]
    gpa<-(5940*sqrt(input$pressure/40)*gpm.40)/(input$speed*(input$bed.width/input$n.per.bed))
    
    a2<-data2()
    gpm2.40<-a2[1,2]
    gpa2<-(5940*sqrt(input$pressure/40)*gpm2.40)/(input$speed*(input$bed.width/input$n.per.bed))
    infoBox("GPA", paste0(ifelse(input$twoNozzles==FALSE, round(gpa, digits=2),
                          round(gpa*input$n1/input$n.per.bed+gpa2*input$n2/input$n.per.bed, digits=2))),
            icon = icon("tint"),
            color = "light-blue", width = 10)
            })
  
  output$flowRateBox <- renderInfoBox({
    a<-data()
    gpm.40<-a[1,2]
    b <- round(gpm.40/(sqrt(40/input$pressure))*128, digits=0)
    infoBox(title="nozzle output (oz/min)", subtitle=input$nozzle, paste0("", b,
                          "\n(10% range: ", round(b*.9, digits=0),"-", round(b*1.1,digits=0),")"),
      icon = icon("flask"),
      color = "green", width = 10)})
  
  output$flowRateBox2 <- renderInfoBox({
    a2<-data()
    gpm2.40<-a2[1,2]
    b2 <- round(gpm.40/(sqrt(40/input$pressure))*128, digits=0)
    infoBox(title="nozzle output (oz/min)", subtitle=input$nozzle2, paste0("", b2, 
                          "\n(10% range: ", round(b2*.9, digits=0),"-", round(b2*1.1,digits=0),")"),
      icon = icon("flask"),
      color = "green", width = 10)})


}

shinyApp(ui, server)
