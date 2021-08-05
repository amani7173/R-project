# The packages:
library(shiny)
library(tidyverse)
#install.packages("shinydashboard")
library(shinydashboard)

df <- read_csv("/Users/almar/Downloads/Cleaned_Transactions.csv")
# Change type to Date
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")



ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader( title = " Personal Finance " ,titleWidth = 900),
  dashboardSidebar(disable = TRUE),
  dashboardBody(tags$style(HTML("


.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#2e2e1f
}
                    .box.box-solid.box-primary{
border-bottom-color:#2e2e1f;
border-left-color:#2e2e1f;
border-right-color:#2e2e1f;
border-top-color:#2e2e1f;
}"
  )),
  fluidRow(
    
    box(width=12,
        # First plot 
        title = " The personal finance by Date", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        # titlePanel("Personal Finance"),
        dateRangeInput(
          inputId = "datePicker", 
          label = "choose a date range", 
          startview = "month",
          min= min(df$Date), 
          max= max(df$Date),
          start = min(df$Date),
          end = max(df$Date)
        ),
        selectInput(inputId = "Type", label = "Choose the Type:",
                    selected = "All Type",
                    choices = c("ALL TYPE"="ALL TYPE",
                                "CREDIT"="CREDIT" ,
                                "DEBIT"="DEBIT"
                    )),
        plotOutput("Credit_VS_Debit"),
        #plotOutput("Categories")
    )),
  fluidRow(
    box(width=12,
        # Second plot 
        title = "Categories", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        selectInput(inputId="Category",
                    label="Select The Category",
                    selected = "All Category ",
                    choices = c("All Category" = "All Category" ,
                                "Entertainment" = "Entertainment",
                                "Cash" = "Cash",
                                "Food" = "Food",
                                "Bills" = "Bills",
                                "Electronics" = "Electronics",
                                "Coffee" = "Coffee",
                                "Money transfer" = "Money transfer",
                                "Essentials" = "Essentials",
                                "Clothes" = "Clothes",
                                "Subscription" = "Subscription"
                    )),
        
        
        plotOutput("Categories")
    ))
  
  )) 


server <- function(input, output) {
  output$Credit_VS_Debit <- renderPlot({
    dates <- strsplit(as.character(input$datePicker), " ")
    
    if (input$Type == "ALL TYPE"){
      cat(paste(input$Type, "\n"))
      df[df$Date >= as.Date(dates[[1]][1]) & df$Date <= as.Date(dates[[2]][1]),] %>%
        group_by(Date, Type) %>%
        summarise(Total = sum(Amount)) %>%
        ggplot(aes(x=Date, y=Total, fill=Type))+
        geom_bar(stat="identity", position = 'dodge')+
        scale_fill_manual(values=c('#b8b894','#6b6b47')) # Colors
      
      
    }  else {
      
      df[df$Date >= as.Date(dates[[1]][1]) & df$Date <= as.Date(dates[[2]][1]) & df$Type == input$Type,] %>%
        group_by(Date, Type) %>%
        summarise(Total = sum(Amount)) %>%
        ggplot(aes(x=Date, y=Total, fill=Type))+
        geom_bar(stat="identity", position = 'dodge')+
        scale_fill_manual(values=c('#b8b894','#6b6b47')) 
    }
  })
  
  output$Categories <- renderPlot({
    dates <- strsplit(as.character(input$datePicker), " ")
    if (input$Category == "All Category"){
      df[df$Date >= as.Date(dates[[1]][1]) & df$Date <= as.Date(dates[[2]][1]),] %>%
        group_by(Category) %>%
        summarise(Total = sum(Amount)) %>%
        ggplot(aes(x=Category, y=Total, fill=Category))+
        geom_bar(stat="identity", position = 'dodge')+
        theme(legend.position="none")+
        scale_fill_manual(values=c('#868679','#6b6b47','#3d3d29','#ebebe0','#ccccb3','#0f0f0a','#7a7a52','#666633','#b8b894','#e0e0d1')) 
      
    }
    else {
      df[df$Date >= as.Date(dates[[1]][1]) & df$Date <= as.Date(dates[[2]][1]) & df$Category == input$Category,] %>%
        group_by(Category) %>%
        summarise(Total = sum(Amount)) %>%
        ggplot(aes(x=Category, y=Total, fill=Category))+
        geom_bar(stat="identity", position = 'dodge')+
        theme(legend.position="none")+
        scale_fill_manual(values=c('#868679','#6b6b47','#3d3d29','#ebebe0','#ccccb3','#0f0f0a','#7a7a52','#666633','#b8b894','#e0e0d1')) 
      
    }
    
    
  })
  
}
shinyApp(ui, server)
