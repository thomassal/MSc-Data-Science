library("readxl")
library("dplyr")
library("shiny")
require(ggplot2)


# Define a server for the Shiny app
server <- function(input, output, session) {
data_path <- "C:/Users/thomas/ownCloud/data science/Data Visualization & Communication/assignment 1/COVID-19-geographic-disbtribution-worldwide.xlsx"
data <- read_excel(data_path)

countries = sort(unique(data$countriesAndTerritories))  
updateSelectInput(session, "country", choices=countries, selected="Greece")

# Fill in the spot we created for a plot
output$barplot <- renderPlot({

coutry_data = data %>% filter(countriesAndTerritories == input$country)
cases_by_month = coutry_data %>% group_by(month) %>% summarise(cases = sum(cases))
cases_by_month = cases_by_month[c(12,1,2,3,4,5,6,7,8,9,10,11),]
cases_by_month["mon"] <- c('12/2019', '01/2020' , '02/2020', '03/2020', '04/2020', '05/2020', '06/2020', '07/2020', '08/2020', '09/2020', '10/2020', '11/2020')
cases_by_month$mon <- factor(cases_by_month$mon, levels = cases_by_month$mon)
t = paste("Cases by month in", input$country, sep=" ")

ggplot(data=cases_by_month,
 aes(x=mon, y=cases)) +
 geom_bar(stat="identity", fill="steelblue") +
 geom_text(aes(label=cases), vjust=-0.3, size=3.5)+
 labs(title=t, x="Month", y = "Cases")+
 theme_minimal() 

 })

}

# Use a fluid Bootstrap layout
ui <- fluidPage(
  # Give the page a title
  titlePanel("Case History of the Coronavirus (COVID-19)"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      h5(""),
      selectInput("country","Country",choices=NULL),
      hr(),
    ),


    # Create a spot for the barplot
    mainPanel(
      plotOutput("barplot")  
    )
    
  )
)

shinyApp(ui, server)
