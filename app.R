library(shiny)
library(zoo)
library(plotly)
library(httr)
library(readr)

# read in data
# The code below works locally but not on shinyapps.io
#df = read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

# This works both locally adn on shinyapps.io
response <- httr::GET("https://covid19.who.int/WHO-COVID-19-global-data.csv")
df <- content(response)

#clean up the date column 
names(df)[names(df) == "ï..Date_reported"] = "Date_reported" #rename (not sure why the name comes through like this!)
df$Date_reported = as.Date(df$Date_reported) #format as date

availableCountries <- unique(df$Country)


#for the input
columns <- c(
	"Daily Cases" = "New_cases_rolling",
	"Daily Deaths" = "New_deaths_rolling",
	"Cumulative Cases" = "Cumulative_cases",
	"Cumulative Deaths" = "Cumulative_deaths"
)

# Define UI 
ui <- fluidPage(

	# App title 
	headerPanel("COVID-19 Data"),

	# Sidebar panel for inputs 
	sidebarPanel(

		selectInput(
			"country", "Country:", 
			availableCountries,
			selected = "United States of America"
		),

		selectInput(
			"column", "Data format:", 
			columns
		),

		sliderInput("rollingWindow", "Days for rolling mean:", 1, 30, 7, step = 1)
	),

	# Main panel for displaying outputs 
	mainPanel(

		# Output: Formatted text for debugging (or a caption) 
		#p(textOutput("debug")),

		# Output: Plot of the requested variable against mpg 
		plotlyOutput("COVIDplot")
	)
)




# Define server logic 
server <- function(input, output) {
	# for debuggin 
	debugging <- reactive({
		paste(input$country, input$column, input$rollingWindow)
	})

	output$debug <- renderText({
		debugging()
	})

	# Generate the plot 
	output$COVIDplot <- renderPlotly(
		generate_plot(input$country, input$column, input$rollingWindow)
	)
 
}


generate_plot <- function(country, column, rollingWindow){

	# select the correct country
	usedf = df[df$Country == country,] #the comma is necessary!

	# add the rolling means
	usedf = transform(usedf, New_cases_rolling = rollmeanr(New_cases, rollingWindow, fill = 0))
	usedf = transform(usedf, New_deaths_rolling = rollmeanr(New_deaths, rollingWindow, fill = 0))

	# create the plot
	p = ggplot(usedf,
			aes(
				x=Date_reported, 
				y=get(column), 
				group=1,
				text=sprintf("Date: %s\n Count: %.0f", Date_reported,get(column))
			),
		) +
		geom_line() + geom_area(alpha=0.5) + 
		xlab("Date") + ylab(names(columns)[columns == column]) +
		scale_x_date(limits = c(min(usedf$Date_reported), max(usedf$Date_reported)), expand=c(0,0)) + 
		scale_y_continuous(limits = c(0, 1.05*max(usedf[column])), expand=c(0,0)) + 
		theme_bw()
	ggplotly(p, tooltip = "text")

}

shinyApp(ui, server)