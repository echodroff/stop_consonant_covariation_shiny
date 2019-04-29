library(shiny)
library(ggplot2)

means <- read.csv("ChodroffWilsonLingVan2018_talkerMeans.csv")

# Define UI----
ui <- fluidPage(
	# App title ----
	titlePanel("Covariation of stop consonant cues"),
	p(textOutput("article")),
	p(textOutput("description")),
	# Sidebar layout with input and output definitions ----
	sidebarLayout(
	sidebarPanel(
	
		# Input: Selector for variable 1
		selectInput("variable1", "Variable 1:",
		c("/p/ VOT" = "p.vot_ms", "/t/ VOT" = "t.vot_ms", "/k/ VOT" = "k.vot_ms",
		"/b/ VOT" = "b.vot_ms", "/d/ VOT" = "d.vot_ms", "/g/ VOT" = "g.vot_ms",
		"/p/ COG" = "p.cog_hz", "/t/ COG" = "t.cog_hz", "/k/ COG" = "k.cog_hz",
		"/b/ COG" = "b.cog_hz", "/d/ COG" = "d.cog_hz", "/g/ COG" = "g.cog_hz")),
		
		# Input: Selector for variable 2
		selectInput("variable2", "Variable 2:",
		c("/p/ VOT" = "p.vot_ms", "/t/ VOT" = "t.vot_ms", "/k/ VOT" = "k.vot_ms",
		"/b/ VOT" = "b.vot_ms", "/d/ VOT" = "d.vot_ms", "/g/ VOT" = "g.vot_ms",
		"/p/ COG" = "p.cog_hz", "/t/ COG" = "t.cog_hz", "/k/ COG" = "k.cog_hz",
		"/b/ COG" = "b.cog_hz", "/d/ COG" = "d.cog_hz", "/g/ COG" = "g.cog_hz")),
		
		# Output: Correlation of the requested variables ----
		h4(textOutput("corText")),
		h4(textOutput("correlation")),
		h4(textOutput("pvalText")),
		h4(textOutput("pval"))
	),
	
	# Main panel for displaying outputs ----
	mainPanel(
	
		# Output: Formatted text for caption ----
		h4(textOutput("caption")),
		
		# Output: Plot of the requested variables ----
		plotOutput("stopPlot")
  )
)
)
# Define server logic to plot various variables ----
server <- function(input, output) {

  # Return the text for printing as a caption ----
  output$caption <- renderText({
	paste("x = ", input$variable1, ", y = ", input$variable2, sep="")
  })
  
  # Compute the formula text ----
  # This is in a reactive expression
  formulaText <- reactive({
    paste("~", input$variable1, "+", input$variable2)
  })
  
  # Get the correlation ----
  output$correlation <- renderText({
  round(cor.test(~means[,colnames(means)==input$variable1] + means[,colnames(means)==input$variable2])$estimate, 3)
  })
  
    # Get the p-value ----
  output$pval <- renderText({
  cor.test(~means[,colnames(means)==input$variable1] + means[,colnames(means)==input$variable2])$p.value
  })
  
  # Generate a plot of the requested variables ----
#   output$stopPlot <- renderPlot({
#     plot(as.formula(formulaText()), data = means)
#     })
  
  # Generate a plot of the requested variables ----
  output$stopPlot <- renderPlot({
  	ggplot(means) + geom_point(aes(x = means[,colnames(means)==input$variable1], y = means[,colnames(means)==input$variable2], color=gender)) + xlab(input$variable1) + ylab(input$variable2) + geom_smooth(aes(means[,colnames(means)==input$variable1],means[,colnames(means)==input$variable2]), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20)
  })
  
  # static text
  output$article <- renderText({"This dataset was released with the following Linguistics Vanguard publication: Chodroff, E. & Wilson, C. (2018). Predictability of stop consonant phonetics across talkers: Between-category and within-category dependencies among cues for place and voice. Linguistics Vanguard 4(s2).\n\n\
  The correlations of VOT were published in Chodroff, E. & Wilson, C. (2017). Structure in talker-specific phonetic realization: Covariation of stop consonant VOT in American English. Journal of Phonetics, 61, 30-47."})
  output$description <- renderText({"Talker mean voice onset time (VOT, ms) and center of gravity (COG, Hz) from 180 native speakers of American English in the Mixer 6 read speech corpus. All measurements were derived from word-initial, prevocalic stop consonants."})
  output$corText <- renderText({ "Pearson correlation: "})
  output$pvalText <- renderText({ "p-value: "})	
	
}

# Create Shiny app ----
shinyApp(ui, server)