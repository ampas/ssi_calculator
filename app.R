# Import Libraries -----
library(shiny)
library(rhandsontable)
library(colorSpec)

default.testSpec = Fs.5nm

# UI Code -----
ui <- fluidPage(

  # Use ShinyJS
  shinyjs::useShinyjs(),

  # Application title
  titlePanel("Academy Spectral Similarity Index (SSI) Calculator"),
  br(),

  tags$head(
    tags$style(HTML('hr {border-top: 1px solid #b3b3b3;}'))
  ),

  # Sidebar
  sidebarLayout(

    sidebarPanel(
      selectInput(inputId = 'testChoice',
                  label = 'Test Spectra',
                  choice = list( Fluorescent = c(specnames(default.testSpec)), 'Custom'),
                  selected = c(specnames(default.testSpec)[1])
      ),

      conditionalPanel("input.testChoice == 'Custom'",
                       # Inputs for CCT and Wavelength
                       numericInput("test.wlMin",
                                    label = "Minimum Wavelength",
                                    value = 300,
                                    min = 300,
                                    max = 400),
                       numericInput("test.wlMax",
                                    label = "Maximum Wavelength",
                                    value = 830,
                                    min = 700,
                                    max = 830),
                       numericInput("test.wlInc",
                                    label = "Wavelength Increments",
                                    value = 5,
                                    min = 1,
                                    max = 10)
      ),

      hr(),

      selectInput(inputId = 'refChoice',
                  label = 'Reference Spectra',
                  choice = c('Default', 'Daylight', 'Blackbody', 'Custom'),
                  selected = c('Default')
      ),

      conditionalPanel("input.refChoice == 'Daylight'",
                       radioButtons("ref.cieD",
                                    label = '',
                                    choiceNames = c('CCT','D50','D55','D65','D75'),
                                    choiceValues = c('CCT',
                                                     5000 * 14388 / 14380,
                                                     5500 * 14388 / 14380,
                                                     6500 * 14388 / 14380,
                                                     7500 * 14388 / 14380),
                                    inline = TRUE)
      ),

      conditionalPanel("input.refChoice == 'Blackbody'",
                       radioButtons("ref.cieP",
                                    label = '',
                                    choiceNames = c('CCT','A'),
                                    choiceValues = c('CCT', 2848),
                                    inline = TRUE)
      ),

      conditionalPanel("input.refChoice == 'Daylight'",
                       numericInput("ref.cctD",
                                    label = "CCT",
                                    value = 5000,
                                    min = 4000,
                                    max = 10000)
      ),

      conditionalPanel("input.refChoice == 'Blackbody'",
                       numericInput("ref.cctP",
                                    label = "CCT",
                                    value = 3200)
      ),

      conditionalPanel("input.refChoice == 'Custom'",
                       numericInput("ref.wlMin",
                                    label = "Minimum Wavelength",
                                    value = 300,
                                    min = 300,
                                    max = 400),
                       numericInput("ref.wlMax",
                                    label = "Maximum Wavelength",
                                    value = 830,
                                    min = 700,
                                    max = 830),
                       numericInput("ref.wlInc",
                                    label = "Wavelength Increments",
                                    value = 2,
                                    min = 1,
                                    max = 10)
      ), width = 3),

    mainPanel(
      column(rHandsontableOutput('spectra.ref'), br(), width = 2),
      column(rHandsontableOutput('spectra.test'), br(), width = 2),
      column(plotOutput('plot.ref'), width = 8,
             textOutput('text'))
    )
  )
)


# Server Code ----
server <- function(input, output, session) {

  # Stop app if browser window closes
  session$onSessionEnded(stopApp)

  # Gray out Daylight CCT numericInput if a canonical illuminant
  observeEvent(input$ref.cieD,
               if (input$ref.cieD == "CCT"){
                 shinyjs::enable('ref.cctD')
               } else {
                 shinyjs::disable('ref.cctD')
               })

  # Gray out Blackbody CCT numericInput if a canonical illuminant
  observeEvent(input$ref.cieP,
               if (input$ref.cieP == "CCT"){
                 shinyjs::enable('ref.cctP')
               } else {
                 shinyjs::disable('ref.cctP')
               })

  # Update Daylight CCT numbericInput if a canonical illuminant or 5000K if 'CCT'
  observeEvent(input$ref.cieD,
               if (input$ref.cieD != "CCT"){
                 updateNumericInput(session, inputId = 'ref.cctD', value = input$ref.cieD)
               } else {
                 updateNumericInput(session, inputId = 'ref.cctD', value = 5000)
               })

  # Update Blackbody CCT numbericInput if a canonical illuminant or 3200K if 'CCT'
  observeEvent(input$ref.cieP,
               if (input$ref.cieP != "CCT"){
                 updateNumericInput(session, inputId = 'ref.cctP', value = input$ref.cieP)
               } else {
                 updateNumericInput(session, inputId = 'ref.cctP', value = 3200)
               })

  observe({
    if (input$refChoice == 'Default') {
                 spectra <- illuminantE(0, wavelength = 300:830)
               } else if (input$refChoice == 'Daylight') {
                 spectra <- daylightSpectra(input$ref.cctD, 300:830)
               } else if (input$refChoice == 'Blackbody') {
                 spectra <- planckSpectra(input$ref.cctP, 300:830)
               } else if (input$refChoice == 'Custom') {
                 spectra <- illuminantE(0, wavelength = seq(input$ref.wlMin,
                                                            input$ref.wlMax,
                                                            input$ref.wlInc))
               }
    })




}

# Run Application ----
shinyApp(ui = ui, server = server)
