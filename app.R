# Import Libraries -----
library(shiny)
library(rhandsontable)
library(colorSpec)
library(shinythemes)

# Define some Test Spectra
default.testSpec <- Fs.5nm

# UI Code -----

ui <- navbarPage(

  # Title
  title = 'Academy Spectral Similarity Index (SSI) Calculator',
  windowTitle = 'SSI Calculator',

  # Theme
  theme = shinytheme("flatly"),

  # Tab Panel for Calculations
  tabPanel( title ="Calculations",

            # Enable ShinyJS to enable/disable elements
            shinyjs::useShinyjs(),

            # Modify look of horizontal rules to make them more prominent
            tags$head(
              tags$style(HTML('hr {border-top: 1px solid #b3b3b3;}'))
            ),

            # Sidebar ----
            sidebarLayout(

              # Add Dropdown for test spectra
              sidebarPanel(
                selectInput(inputId = 'testChoice',
                            label = 'Test Spectra',
                            choice = list( Fluorescent = c(specnames(default.testSpec)), 'Custom'),
                            selected = c(specnames(default.testSpec)[1])
                ),

                # If test spectra is custom show widgets to specify wl range and increments
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

                # Horizontal rule
                hr(),

                # Add dropdown for reference spectra
                selectInput(inputId = 'refChoice',
                            label = 'Reference Spectra',
                            choice = c('Default', 'Daylight', 'Blackbody'), # Add custom ref spec in future version , 'Custom'),
                            selected = c('Default')
                ),

                # If daylight show radio buttons to specify CCT or a canonical daylight.  Returns proper CCT.
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

                # If blackbody how radio buttons to specify CCT or or Illuminant A
                conditionalPanel("input.refChoice == 'Blackbody'",
                                 radioButtons("ref.cieP",
                                              label = '',
                                              choiceNames = c('CCT','A'),
                                              choiceValues = c('CCT', 2848),
                                              inline = TRUE)
                ),

                # If daylight add a box where CCT is shown or specified
                conditionalPanel("input.refChoice == 'Daylight'",
                                 numericInput("ref.cctD",
                                              label = "CCT",
                                              value = 5000,
                                              min = 4000,
                                              max = 25000)
                ),

                # If blackbody add a box where CCT is shown or specified
                conditionalPanel("input.refChoice == 'Blackbody'",
                                 numericInput("ref.cctP",
                                              label = "CCT",
                                              value = 3200,
                                              min = 1000,
                                              max = 10000)
                ),

                width = 3),

              # Main Panel -----
              mainPanel(
                column(
                  # Table with reference spectra values
                  h3('Data'),
                  hr(),
                  column(
                    rHandsontableOutput('spectra.ref'),
                    br(),
                    width = 6),
                  # Table with test spectra values
                  column(rHandsontableOutput('spectra.test'),
                         br(),
                         width = 6),
                  width = 4),
                # Plot
                column(
                  h3('Plot'),
                  hr(),
                  plotOutput('plot.ref'),
                  h3('Results'),
                  hr(),
                  # SSI Result
                  column(
                    h4("SSI"),
                    textOutput('ssi.text'),
                    width = 6
                  ),
                  # If default show details of spectra used
                  column(
                    conditionalPanel("input.refChoice == 'Default'",
                                     h4("Default Reference Spectra Used"),
                                     textOutput('cct.test')
                    ),
                    width = 6),
                  width = 8
                )
              )
            )
  ),
  # Tab Panel for Calculations
  tabPanel( title ="About",
            tags$html(
              tags$body(
                p('Written and maintained by the Academy of Motion Picture Arts and Sciences'),
                p('Source code can be found on',
                  a(href='https://www.github.com/ampas/ssi_calculator/', 'Github', target='_blank')
                  ),
                p('For more info go to',
                  a(href='http://www.oscars.org/ssi', 'http://www.oscars.org/ssi', target='_blank')
                  ),
                p('This calculator was built using',
                  a(href='https://www.r-project.org/', 'R', target='_blank'), ',',
                  a(href='https://shiny.rstudio.com/', 'Shiny', target='_blank'), 'and',
                  a(href='https://cran.r-project.org/web/packages/colorSpec/index.html', 'ColorSpec', target='_blank')
                  ),
                p('Last updated - 10/25/2018')
            )
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

  # Define reactive values object for spectra and set defaults
  spectra <-
    reactiveValues(
      # Equal energy default ... very small non-zero value to avoid errors and warnings elsewhere
      ref = illuminantE(energy = 1e-5, wavelength = 300:830) %>% `organization<-`('matrix'),
      test = illuminantE(energy = 1e-5, wavelength = 300:830) %>% `organization<-`('matrix')
    )

  # Create observer to generate reference spectra based on UI choices
  observe({
    # If reference is default
    if (input$refChoice == 'Default') {
      #Check to see if compute CCT throws a NA
      if (!is.na(computeCCT(spectra$test)[[1]])) {
        # Calculate test spectra cct
        cct <- computeCCT(spectra$test)
        # Compute default reference spectra based on test cct
        if (cct <= 4000) {
          s <- planckSpectra(cct, 300:830)
        } else {
          s <- daylightSpectra(cct, 300:830)
        }
      } else {
        warning('CCT of test spectra can not be calculated')
        s <- spectra$test
      }
    } else if (input$refChoice == 'Daylight') {
      # If reference is daylight
      validate(
        need(input$ref.cctD <= 25000, 'CCT must be 25000K or less'),
        need(input$ref.cctD >= 4000, 'CCT must be  at least 4000K')
      )
      # Compute daylight reference spectra
      s <- daylightSpectra(input$ref.cctD, 300:830)
    } else if (input$refChoice == 'Blackbody') {
      # If reference is blackbody
      validate(
        need(input$ref.cctP <= 10000, 'CCT must be 10000K or less'),
        need(input$ref.cctP >= 1000, 'CCT must be  at least 1000K'))
      # Compute blackbody reference spectra
      if (input$ref.cieP == 'CCT') {
        s <- planckSpectra(input$ref.cctP, 300:830)
      } else {
        s <- planckSpectra(input$ref.cctP, 300:830, c2 = 1.435e7)
      }
    }
    spectra$ref <- s
  })

  # Create observer and generate test spectra based on UI choices
  observe({
    if (input$testChoice != 'Custom') {
      # Subset if not custom
      s <- subset(default.testSpec, paste(input$testChoice, "$", sep=""))
    } else {
      validate(
        need(input$test.wlMin >= 300, 'Min Wavelength must be >= 300'),
        need(input$test.wlMax <= 830, 'Min Wavelength must be >= 830'),
        need(input$test.wlInc >= 1,'Min Wavelength must be >= 830')
      )
      s = illuminantE(energy = 0,
                      wavelength = seq(input$test.wlMin,
                                       input$test.wlMax,
                                       input$test.wlInc)
      ) %>% `organization<-`('matrix')
    }
    spectra$test <- s
  })
  interpolateAndNormalize <- function(spec) {
    # Interpolate to 300-830nm in 1nm intervals
    spec.resample <- resample(
      spec,
      wavelength = 300:830,
      method = 'linear',
      extrapolation = 0)

    # Normailze 560nm to 1.0
    spec.out <- normalize(spec.resample, 560)

    return(spec.out)
  }

  # Calculate the CCT of the test spectra and prepare text used for display in the UI
  output$cct.test <- renderText({
    cct <- computeCCT(spectra$test)
    if (cct <= 4000) {
      paste('Blackbody with CCT =', round(cct, digits = 2), 'K')
    } else if (cct > 4000 && cct < 25000) {
      paste('CIE Daylight with CCT =',  round(cct, digits = 2), 'K')
    } else {
      paste('CCT can not be calculated for the test spectra!')
    }
  })

  output$plot.ref <- renderPlot({
    # Interpolate and normalize data
    spectra.ref <- interpolateAndNormalize(spectra$ref)
    spectra.test <- interpolateAndNormalize(spectra$test)
    # Combine Test and Reference Spectra into one ColorSpec object
    specnames(spectra.ref) <- 'Reference'
    specnames(spectra.test) <- 'Test'
    s <-bind(spectra.ref, spectra.test)
    # Define yMax for the plot Y axis limit
    if (max(s) < 1) {
      yMax <- 1
    } else {
      yMax <- max(s)
    }
    # Plot
    plot(s,
         color=c('black','red'),
         main='Spectral Power Distributions',
         CCT = TRUE,
         ylim = c(0, yMax))
  })

  # SSI Result
  output$ssi.text <- renderText({
    '100'
  })

  # Generate Table of Test Sepctra
  output$spectra.test <- renderRHandsontable({
    if (input$testChoice == 'Custom') {
      ro <- FALSE
      spectra.test <- spectra$test
    } else {
      # Interpolate and normalize
      spectra.test <- interpolateAndNormalize(spectra$test)
      # Set read only flag to TRUE unless custom
      ro <- TRUE
    }
    # Generate Table
    rhandsontable(coredata(spectra.test),
                  colHeaders='Test',
                  digits = 10,
                  width = 500,
                  readOnly = ro) %>%
      hot_col(1, format = '0.00000')
  })

  # Generate Table of Reference Sepctra
  output$spectra.ref <- renderRHandsontable({
    rhandsontable(coredata(spectra$ref),
                  colHeaders='Reference',
                  digits = 10,
                  width = 500,
                  readOnly = TRUE) %>%
      hot_col(1, format = '0.00000')
  })



}

# Run Application ----
shinyApp(ui = ui, server = server)
