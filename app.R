# Import Libraries ----
library(shiny)
library(rhandsontable)
library(colorSpec)
library(shinythemes)
library(shinyjs)
library(glue)

# Load Test Spectra ----
default.testSpec <- Fs.5nm

# Define constants ----
CORRECTION_FACTOR     <- 14388 / 14380
MIN_WAVELENGTH        <- 300
MAX_WAVELENGTH        <- 830
DEFAULT_DAYLIGHT_CCT  <- 5000
DEFAULT_BLACKBODY_CCT <- 3200
MIN_DAYLIGHT_CCT      <- 4000
MAX_DAYLIGHT_CCT      <- 25000
MIN_BLACKBODY_CCT     <- 1000
MAX_BLACKBODY_CCT     <- 10000

## Subfunctions ----
interpolateAndNormalize <- function(spec) {
  
  # Interpolate to MIN_WAVELENGTH-MAX_WAVELENGTH nm in 1nm intervals
  spec.resample <- resample(
    spec,
    wavelength = MIN_WAVELENGTH:MAX_WAVELENGTH,
    method = 'linear',
    extrapolation = 0)
  
  # Normailze 560nm to 1.0
  spec.out <- normalize(spec.resample, 560)
  
  return(spec.out)
}

# UI Code -----

ui <- navbarPage(
  
  # Title
  title = 'Academy Spectral Similarity Index (SSI) Calculator',
  windowTitle = 'SSI Calculator',
  
  # Theme
  theme = shinytheme("flatly"),
  
  # Common header above the tabs
  header = tagList(
    # Enable ShinyJS to enable/disable elements
    useShinyjs(),
    
    # Modify look of horizontal rules to make them more prominent.
    # If more CSS starts to get added, it would be a good idea to pull it
    # out into a separate CSS file rather than putting it all inline.
    tags$head(
      tags$style('hr {border-top: 1px solid #b3b3b3;}')
    )
  ),
  
  # Tab Panel for calculations
  tabPanel(
    title = "Calculations",
    
    # Sidebar
    sidebarLayout(
      
      # Add Dropdown for test spectrum
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = 'testChoice',
          label = 'Test Spectrum',
          choices = list(Fluorescent = specnames(default.testSpec), 'Custom'),
          selected = specnames(default.testSpec)[1]
        ),
        
        # If test spectrum is custom, show widgets to specify wl range and increments
        conditionalPanel(
          "input.testChoice == 'Custom'",
          # Inputs for CCT and Wavelength
          numericInput(
            "test.wlMin",
            label = "Minimum Wavelength",
            value = MIN_WAVELENGTH,
            min = MIN_WAVELENGTH,
            max = 400
          ),
          numericInput(
            "test.wlMax",
            label = "Maximum Wavelength",
            value = MAX_WAVELENGTH,
            min = 700,
            max = MAX_WAVELENGTH
          ),
          numericInput(
            "test.wlInc",
            label = "Wavelength Increments",
            value = 1,
            min = 0.1,
            max = 10
          )
        ),
        
        # Horizontal rule
        hr(),
        
        # Add dropdown for reference spectrum
        selectInput(
          inputId = 'refChoice',
          label = 'Reference Spectrum',
          choices = c('Default', 'Daylight', 'Blackbody'),
          selected = 'Default'
        ),
        
        # If daylight show radio buttons to specify CCT or a canonical daylight.  Returns proper CCT.
        conditionalPanel(
          "input.refChoice == 'Daylight'",
          radioButtons(
            "ref.cieD",
            label = '',
            choiceNames = c('CCT','D50','D55','D65','D75'),
            choiceValues = c('CCT',
                             5000 * CORRECTION_FACTOR,
                             5500 * CORRECTION_FACTOR,
                             6500 * CORRECTION_FACTOR,
                             7500 * CORRECTION_FACTOR),
            inline = TRUE
          )
        ),
        
        # If blackbody how radio buttons to specify CCT or or Illuminant A
        conditionalPanel(
          "input.refChoice == 'Blackbody'",
          radioButtons(
            "ref.cieP",
            label = '',
            choiceNames = c('CCT', 'A'),
            choiceValues = c('CCT', 2848),
            inline = TRUE
          )
        ),
        
        # If daylight add a box where CCT is shown or specified
        conditionalPanel(
          "input.refChoice == 'Daylight'",
          numericInput(
            "ref.cctD",
            label = "CCT",
            value = DEFAULT_DAYLIGHT_CCT,
            min = MIN_DAYLIGHT_CCT,
            max = MAX_DAYLIGHT_CCT
          )
        ),
        
        # If blackbody add a box where CCT is shown or specified
        conditionalPanel(
          "input.refChoice == 'Blackbody'",
          numericInput(
            "ref.cctP",
            label = "CCT",
            value = DEFAULT_BLACKBODY_CCT,
            min = MIN_BLACKBODY_CCT,
            max = MAX_BLACKBODY_CCT
          )
        )
      ),
      
      # Main Panel
      mainPanel(
        width = 9,
        fluidRow(
          column(
            width = 4,
            h3('Data'),
            hr(),
            fluidRow(
              # Table with reference spectrum values
              column(
                width = 6,
                rHandsontableOutput('spectra.ref')
              ),
              # Table with test spectrum values
              column(
                width = 6,
                rHandsontableOutput('spectra.test')
              )
            ),
            br()
          ),
          # Plot
          column(
            width = 8,
            h3('Plot'),
            hr(),
            plotOutput('plot.ref'),
            h3('Results'),
            hr(),
            fluidRow(
              # SSI Result
              column(
                width = 6,
                h4("SSI"),
                textOutput('ssi.text')
              ),
              column(
                width = 6,
                conditionalPanel(
                  "input.refChoice == 'Default'",
                  h4("Default Reference Spectrum Used"),
                  textOutput('cct.test')
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Tab Panel for information
  tabPanel(
    title = "About",
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
    p('Last updated - 02/02/2019')
  )
)


# Server Code ----
server <- function(input, output, session) {
  
  ## Observe Events ----
  
  # Gray out Daylight CCT numericInput if a canonical illuminant
  observeEvent(input$ref.cieD, {
    toggleState('ref.cctD', input$ref.cieD == "CCT")
  })
  
  # Gray out Blackbody CCT numericInput if a canonical illuminant
  observeEvent(input$ref.cieP, {
    toggleState('ref.cctP', input$ref.cieP == "CCT")
  })
  
  
  # Update Daylight CCT numbericInput if a canonical illuminant or 5000K if 'CCT'
  observeEvent(input$ref.cieD, {
    if (input$ref.cieD == "CCT") {
      cct <- DEFAULT_DAYLIGHT_CCT
    } else {
      cct <- input$ref.cieD
    }
    updateNumericInput(session, inputId = 'ref.cctD', value = cct)
  })
  
  # Update Blackbody CCT numbericInput if a canonical illuminant or 3200K if 'CCT'
  observeEvent(input$ref.cieP, {
    if (input$ref.cieP == "CCT") {
      cct <- DEFAULT_BLACKBODY_CCT
    } else {
      cct <- input$ref.cieP
    }
    updateNumericInput(session, inputId = 'ref.cctP', value = cct)
  })
  
  ## Reactive Expressions ----
  
  # Reactive expression to get wavelengths
  getTestWl <- reactive({
    seq(input$test.wlMin,
        input$test.wlMax,
        input$test.wlInc)
  })
  
  # Reactive epression to get test cct
  getTestCCT <- reactive({
    XYZ <- product(getTestSpec(),
                   xyz1931.1nm,
                   wavelength = 'auto')
    if (XYZ[2] == 0) {
      NA
    } else {
      computeCCT(getTestSpec())
    }
  })
  
  # Reactive expression to get reference spectra for default mode
  getRefSpecDefault <- reactive({
    # Calculate test spectrum cct
    cct <- getTestCCT()
    # Check to see if compute CCT throws a NA
    if (is.na(cct)) {
      spec <- getTestSpec()
    } else {
      # Compute default reference spectrum based on test cct
      if (cct <= MIN_DAYLIGHT_CCT) {
        spec <- planckSpectra(cct, MIN_WAVELENGTH:MAX_WAVELENGTH)
      } else {
        spec <- daylightSpectra(cct, MIN_WAVELENGTH:MAX_WAVELENGTH)
      }
    }
    spec
  })
  
  # Reactive expression to get reference spectrum for daylight mode
  getRefSpecDaylight <- reactive({
    validate(
      need(input$ref.cctD <= MAX_DAYLIGHT_CCT,
           glue('CCT must be {MAX_DAYLIGHT_CCT}K or less')),
      need(input$ref.cctD >= MIN_DAYLIGHT_CCT,
           glue('CCT must be  at least {MIN_DAYLIGHT_CCT}K'))
    )
    # Compute daylight reference spectrum
    spec <- daylightSpectra(input$ref.cctD, MIN_WAVELENGTH:MAX_WAVELENGTH)
    spec
  })
  
  # Reactive expression to get reference spectrum for daylight mode
  getRefSpecBlackbody <- reactive({
    # Validate the input to make sure the range is good
    validate(
      need(input$ref.cctP <= MAX_BLACKBODY_CCT,
           glue('CCT must be {MAX_BLACKBODY_CCT}K or less')),
      need(input$ref.cctP >= MIN_BLACKBODY_CCT,
           glue('CCT must be  at least {MIN_BLACKBODY_CCT}K'))
    )
    # Compute blackbody reference spectrum
    if (input$ref.cieP == 'CCT') {
      # For generic blackbody of a given CCT
      spec <- planckSpectra(input$ref.cctP, MIN_WAVELENGTH:MAX_WAVELENGTH)
    } else {
      # For CIE Illuminant A
      spec <- planckSpectra(input$ref.cctP, MIN_WAVELENGTH:MAX_WAVELENGTH, c2 = 1.435e7)
    }
    spec
  })
  
  getRefSpec <- reactive({
    switch(
      input$refChoice,
      'Default' = getRefSpecDefault(),
      'Daylight' = getRefSpecDaylight(),
      'Blackbody' = getRefSpecBlackbody()
    )
  })
  
  getTestSpecTable <- reactive({
    if (input$testChoice != 'Custom') {
      # Subset if not custom
      subset(default.testSpec, glue("{input$testChoice}$"))
    } else {
      validate(
        need(input$test.wlMin >= MIN_WAVELENGTH, glue('Min Wavelength must be >= {MIN_WAVELENGTH}')),
        need(input$test.wlMax <= MAX_WAVELENGTH, glue('Max Wavelength must be <= {MAX_WAVELENGTH}')),
        need(input$test.wlInc >= 0.1, 'Wavelength increments must be >= 0.1'),
        need(input$test.wlInc <= 10, 'Wavelength increments must be <= 10')
      )
      illuminantE(0, getTestWl())
    }
  })
  
  getTestSpec <- reactive({
    if (input$testChoice != 'Custom') {
      getTestSpecTable()
    } else {
      req(input$spectra.test)
      table <- hot_to_r(input$spectra.test)
      req(length(getTestWl()) == length(table))
      colorSpec(table,
                wavelength = getTestWl(),
                organization = 'matrix')
    }
  })
  
  ## Outputs ----
  
  # Calculate the CCT of the test spectrum and prepare text used for display in the UI
  output$cct.test <- renderText({
    cct <- getTestCCT()
    if (is.na(cct)) {
      'No default reference spectrum could be calculated because a CCT
      can not be calculated for the test spectrum'
    } else if (cct <= MIN_DAYLIGHT_CCT) {
      glue('Blackbody with CCT = {round(cct, digits = 2)}K')
    } else if (cct > MIN_DAYLIGHT_CCT && cct < MAX_DAYLIGHT_CCT) {
      glue('CIE Daylight with CCT = {round(cct, digits = 2)}K')
    }
  })
  
  # Generate plot of test and reference spectra
  output$plot.ref <- renderPlot({
    # Calculate test and reference spectra
    spectra.ref <- getRefSpec()
    spectra.test <- getTestSpec()
    # Interpolate and normalize data
    spectra.ref <- interpolateAndNormalize(spectra.ref)
    spectra.test <- interpolateAndNormalize(spectra.test)
    # Combine Test and Reference Spectrum into one ColorSpec object
    specnames(spectra.ref) <- 'Reference'
    specnames(spectra.test) <- 'Test'
    s <- bind(spectra.ref, spectra.test)
    # Define yMax for the plot y-axis limit
    if (max(s) < 1) {
      yMax <- 1
    } else if (max(s) > 30) {
      yMax <- 30
    } else {
      yMax <- max(s)
    }
    # Plot
    plot(s,
         color = c('black', 'red'),
         main = 'Spectral Power Distributions',
         CCT = TRUE,
         ylim = c(0, yMax))
  })
  
  # Calculate SSI
  output$ssi.text <- renderText({
    '100'
  })
  
  # Generate table with test sepctrum
  output$spectra.test <- renderRHandsontable({
    if (input$testChoice == 'Custom') {
      # Make table editable
      ro <- FALSE
      spectra.test <- getTestSpecTable()
    } else {
      # Interpolate and normalize
      spectra.test <- interpolateAndNormalize(getTestSpecTable())
      # Make table read only
      ro <- TRUE
    }
    # Generate Table
    rhandsontable(coredata(spectra.test, forcemat = TRUE),
                  colHeaders = 'Test',
                  digits = 10,
                  readOnly = ro) %>%
      hot_col(1, format = '0.00000')
  })
  
  # Generate Table of Reference Sepctra
  output$spectra.ref <- renderRHandsontable({
    rhandsontable(coredata(getRefSpec(), forcemat = TRUE),
                  colHeaders = 'Reference',
                  digits = 10,
                  readOnly = TRUE) %>%
      hot_col(1, format = '0.00000')
  })
  
}

# Run Application ----
shinyApp(ui = ui, server = server)
