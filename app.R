# Import Libraries ----
library(shiny)
library(rhandsontable)
library(colorSpec)
library(shinythemes)
library(shinyjs)
library(glue)

# Define constants ----
CORRECTION_FACTOR_DAYLIGHT  <- 14388 / 14380
CORRECTION_FACTOR_ILLUM_A   <- 14350 / 14388
MIN_WAVELENGTH              <- 300
MAX_WAVELENGTH              <- 830
DEFAULT_DAYLIGHT_CCT        <- 5000
DEFAULT_BLACKBODY_CCT       <- 3200
MIN_DAYLIGHT_CCT            <- 4000
MAX_DAYLIGHT_CCT            <- 25000
MIN_BLACKBODY_CCT           <- 1000
MAX_BLACKBODY_CCT           <- 10000

# App revision info
commit.id <- substr(system("git log --format='%H' -n 1", intern = TRUE),
                    1, 7)
commit.url <- paste0('https://www.github.com/ampas/ssi_calculator/commit/',
                     commit.id)
commit.datetime <- system("git show -s --format=%ci",
                          intern = TRUE)

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

# Load Test Spectra ----
cieF.testSpec <- interpolateAndNormalize(Fs.5nm)
external.testSpec <- interpolateAndNormalize(readSpectra('data/testSources.txt'))
default.testSpec <- bind(cieF.testSpec, external.testSpec)

# UI Code ----
ui <- navbarPage(

  # Title
  title = 'Academy Spectral Similarity Index (SSI) Calculator (BETA)',
  windowTitle = 'SSI Calculator',

  # Theme
  theme = shinytheme("flatly"),

  # Common header above the tabs
  header = tagList(# Enable ShinyJS to enable/disable elements
    useShinyjs(),

    # Modify look of horizontal rules to make them more prominent.
    # If more CSS starts to get added, it would be a good idea to pull it
    # out into a separate CSS file rather than putting it all inline.
    tags$head(tags$style('hr {border-top: 1px solid #b3b3b3;}'))),

  ## Calculations Tab Panel----
  tabPanel(title = "Calculations",

           # Sidebar
           sidebarLayout(
             # Add Dropdown for test spectrum
             sidebarPanel(
               width = 3,
               selectInput(
                 inputId = 'testChoice',
                 label = 'Test Spectrum',
                 choices = list('Custom',
                                'Gas Discharge' = specnames(external.testSpec)[3:4],
                                'LED' = specnames(external.testSpec)[1:2],
                                'Fluorescent' = specnames(cieF.testSpec)
                                ),
                 selected = 'Custom'
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

               # If daylight show radio buttons to specify CCT or a
               # canonical daylight. Returns proper CCT.
               conditionalPanel(
                 "input.refChoice == 'Daylight'",
                 radioButtons(
                   "ref.cieD",
                   label = '',
                   choiceNames = c('CCT', 'D50', 'D55', 'D65', 'D75'),
                   choiceValues = c(
                     'CCT',
                     5000 * CORRECTION_FACTOR_DAYLIGHT,
                     5500 * CORRECTION_FACTOR_DAYLIGHT,
                     6500 * CORRECTION_FACTOR_DAYLIGHT,
                     7500 * CORRECTION_FACTOR_DAYLIGHT
                   ),
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
                   choiceValues = c('CCT', 2855.542),
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
             mainPanel(width = 9,
                       fluidRow(
                         column(width = 4,
                                h3('Data'),
                                hr(),
                                fluidRow(
                                  # Table with test spectrum values
                                  column(width = 6,
                                         rHandsontableOutput('spectra.test')),
                                  # Table with reference spectrum values
                                  column(width = 6,
                                         rHandsontableOutput('spectra.ref'))
                                ),
                                br()),
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
                               h4("Spectral Similarity Index"),
                               wellPanel(textOutput('ssi.text'))
                             ),
                             column(
                               width = 6,
                               conditionalPanel(
                                 "input.refChoice == 'Default'",
                                 h4("Default Reference Spectrum Used"),
                                 wellPanel(textOutput('cct.test'))
                               )
                             )
                           ),
                           fluidRow(
                             #Beta Notices
                             column(
                               width = 12,
                               tags$b('NOTICE:'),
                               div(
                                 'The Academy SSI Calculator is currently in beta testing!'
                               ),
                               div(
                                 'The calculator should not yet be considered a reference
                               implemenation of the Spectral Similarity Index (SSI) as in may
                               contain erorrs. Please log issues on',
                                 a(href = 'https://github.com/ampas/ssi_calculator/issues', 'Github', target = '_blank')
                               )
                             )
                           )
                         )
             ))
             )),

  ## About Tab Panel ----
  tabPanel(
    title = "About",
    h2('Introduction'),
    div(
      includeMarkdown('INTRODUCTION.md'),
      h2('Software'),
      'This calculator was built using',
      a(href = 'https://www.r-project.org/',
        'R',
        target = '_blank'),
      ',',
      a(href = 'https://shiny.rstudio.com/',
        'Shiny',
        target = '_blank'),
      ',',
      a(href = 'https://www.rstudio.com/products/shiny/shiny-server/',
        'Shiny Server Open Source',
        target = '_blank'),
      'and',
      a(href = 'https://cran.r-project.org/web/packages/colorSpec/index.html',
        'colorSpec',
        target = '_blank'),
      br(),
      'Current software versions : ',
      tags$ul(
        tags$li(R.version.string),
        tags$li('Shiny',
                as.character(packageVersion('shiny'))),
        tags$li(
          try(
            system('shiny-server --version',
                 intern = TRUE,
                 ignore.stderr = TRUE)[1]
            )
          ),
        tags$li('colorSpec',
                as.character(packageVersion('colorSpec')))
        ),
      'The calculator source code was last updated on ',
      commit.datetime,
      br(),
      'The git commit id of the current calculator code is ',
      a(href = commit.url,
             commit.id,
             target = '_blank'),
      h2('License Terms'),
      br(),
      includeMarkdown('LICENSE.md')
    ),
    style = "width:900px;"
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

  # Reactive expression to get the custom wavelengths
  getCustomWl <- reactive({
    seq(input$test.wlMin,
        input$test.wlMax,
        input$test.wlInc)
  })

  # Reactive expression to get the current wavelengths that should be used
  getCurrentWl <- reactive({
    if (input$testChoice == 'Custom') {
      getCustomWl()
    } else {
      seq(MIN_WAVELENGTH, MAX_WAVELENGTH)
    }
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

  # Reactive expression to get the SSI for test/ref spectra
  getSSI <- reactive({
    computeSSI(
      getTestSpec(),
      getRefSpec()
      )
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
        spec <- planckSpectra(cct, getCurrentWl())
      } else {
        spec <- daylightSpectra(cct, getCurrentWl())
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
    spec <- daylightSpectra(input$ref.cctD, getCurrentWl())
    if (input$ref.cieD == 5000 * CORRECTION_FACTOR_DAYLIGHT) {
      specnames(spec) <- 'CIE D50'
    } else if (input$ref.cieD == 5500 * CORRECTION_FACTOR_DAYLIGHT) {
      specnames(spec) <- 'CIE D55'
    } else if (input$ref.cieD == 6500 * CORRECTION_FACTOR_DAYLIGHT) {
      specnames(spec) <- 'CIE D65'
    } else if (input$ref.cieD == 7500 * CORRECTION_FACTOR_DAYLIGHT) {
      specnames(spec) <- 'CIE D75'
    }
    spec
  })

  # Reactive expression to get reference spectrum for blackbody mode
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
      spec <- planckSpectra(input$ref.cctP,
                            getCurrentWl())
    } else {
      # For CIE Illuminant A
      spec <- planckSpectra(input$ref.cctP * CORRECTION_FACTOR_ILLUM_A,
                            getCurrentWl(),
                            c2 = 1.435e-2)
      specnames(spec) <- 'CIE A'
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
        need(input$test.wlMin >= MIN_WAVELENGTH,
             glue('Min Wavelength must be >= {MIN_WAVELENGTH}')),
        need(input$test.wlMax <= MAX_WAVELENGTH,
             glue('Max Wavelength must be <= {MAX_WAVELENGTH}')),
        need(input$test.wlInc >= 0.1,
             'Wavelength increments must be >= 0.1'),
        need(input$test.wlInc <= 10,
             'Wavelength increments must be <= 10')
      )
      illuminantE(0, getCurrentWl())
    }
  })

  getTestSpec <- reactive({
    if (input$testChoice != 'Custom') {
      getTestSpecTable()
    } else {
      req(input$spectra.test)
      table <- hot_to_r(input$spectra.test)
      req(length(getCurrentWl()) == length(table))
      colorSpec(table,
                wavelength = getCurrentWl(),
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
    ssi <- getSSI()
    glue(
      strsplit(names(ssi)[1], "_")[[1]][2],
      '=',
      ssi[[1]],
      .sep = " "
      )
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
                  digits = 20,
                  width = 150,
                  stretchH = 'all',
                  readOnly = ro) %>%
      hot_col(1, format = '0.00000')
  })

  # Generate Table of Reference Sepctra
  output$spectra.ref <- renderRHandsontable({
    rhandsontable(coredata(getRefSpec(), forcemat = TRUE),
                  colHeaders = 'Reference',
                  digits = 20,
                  width = 150,
                  stretchH = 'all',
                  readOnly = TRUE) %>%
      hot_col(1, format = '0.00000')
  })

}

# Run Application ----
shinyApp(ui = ui, server = server)
