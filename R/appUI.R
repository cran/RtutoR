appUI <- shinyUI(
  dashboardPage(
  dashboardHeader(title = "Plotter"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Read Me", tabName = "readme", icon = icon("dashboard")),
      menuItem("Build a Plot", tabName = "datasets", icon = icon("dashboard")),
      menuItem("Plot Zoom View", tabName = "plot_zoom", icon = icon("dashboard"))

    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),

    tabItems(
      # Dataset Tab
      tabItem(tabName = "datasets",

              fluidRow(
                tabBox(title = "", width = 6,
                       tabPanel("Select data",
                                uiOutput("choose_dataset")
                       ),


                       tabPanel("Variables & Geom type",
                                sliderInput("add_layers","Add additional layer",min=1,max=3,step=1,value=1),

                                uiOutput("select_var"),
                                uiOutput("select_plot"),
                                uiOutput("show_all_plots")


                       ),
                       tabPanel("Set Aesthetics",

                                uiOutput("show_aesthetics_control"),
                                uiOutput("map_aesthetic_to_var"),
                                uiOutput("manual_aesthetic_control"),
                                uiOutput("reset_buttons")

                       ),

                       tabPanel("Other Plot Controls",

                                uiOutput("collapsible_panel"),
                                shinyBS::bsButton("reset_collapse_panel","Reset All", style = "primary")
                       )

                ),

                box(title = "Plot Output", status = "primary", solidHeader = T, width = 6,

                    conditionalPanel(
                      condition = "input.switch_interactivity == false",
                      plotOutput("plot_output")
                    ),
                    conditionalPanel(
                      condition = "input.switch_interactivity == true",
                      plotly::plotlyOutput("interactive_plot_output")
                    ),
                    checkboxInput("switch_interactivity","Switch ON Interactivity", value = F),
                    h4(strong("Underlying Code"),style="color:brown"),
                    textOutput("plot_code")
                )
              )

      ),
      tabItem(tabName = "readme",
              fluidRow(
                box(title = "Read Me", status = "primary", solidHeader = T, width = 12,
                    htmlOutput("read_me")))),
      tabItem(tabName = "plot_zoom",
              fluidRow(
                box(title = "Plot (Enlarged View)", status = "primary", solidHeader = T, width = 12, height = 700,
                    plotOutput("plot_out_zoom"))))
    )
  )
  )
)

