
#' Launch a Shiny App for Exploratory Analysis
#'
#' @param df Dataframe object for exploratory analysis
#'
#' @return Launches a Shiny App. Please refer to the ReadMe page of the App to
#' understand how the app works
#' @export
#' @import ggplot2 shinydashboard rlang
#'
#' @examples
#'  \dontrun{
#' diamonds_sample <- ggplot2::diamonds[sample(1:nrow(ggplot2::diamonds),size=1000),]
#' gen_exploratory_report_app(diamonds_sample)}

gen_exploratory_report_app <- function(df) {

  output_obj_name <- list()
  shinyApp(
    ui <- dashboardPage(
      dashboardHeader(title = "Plotter"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Read Me", tabName = "readme", icon = icon("dashboard")),
          menuItem("Build a Plot", tabName = "datasets", icon = icon("dashboard"))
        )
      ),
      dashboardBody(
        shinyjs::useShinyjs(),

        tabItems(
          # Dataset Tab
          tabItem(tabName = "datasets",

                  fluidRow(
                    tabBox(title = "", width = 6,
                           tabPanel("Report Options",
                                    uiOutput("select_target"),
                                    selectInput("select_theme","Select theme", choices = c("",as.character(unlist(themes))), selected = "ggthemes::theme_fivethirtyeight()"),
                                    selectInput("n_plots","No. of plots per slide", choices = c("2","1")),
                                    selectInput("top_n_option","Select Top N features?", choices = c("No","Yes")),
                                    conditionalPanel(
                                      condition = "input.top_n_option == 'Yes'",
                                      numericInput("top_n_value","No. of features to select",value = 10),
                                      selectInput("feature_selector","Select feature selector method",
                                                  choices = c("chi.squared","information.gain","gain.ratio","symmetrical.uncertainty"))
                                    ),
                                    textInput("file_name","Enter the output file name (e.g res.pptx)"),
                                    numericInput("max_levels_cat_variable","Exclude categorical features with no. of levels greater than",value = 10),
                                    shinyBS::bsButton("gen_report","Generate Report", style = "primary")
                           )
                    ),

                    box(title = "Plot Output", status = "primary", solidHeader = T, width = 6,

                        uiOutput("plot_selector"),
                        plotly::plotlyOutput("plot_out"),
                        shinyBS::bsButton("next_plot","Next Plot", style = "primary")
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
    ,


    server <- function(input, output, session) {


      #### Read Me ####################
      output$read_me <- reactive({

        includeMarkdown("./R/ReadMe_ppt_app.rmd")

      })

      output$select_target <- renderUI({
        vars <- colnames(df)
        selectInput(
          inputId = "target_var","Select Target Variable",choices = c("",vars)
        )
      })


      generate_report <- reactive({

        withProgress(message = 'Making plot',{
          target_var <- input$target_var
          if(input$top_n_option == "Yes") {
            output_obj_name <<- generate_exploratory_analysis_ppt(df, target_var = target_var,
                                                                  output_file_name = input$file_name,
                                                                  plot_theme = input$select_theme,
                                                                  n_plots_per_slide = input$n_plots,
                                                                  top_k_features = input$top_n_value,
                                                                  f_screen_model = input$feature_selector,
                                                                  max_levels_cat_var = input$max_levels_cat_variable
                                                                  )

          } else {
            output_obj_name <<- generate_exploratory_analysis_ppt(df, target_var = target_var,
                                                                  output_file_name = input$file_name,
                                                                  plot_theme = input$select_theme,
                                                                  n_plots_per_slide = input$n_plots,
                                                                  max_levels_cat_var = input$max_levels_cat_variable
                                                                  )

          }

           output_obj_name
        })

      })

      observeEvent(input$gen_report,{

        generate_report()

        output$plot_selector <- renderUI({
          plot_names <- names(output_obj_name$plots)
          selectInput("plot_names","Select a Plot",plot_names)
        })

      })


      observeEvent(input$next_plot,{

        output$plot_out <- plotly::renderPlotly({
          plots <- output_obj_name$plots
          count <- input$next_plot %% length(plots)
          if (count == 0) {
            count = length(plots)
          }
          plotly::ggplotly(plots[[count + 1]])
        })


      })

      observeEvent(input$plot_names,{

        output$plot_out <- plotly::renderPlotly({
          sel_plot <- input$plot_names
          plots <- output_obj_name$plots
          plotly::ggplotly(plots[[sel_plot]])

        })

      })


    })

}
