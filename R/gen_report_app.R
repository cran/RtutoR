
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

  plot_counter <- NULL

  output_obj_name <- list()
  shinyApp(
    ui <- dashboardPage(
      dashboardHeader(title = "Report Generator"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Read Me", tabName = "readme", icon = icon("dashboard")),
          menuItem("Generate Report", tabName = "datasets", icon = icon("dashboard"))
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
                                    uiOutput("grouping_var"),
                                    shinyBS::bsButton("gen_report","Generate Report", style = "primary")
                           )
                    ),

                    box(title = "Plot Output", status = "primary", solidHeader = T, width = 6,
                        uiOutput("plot_category_selector"),
                        uiOutput("plot_selector"),
                        plotly::plotlyOutput("plot_out")
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

        includeMarkdown(system.file("ReadMe_ppt_app.rmd", package = "RtutoR"))

      })

      output$select_target <- renderUI({
        vars <- colnames(df)
        selectInput(
          inputId = "target_var","Select Target Variable",choices = c("",vars)
        )
      })

      output$grouping_var <- renderUI({
        validate(
          need(input$target_var != "", "")
        )

        if (input$target_var != "") {
          vars <- colnames(df)

          # show only categorical features
          # Change character to factors
          df[,which(sapply(df,class) == "character")] = lapply(df[,which(sapply(df,class) == "character"),drop=F], as.factor)

          # remove variables with more than specified no. of levels
          df = df[,sapply(df, function(x) length(levels(x))) <= input$max_levels_cat_variable]
          data_type <- sapply(df,class)
          data_type = data_type[-which(names(data_type) == input$target_var)]
          categorical_features <- names(data_type)[sapply(data_type,function(x) any(x %in% c("factor","character","logical")))]

          selectInput(
            inputId = "group_var","Select Grouping Variable",choices = c("No Grouping Variable",categorical_features),multiple = TRUE, selected = "No Grouping Variable"
          )
        }

      })


      generate_report <- reactive({

        withProgress(message = 'Making plot',{
          target_var <- input$target_var
          if(input$top_n_option  == "Yes") {
            if(input$group_var == "No Grouping Variable"){
              group_names = NULL
            } else {
              group_names <- as.character(input$group_var)
            }
            output_obj_name <<- generate_exploratory_analysis_ppt(df, target_var = target_var,
                                                                  output_file_name = input$file_name,
                                                                  plot_theme = input$select_theme,
                                                                  n_plots_per_slide = input$n_plots,
                                                                  top_k_features = input$top_n_value,
                                                                  f_screen_model = input$feature_selector,
                                                                  max_levels_cat_var = input$max_levels_cat_variable,
                                                                  group_names = group_names
                                                                  )

          } else {
            if(input$group_var == "No Grouping Variable"){
              group_names = NULL
            } else {
              group_names <- as.character(input$group_var)
            }
            output_obj_name <<- generate_exploratory_analysis_ppt(df, target_var = target_var,
                                                                  output_file_name = input$file_name,
                                                                  plot_theme = input$select_theme,
                                                                  n_plots_per_slide = input$n_plots,
                                                                  max_levels_cat_var = input$max_levels_cat_variable,
                                                                  group_names = group_names
                                                                  )

          }

           output_obj_name
        })

      })

      observeEvent(input$gen_report,{

        generate_report()

        output$plot_category_selector <- renderUI({
          cat_names <- names(output_obj_name)
          selectInput("plot_cat_names","Select Plot Category",cat_names)
        })

      })

      observeEvent(input$plot_cat_names,{
        output$plot_selector <- renderUI({
          plot_names <- names(output_obj_name[[as.character(input$plot_cat_names)]][["plots"]])
          selectInput("plot_names","Select a Plot",plot_names)
        })
      })


      observeEvent(input$plot_names,{

        plot_counter <<- 1
        output$plot_out <- plotly::renderPlotly({
          sel_plot_name <- input$plot_names
          sel_plot <- output_obj_name[[as.character(input$plot_cat_names)]][["plots"]][[sel_plot_name]]
          plotly::ggplotly(sel_plot)

        })

      })


    })

}
