

#' Launch Plotter
#' @description Launches the plotting app
#' @param list_of_datasets List of datasets which should be made available for selection when the app is launched
#' @return Launches App
#' @import dplyr shinydashboard ggplot2 shiny
#' @examples
#' \dontrun{
#' diamonds_sample <- ggplot2::diamonds[sample(1:nrow(diamonds),size=1000),]
#' launch_plotter(list(diamonds_sample = diamonds_sample, mtcars = mtcars, iris = iris))}
#' @export

launch_plotter <- function(list_of_datasets) {


  #### Display reccomended plot lists #####


  applicable_plotlist <- function(x,y){

    plots <- vector()

    if (is.null(y)) {
      if (is.numeric(x)){
        plots <- c("geom_density","geom_histogram")
      } else {
        plots <- c("geom_bar")
      }

    } else if (is.factor(x) & is.numeric(y)) {
      plots <- c("geom_boxplot","geom_violin")
    } else if (is.numeric(x) & is.numeric(y)) {
      plots <- c("geom_point","geom_smooth","geom_line","geom_area","geom_text")
    } else {
      plots <- c("geom_bar")
    }
    plots

  }

  Type <- Aes <- Widget <- NULL
  g <- ggplot(aesthetics) + geom_bar(aes(Type)) + ggthemes::theme_hc()

  shinyApp(ui <- dashboardPage(
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
                         tabPanel("Select data",
                                  uiOutput("choose_dataset")
                         ),


                         tabPanel("Variables & plot type",
                                  sliderInput("add_layers","Add additional layer",min=1,max=3,step=1,value=1),

                                  uiOutput("select_var"),
                                  uiOutput("select_plot"),
                                  uiOutput("show_all_plots")


                         ),
                         tabPanel("Other aesthetics & Controls",

                                  uiOutput("show_aesthetics_control"),
                                  uiOutput("map_aesthetic_to_var"),
                                  uiOutput("manual_aesthetic_control"),
                                  uiOutput("reset_buttons")

                         ),

                         tabPanel("Themes",
                                  selectInput("select_theme","Select theme", choices = c("",as.character(unlist(themes)))),
                                  textInput("add_title","Add Plot Title", value = "")
                         )


                         ),

                  box(title = "Plot Output", status = "primary", solidHeader = T, width = 6,

                      conditionalPanel(
                        condition = "input.switch_interactivity % 2 == 0",
                        plotOutput("plot_output")
                      ),
                      conditionalPanel(
                        condition = "input.switch_interactivity % 2 != 0",
                        plotly::plotlyOutput("interactive_plot_output")
                      ),

                      shinyBS::bsButton("switch_interactivity","Switch ON Interactivity", style = "primary"),
                      h4(strong("Underlying Code"),style="color:brown"),
                      textOutput("plot_code")
                  )
                )

        ),
        tabItem(tabName = "readme",
                fluidRow(
                  box(title = "Read Me", status = "primary", solidHeader = T, width = 12,
                      htmlOutput("read_me"))))
      ))),


    server <- function(input, output, session) {

      #### Read Me ####################
      output$read_me <- reactive({


        val <- as.character(readme_file_plotter[1,1])
        gen_markdown(val)

      })


      #### Tab 1 - Choose a dataset from the dropdown ####################

      output$choose_dataset <- renderUI({

        selectInput("get_dataset","Choose a dataset", choices = names(list_of_datasets))

      })


      #### Display variables from the selected dataset ################

      output$select_var <- renderUI({


        selected_dataset <- eval(parse(text = input$get_dataset))
        vars <- colnames(selected_dataset)
        layerCount <- as.integer(input$add_layers)

        # Dynamically generate the variable list
        lapply(1:layerCount,function(i) {

          div(id= "var_control",
              selectInput(inputId = paste("xaxis_var",i),paste("Layer ", i,  "- Select x-axis"),choices = c("",vars)),
              selectInput(inputId = paste("yaxis_var",i),paste("Layer ", i, "- Select y-axis"),choices = c("",vars))
          )
        })
      })

      ######### Generate Plot dropdown #################

      output$show_all_plots <- renderUI({

        layerCount <- as.integer(input$add_layers)

        lapply(1:layerCount,function(i) {
          shinyBS::bsButton(inputId = paste("show_all_plots",i,sep=""), "Show all plots", style = "primary")
        })
      })



      ####### Show recommended plots basis selected variables ##########

      output$select_plot <- renderUI({

        selected_dataset <- eval(parse(text = input$get_dataset))
        layerCount <- as.integer(input$add_layers)

        lapply(1:layerCount,function(i) {

          x_var <- as.character(input[[paste("xaxis_var",i)]])
          y_var <- as.character(input[[paste("yaxis_var",i)]])

          plot_list <- applicable_plotlist(selected_dataset[[x_var]],selected_dataset[[y_var]])

          if (input[[paste("show_all_plots",i,sep="")]] %% 2 == 0) {
            selectInput(inputId = paste("plot_type",i),paste("Layer ", i, " - Select Plot"), choices = plot_list)
          } else {
            selectInput(inputId = paste("plot_type",i),paste("Layer ", i, " - Select Plot"), choices = all_plots)
          }


        })
      })

      ############## Toggle button - Toggle b/w all plots and recommended plots ########

      observe({

        layerCount <- as.integer(input$add_layers)

        lapply(1:layerCount,function(i) {
          observeEvent(input[[paste("show_all_plots",i,sep="")]], {
            if (input[[paste("show_all_plots",i,sep="")]] %% 2 == 0) {
              shinyBS::updateButton(session,inputId = paste("show_all_plots",i,sep=""), "Show All plots", style = "primary")

            } else {
              shinyBS::updateButton(session,inputId = paste("show_all_plots",i,sep=""), "Show recommended plots", style = "success")
            }

          })
        })
      })

      #### Toggle button - Hide/Show aesthetic controls ############

      output$show_aesthetics_control <- renderUI({

        layerCount <- as.integer(input$add_layers)

        lapply(1:layerCount,function(i) {
          shinyBS::bsButton(inputId = paste("show_aesthetic_control",i,sep=""), paste("Show Aesthetic Control for"," Layer",i,sep=""), style = "primary")
        })
      })

      #### Dynamically generate aesthetic controls (Aesthetic mapped to variable) ######

      output$map_aesthetic_to_var <- renderUI({

        selected_dataset <- eval(parse(text = input$get_dataset))
        vars <- colnames(selected_dataset)

        layerCount <- as.integer(input$add_layers)


        lapply(1:layerCount,function(i) {

          applicable_aes <- aesthetics %>% filter(Geom == input[[paste("plot_type",i)]] & Type == "Aesthetic") %>% select(Aes)
          id_val = paste("map_aes_controls",i,sep="")
          div(id = id_val,
            h4(strong(paste("LAYER ",i, " - MAP AESTHETIC TO VARIABLE")),style="color:darkblue"),
            selectInput(inputId = paste("select_aes",i,sep=""),"Select an Aesthetic",choices = c("",as.character(unlist(applicable_aes)))),
            selectInput(inputId = paste("aes_mapping_var",i,sep=""),"Select Mapping Variable",choices = c("",vars))
          )
        })
      })

      #### Dynamically generate Manual aesthetic Controls ####

      output$manual_aesthetic_control <- renderUI({

        layerCount <- as.integer(input$add_layers)

        lapply(1:layerCount,function(i) {

          applicable_aes <- aesthetics %>% filter(Geom == input[[paste("plot_type",i)]]) %>% select(Aes)
          applicable_aes <- as.character(unlist(applicable_aes))

          aes_widget_form <- aesthetics %>% filter(Geom == input[[paste("plot_type",i)]]) %>% select(Widget, Type)
          length_aes <- nrow(aes_widget_form) - nrow(aes_widget_form[aes_widget_form$Type == "Controls",])
          aes_widget_form <- as.character(unlist(aes_widget_form %>% select(Widget)))


          # reset_value <- input[[paste("reset_button",i,sep="")]]
          id_val <- paste("manual_control_group",i,sep="")
          div(id = id_val,
          lapply(1:length(applicable_aes),function(j) {

            if (j == 1) {
              div(
                h4(strong(paste("LAYER ",i, " - MANUALLY SET AESTHETIC"), style="color:darkblue")),
                eval(parse(text = aes_widget_form[j]))
              )

            } else if (j == (length_aes+1)) {

              div(
              h4(strong(paste("LAYER ",i, " - PLOT SPECIFIC CONTROLS"), style="color:darkblue")),
              eval(parse(text = aes_widget_form[j]))
              )

            } else {
              eval(parse(text = aes_widget_form[j]))
            }


          })
          )

        })
      })

      ############ Generate reset buttons #################

      output$reset_buttons <- renderUI({

        layerCount <- as.integer(input$add_layers)

        lapply(1:layerCount,function(i) {
          shinyBS::bsButton(inputId = paste("reset_button",i,sep=""), "Reset All", style = "primary")
        })
      })

      ############## Reset all values when button is clicked ######

      observe({

        layerCount <- as.integer(input$add_layers)

        lapply(1:layerCount,function(i) {
          observeEvent(input[[paste("reset_button",i,sep="")]], {
            shinyjs::reset(paste("manual_control_group",i,sep=""))
            shinyjs::reset(paste("map_aes_controls",i,sep=""))

          })
        })
      })

      ######### Interactivity Button ########################

      observeEvent(input$switch_interactivity, {

        if (input$switch_interactivity %% 2 == 0) {
          shinyBS::updateButton(session, inputId = "switch_interactivity", label = "Switch ON Interactivity", style = "primary")

        } else {
          shinyBS::updateButton(session, inputId = "switch_interactivity", label = "Switch OFF Interactivity", style = "success")

        }


      })


      ############ Show/Hide Aesthetic Controls ############

      observe({

          layerCount <- as.integer(input$add_layers)

          lapply(1:layerCount,function(i) {
            if (!is.null(input[[paste("show_aesthetic_control",i,sep="")]])) {
              id_val_map <- paste("map_aes_controls",i,sep="")
              id_val_manual <- paste("manual_control_group",i,sep="")
              id_val_reset <- paste("reset_button",i,sep="")
              if (input[[paste("show_aesthetic_control",i,sep="")]] %% 2 == 0) {
                shinyjs::hide(id = id_val_map)
                shinyjs::hide(id = id_val_manual)
                shinyjs::hide(id = id_val_reset)
                shinyBS::updateButton(session,inputId = paste("show_aesthetic_control",i,sep=""), label = paste("Show Layer ",i," Controls",sep=""), style = "primary")

              } else {
                shinyjs::show(id = id_val_map)
                shinyjs::show(id = id_val_manual)
                shinyjs::show(id = id_val_reset)
                shinyBS::updateButton(session,inputId = paste("show_aesthetic_control",i,sep=""), label = paste("Hide Layer ",i," Controls",sep=""), style = "success")
              }
            }

          })

      })


      ######## Construct Plot ###############


      genPlot <- reactive({

        layerCount <- as.integer(input$add_layers)
        layers <- list()

        layers <- lapply(1:layerCount,function(i) {

          # Extract Variable and Plot Type selection
          plot_type <- input[[paste("plot_type",i)]]
          x_var <- input[[paste("xaxis_var",i)]]
          y_var <- input[[paste("yaxis_var",i)]]

          if (!is.null(input[[paste("select_aes",i,sep="")]])) {
            # Extract Mapping variable selection
            selected_aes <- input[[paste("select_aes",i,sep="")]]
            mapping_var <- input[[paste("aes_mapping_var",i,sep="")]]

            # Extract Manual aesthetic control settings
            applicable_aes <- aesthetics %>% filter(Geom == input[[paste("plot_type",i)]]) %>% select(Aes)
            applicable_aes <- as.character(unlist(applicable_aes))
            manual_aes_control <- vector()
            manual_aes_value <- vector()

            manual_aes_control <- sapply(1:length(applicable_aes),function(j) {
              if (!is.null(input[[paste("manual_adjustment",i,j,sep="")]])) {
                if (input[[paste("manual_adjustment",i,j,sep="")]] != "") {
                  manual_aes_control[j] <- applicable_aes[j] } else {
                    manual_aes_control[j] <- ""
                  }

              } else {
                manual_aes_control[j] <- ""
              }
            })

            manual_aes_value <- sapply(1:length(applicable_aes),function(j) {

              if (!is.null(input[[paste("manual_adjustment",i,j,sep="")]])) {
                if (input[[paste("manual_adjustment",i,j,sep="")]] != "") {
                  if (applicable_aes[j] %in% c('color','fill','linetype','position','method','stat')) {
                    manual_aes_value[j] <- paste("'", input[[paste("manual_adjustment",i,j,sep="")]] , "'",sep="")

                  } else {
                    manual_aes_value[j] <- input[[paste("manual_adjustment",i,j,sep="")]]
                  }

                } else {
                  manual_aes_value <- ""
                }

              } else {
                manual_aes_value[j] <- ""
              }

            })

            # Remove aesthetics not manipulated
            manual_aes_control <- manual_aes_control[manual_aes_control != ""]
            manual_aes_value <- manual_aes_value[manual_aes_value != ""]

            manual_adjust_concat <- paste(manual_aes_control,manual_aes_value,sep="=")
            manual_adjust_concat <- paste(manual_adjust_concat,collapse = ",")


            if (selected_aes == "" & manual_adjust_concat == "") {
              layers[i] <- paste(plot_type,"(aes(",x_var,",",y_var,"))")
            } else if(selected_aes != "" & manual_adjust_concat == "") {
              layers[i] <- paste(plot_type,"(aes(",x_var,",",y_var,",",selected_aes,"=",mapping_var,"))")
            } else if(selected_aes == "" & manual_adjust_concat != "") {
              layers[i] <- paste(plot_type,"(aes(",x_var,",",y_var,")", ",",manual_adjust_concat,")")
            } else {
              layers[i] <- paste(plot_type,"(aes(",x_var,",",y_var,",",selected_aes,"=",mapping_var,")", ",",manual_adjust_concat,")")
            }


          } else {
            layers[i] <- paste(plot_type,"(aes(",x_var,",",y_var,"))")
          }

        })

        plot_template <- paste(layers,collapse = "+")
        plot_template <- paste("ggplot(dataset)",plot_template,sep="+")

        # dataset_val <- get_dataset()
        plot_template <- gsub("dataset",input$get_dataset,plot_template)
        plot_template

        # eval(parse(text = plot_template))


      })

      #### Add theme to plot ################
      plot_with_themes <- reactive({

        plot_template <- genPlot()

        if (!is.null(input$select_theme) & input$select_theme != "") {
          plot_template <- paste(plot_template, "+", input$select_theme)
        } else {
          plot_template <- plot_template
        }

        ### Add Title #############

        if (input$add_title != "") {
          plot_template <- paste(plot_template, " + ggtitle(' ", input$add_title," ') ")

        }
        plot_template



      })

      ######## Generate Plot (static) #####

      output$plot_output <- renderPlot({
        validate(
          need(!is.null(genPlot()),"GGGG")
        )

        plot_template <- plot_with_themes()
        eval(parse(text = plot_template))

      })

      ########## Generate Plot (Interactive) ####

      output$interactive_plot_output <- plotly::renderPlotly({

        plot_template <- plot_with_themes()
        g <- eval(parse(text = plot_template))
        plotly::ggplotly(g)

      })

      ########## Display the underlying code ###########

      output$plot_code <- renderText({

        if (input$switch_interactivity %% 2 == 0) {
          plot_with_themes()
        } else {
          paste("plotly::ggplotly(",plot_with_themes(),")")

        }


      })

      session$onSessionEnded(function() {
        stopApp()
  })


}

)
}
