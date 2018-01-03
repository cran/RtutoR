
#' Generate Exploratory Analysis Report
#'
#' @param df Dataframe object for exploratory analysis
#' @param target_var Dependent Variable Name
#' @param output_file_name File Name for the Output presentation. Saves the output in the current working
#' directory if only the file name is provided
#' @param plot_theme Specify a theme for ggplot. Includes themes provided as part of ggplot2 as well as themes
#' available in the ggthemes package. Launch the shiny app to view the valid set of themes available
#' @param group_names Grouping variable (or multiple grouping variables) to be used for analysis.
#' if this argument is supplied, an additional section is added to the Exploratory Analysis report:
#' "Bi variate Analysis with Grouping variable".
#' @param n_plots_per_slide Presentation Slide Format - Choose between 1 or 2 plots per slide.
#' Default value is 2
#' @param top_k_features Should top k features be selected for analysis instead of all available features
#' in the dataset.If yes, provide a numeric value to this argument. The Top K features are identified based
#' on a filtering approach. Four different filtering options are provided (from the FSelector package)
#' @param f_screen_model Filtering approach for extracting the top k features
#' @param max_levels_cat_var Remove categorical features with more than a specified number of levels.
#' Default is 10
#' @import ggplot2 rlang ReporteRs tidyr FSelector
#' @importFrom stats cor fivenum median
#'
#' @return Output presentation with Univariate and Bi-variate analysis and plots
#' @export
#'
#' @examples
#'  \dontrun{
#' diamonds_sample <- ggplot2::diamonds[sample(1:nrow(ggplot2::diamonds),size=1000),]
#' generate_exploratory_analysis_ppt(df = diamonds_sample, target_var = "price")}
#'
generate_exploratory_analysis_ppt <- function(df, target_var, output_file_name,
                                              n_plots_per_slide = 2,
                                              plot_theme = "ggthemes::theme_fivethirtyeight()",
                                              group_names = NULL,
                                              top_k_features = NULL,
                                              f_screen_model = c("chi.squared","information.gain","gain.ratio","symmetrical.uncertainty"),
                                              max_levels_cat_var = 10) {


  # Input validation
  input_validation(df = df, target_var = target_var,
                   n_plots_per_slide = n_plots_per_slide, group_names = group_names,
                   plot_theme = plot_theme)

  # Change character to factors
  df[,which(sapply(df,class) == "character")] = lapply(df[,which(sapply(df,class) == "character"),drop=F], as.factor)

  # remove variables with more than specified no. of levels
  df = df[,sapply(df, function(x) length(levels(x))) <= max_levels_cat_var]

  # if top_k_features argument is provided perform feature selection to select top k features
  if(!is.null(top_k_features)) {
    var_imp <- eval(parse(text = f_screen_model))(substitute(target_var ~ ., list(target_var = as.name(target_var))), df)
    top_k_vars <- cutoff.k(var_imp,top_k_features)
    retain_cols <- c(top_k_vars,target_var)
    df <- df[,colnames(df) %in% retain_cols]
  }

  # Separate out numerical and categorical features
  vars <- colnames(df)

  data_type <- sapply(df,class)
  target_var_type <- data_type[which(names(data_type) == target_var)]

  data_type = data_type[-which(names(data_type) == target_var)]

  numeric_features <- names(data_type)[sapply(data_type,function(x) any(x %in% c("numeric","integer","double")))]
  categorical_features <- names(data_type)[sapply(data_type,function(x) any(x %in% c("factor","character","logical")))]


  # Perform Univariate Analysis for each of the features

  # Univariate analysis for Numeric features
  univariate_hist_plots <- lapply(numeric_features, function(var_name) histogram_func(
    data = df,var_name = var_name, plot_theme = plot_theme))
  names(univariate_hist_plots) <- lapply(numeric_features, function(x) paste("Histogram -",x))

  univar_num_features_summary <- lapply(numeric_features, function(var_name) {
    val <- data.frame(t(fivenum(df[[var_name]])))
    val[] = lapply(val, function(x) round(x,2))
    colnames(val) <- c("min","25th","50th","75th","max")
    return(val)

  })

  # Univariate analysis for Categorical features
  univariate_bar_plots <- lapply(categorical_features, function(var_name) barplot_univariate(
    data = df,var_name = var_name, plot_theme = plot_theme))
  names(univariate_bar_plots) <- lapply(categorical_features, function(x) paste("Bar Plot -",x))

  univar_cat_features_summary <- lapply(categorical_features, function(var_name) {
    count <- table(df[[var_name]])
    perct <- paste0(round(prop.table(count)*100,2),"%")
    val <- as.data.frame(rbind(count,perct))
    labels <- data.frame(Metric = c("Count","Perct."))
    val <- cbind(labels,val)
    return(val)

  })

  # Combine results from univariate analysis
  all_plots_univar <- c(univariate_hist_plots,univariate_bar_plots)
  all_tables_univar <- c(univar_num_features_summary,univar_cat_features_summary)
  res_univar <- list(plots = all_plots_univar,tables = all_tables_univar)


  # Bi-variate analysis for Categorical Target variable
  if (target_var_type == "factor") {

    # Bi-variate analysis for numeric features

    boxplot_list <- lapply(numeric_features, function(var_name) eval(parse(text="boxplot_categorical_target"))(
      data = df,target_var = target_var, var_name = var_name, plot_theme = plot_theme,group_name = NULL))
    names(boxplot_list) <- lapply(numeric_features, function(x) paste(x, "VS",target_var))

    numeric_features_summary <- lapply(numeric_features, function(var_name) {
      res <- df %>% group_by(!!sym(target_var)) %>% summarise(mean_val = round(mean(!!sym(var_name),na.rm = T),2),
                                                       median_val = round(median(!!sym(var_name),na.rm = T),2))
      colnames(res)[2:3] = c(paste("mean -",var_name),paste("median -",var_name))
      return(res)})


    # If grouping variables are provided perform additional analysis
    if (!is.null(group_names)) {

      group_var_combo <- expand.grid(x = numeric_features,y = group_names)
      group_var_combo <- group_var_combo[as.character(group_var_combo$x) != as.character(group_var_combo$y),]

      if(nrow(group_var_combo) > 0) {
        boxplot_with_group_var <- apply(group_var_combo, 1, function(i) {
          boxplot_categorical_target(
            data = df,target_var = target_var, var_name = i[1], plot_theme = plot_theme,group_name = i[2])
        })
        names(boxplot_with_group_var) <- apply(group_var_combo,1,function(x) paste(x[1]," vs ",target_var,"group by",x[2]))


        numeric_features_summary_group_var <- apply(group_var_combo,1, function(i) {
          df %>% group_by(!!sym(target_var),!!sym(i[2])) %>% summarise(mean_val = round(mean(!!sym(i[1]),na.rm = T),2),
                                                                       median_val = round(median(!!sym(i[1]),na.rm = T),2))
          })

      } else {
        boxplot_with_group_var <- NULL
        numeric_features_summary_group_var <- NULL
      }

    }

    # Bi-variate analysis for categorical features

    barplot_list <- lapply(categorical_features, function(var_name) barplot_func(
      data = df,target_var = target_var, var_name = var_name, plot_theme = plot_theme, group_name = NULL))
    names(barplot_list) <- lapply(categorical_features, function(x) paste(x, "VS",target_var))


    categorical_features_summary <- lapply(categorical_features, function(var_name) {
      count_df <- df %>% count(!!sym(target_var),!!sym(var_name)) %>% spread(!!sym(var_name), n,fill =0)
      perct_df <- round(prop.table(as.matrix(count_df[,c(2:ncol(count_df))]),2),4)*100
      perct_df <- as.data.frame(perct_df)
      perct_df[] = lapply(perct_df,function(x) paste0(x,"%"))
      res <- cbind(count_df,perct_df)
      return(res)}
    )

    if(!is.null(group_names)) {

      group_var_combo <- expand.grid(x = categorical_features,y = group_names)
      group_var_combo <- group_var_combo[as.character(group_var_combo$x) != as.character(group_var_combo$y),]

      if(nrow(group_var_combo) > 0) {
        barplot_with_group_var <- apply(group_var_combo, 1, function(i) {
          barplot_func(
            data = df,target_var = target_var, var_name = i[1], plot_theme = plot_theme,group_name = i[2])
        })
        names(barplot_with_group_var) <- apply(group_var_combo,1,function(x) paste(x[1]," vs ",target_var,"group by",x[2]))


        categorical_features_group_var <- apply(group_var_combo, 1, function(i) {
          count_df <- df %>% count(!!sym(target_var),!!sym(i[1]), !!sym(i[2])) %>% spread(!!sym(i[1]), n,fill =0)
          return(count_df)}
        )

      } else {
        barplot_with_group_var <- NULL
        categorical_features_group_var <- NULL
      }


    }

    all_plots_bivar <- c(boxplot_list,barplot_list)
    all_tables_bivar <- c(numeric_features_summary,categorical_features_summary)
    res_bivar <- list(plots = all_plots_bivar,tables = all_tables_bivar)

    if(!is.null(group_names)) {
      all_plots_bivar_group <- c(boxplot_with_group_var,barplot_with_group_var)
      all_tables_bivar_group <- c(numeric_features_summary_group_var,categorical_features_group_var)
      res_bivar_group <- list(plots = all_plots_bivar_group,tables = all_tables_bivar_group)
      final_res <- list(univar = res_univar,bivar = res_bivar,bivar_with_group_var = res_bivar_group)


    } else {

      final_res <- list(univar = res_univar,bivar = res_bivar)
    }

    generate_ppt(final_res,n_plots_per_slide = n_plots_per_slide, output_file_name = output_file_name)
    return(final_res)


  } else if (target_var_type %in% c("integer","numeric")) {

    boxplot_list <- lapply(categorical_features, function(var_name) boxplot_numeric_target(
      data = df,target_var = target_var, var_name = var_name, plot_theme = plot_theme, group_name =  var_name))
    names(boxplot_list) <- lapply(categorical_features, function(x) paste(x, "VS",target_var))

    cat_features_summary <- lapply(categorical_features, function(var_name) {
      res <- df %>% group_by(!!sym(var_name)) %>% summarise(mean_val = round(mean(!!sym(target_var),na.rm = T),2),
                                                     median_val = round(median(!!sym(target_var),na.rm = T),2))
      colnames(res)[2:3] = c(paste("mean -",target_var),paste("median -",target_var))
      return(res)})

    if(!is.null(group_names)) {

      group_var_combo <- expand.grid(x = categorical_features,y = group_names)
      group_var_combo <- group_var_combo[as.character(group_var_combo$x) != as.character(group_var_combo$y),]

      if(nrow(group_var_combo) > 0) {
        boxplot_with_group_var <- apply(group_var_combo, 1, function(i) {
          boxplot_numeric_target(
            data = df,target_var = target_var, var_name = i[1], plot_theme = plot_theme,group_name = i[2])
        })
        names(boxplot_with_group_var) <- apply(group_var_combo,1,function(x) paste(x[1]," vs ",target_var,"group by",x[2]))

        cat_features_summary_group_var <- apply(group_var_combo,1, function(i) {
          df %>% group_by(!!sym(i[1]),!!sym(i[2])) %>% summarise(mean_val = round(mean(!!sym(target_var),na.rm = T),2),
                                                                 median_val = round(median(!!sym(target_var),na.rm = T),2))})


      } else {
        boxplot_with_group_var <- NULL
        cat_features_summary_group_var <- NULL
      }

    }

    scatterplot_list <- lapply(numeric_features, function(var_name) scatterplot_func(
      data = df,target_var = target_var, var_name = var_name, plot_theme = plot_theme, group_name = NULL))
    names(scatterplot_list) <- lapply(numeric_features, function(x) paste(x, "VS",target_var))

    corr_numeric_features <- lapply(numeric_features, function(var_name) {
      cor_val <- round(cor(df[[target_var]],df[[var_name]],use="complete.obs"),3)
      res <- data.frame(Feature = var_name, Correlation = cor_val)
      colnames(res) = c("Feature","Correlation with Target")
      return(res)
    })

    if (!is.null(group_names)) {
      group_var_combo <- expand.grid(x = numeric_features,y = group_names)
      group_var_combo <- group_var_combo[as.character(group_var_combo$x) != as.character(group_var_combo$y),]

      if(nrow(group_var_combo) > 0) {
        scatterplot_wth_group_var <- apply(group_var_combo, 1, function(i) scatterplot_func(
          data = df,target_var = target_var, var_name = i[1], plot_theme = plot_theme, group_name = i[2]))
        names(scatterplot_wth_group_var) <- apply(group_var_combo,1,function(x) paste(x[1]," vs ",target_var,"group by",x[2]))

        corr_num_features_group_var <- apply(group_var_combo, 1, function(i) {
          res <- df %>% group_by(!!sym(i[2])) %>% summarise(cor(!!sym(target_var),!!sym(i[1]),use="complete.obs"))
          colnames(res)[2] <- "Correlation with Target"
          return(res)
        })

      } else {
        scatterplot_wth_group_var <- NULL
        corr_num_features_group_var <- NULL
      }

    }



    all_plots_bivar <- c(boxplot_list,scatterplot_list)
    all_tables_bivar <- c(cat_features_summary,corr_numeric_features)


    res_bivar <- list(plots = all_plots_bivar,tables = all_tables_bivar)

    if(!is.null(group_names)) {
      all_plots_bivar_group <- c(boxplot_with_group_var,scatterplot_wth_group_var)
      all_tables_bivar_group <- c(cat_features_summary_group_var,corr_num_features_group_var)
      res_bivar_group <- list(plots = all_plots_bivar_group,tables = all_tables_bivar_group)
      final_res <- list(univar = res_univar,bivar = res_bivar,bivar_with_group_var = res_bivar_group)


    } else {

      final_res <- list(univar = res_univar,bivar = res_bivar)
    }

    generate_ppt(final_res,n_plots_per_slide = n_plots_per_slide, output_file_name = output_file_name)
    return(final_res)



  }
}



# Plotting functions

boxplot_categorical_target = function (data, target_var, var_name, plot_theme, group_name) {

  if(is.null(group_name)) {
    g <- ggplot(data = data, aes_string(x = target_var, y = var_name, fill = target_var)) + geom_boxplot() +
      xlab(target_var)
  } else {
    g <- ggplot(data = data, aes_string(x = target_var, y = var_name, fill = group_name)) + geom_boxplot() +
      xlab(target_var)
  }

  g <- g + ggtitle(paste(g$labels$x," vs ", g$labels$y))
  eval(parse(text = paste("g + ",plot_theme)))
}

boxplot_numeric_target = function (data, target_var, var_name, plot_theme, group_name) {

  if(is.null(group_name)) {
    g <- ggplot(data = data, aes_string(x = var_name, y = target_var, fill = var_name)) + geom_boxplot() +
      xlab(var_name)
  } else {
    g <- ggplot(data = data, aes_string(x = var_name, y = target_var, fill = group_name)) + geom_boxplot() +
      xlab(var_name)
  }

  g <- g + ggtitle(paste(g$labels$x," vs ", g$labels$y))
  eval(parse(text = paste("g + ",plot_theme)))
}


barplot_func = function (data, target_var, var_name,plot_theme, group_name) {
  if(is.null(group_name)) {
    g <- ggplot(data = data, aes_string(x = var_name, fill = target_var)) + geom_bar(position = "fill") +
      xlab(var_name)
  } else {
    g <- ggplot(data = data, aes_string(x = var_name, fill = target_var)) + geom_bar(position = "fill") +
      facet_wrap(group_name) + xlab(var_name)
  }

  g <- g + ggtitle(paste(g$labels$fill," vs ", g$labels$x))
  eval(parse(text = paste("g + ",plot_theme)))
}

scatterplot_func = function (data, target_var, var_name, plot_theme, group_name) {

  g <- ggplot(data = data, aes_string(x = target_var, y = var_name,color = group_name)) + geom_point() + geom_smooth() +
    xlab(target_var)
  g <- g + ggtitle(paste(g$labels$x," vs ", g$labels$y))
  eval(parse(text = paste("g + ",plot_theme)))
}

histogram_func <- function (data, var_name,plot_theme) {
  g <- ggplot(data = data) + geom_histogram(aes_string(x = var_name)) +
    xlab(var_name)
  g <- g + ggtitle(paste(g$labels$y, " of ", g$labels$x))

  eval(parse(text = paste("g + ",plot_theme)))
}

barplot_univariate <- function (data, var_name, plot_theme) {
  g <- ggplot(data = data) + geom_bar(aes_string(x=var_name,fill=var_name)) +
    xlab(var_name)
  g <- g + ggtitle(paste(g$labels$y, " of ", g$labels$x))
  eval(parse(text = paste("g + ",plot_theme)))
}





generate_ppt <- function(final_res,n_plots_per_slide = c("1","2"),
                         output_file_name = "res.pptx") {



  doc = pptx()

  # Slide 1 : Title slide
  doc <- addSlide(doc, "Title Slide")
  doc <- addTitle(doc,"Exploratory Analysis Report")

  # Add section labels
  if(length(final_res) == 2) {
    section_labels = c("Univariate Analysis","Bi-variate analysis")
  } else if (length(final_res) == 3) {
    section_labels = c("Univariate Analysis","Bi-variate analysis","Bi-variate with Group_var")
  }

  if (n_plots_per_slide == "1") {
    for (i in 1:length(final_res)) {
      all_plots <- final_res[[i]]$plots
      all_tables <- final_res[[i]]$tables
      doc <- add_slides_from_plot_list(doc,all_plots,all_tables,
                                       n_plots_per_slide = 1, section_label = section_labels[i])
    }


  } else if (n_plots_per_slide == "2") {
    for (i in 1:length(final_res)) {
      all_plots <- final_res[[i]]$plots
      all_tables <- final_res[[i]]$tables
      doc <- add_slides_from_plot_list(doc,all_plots,all_tables,
                                       n_plots_per_slide = 2,section_label = section_labels[i])
    }

  }


  writeDoc(doc, file = output_file_name)


}

add_slides_from_plot_list <- function(doc,all_plots,all_tables,
                                      n_plots_per_slide, section_label
                                      ) {

  doc <- addSlide(doc, "Section Header")
  doc <- addParagraph(doc, section_label)

  if(n_plots_per_slide == 1) {

    for (i in 1:length(all_plots)) {

      doc <- addSlide(doc, "Two Content")
      title <- paste(section_label,"Slide ", iter, "of", length(all_plots))
      doc <- addTitle(doc, title, level = 6)


      doc <- addPlot(doc, fun = print, x = all_plots[[i]],
                     offx = 1, offy = 1.5, width = 6, height = 5,vector.graphic = FALSE,editable = FALSE)

      flex_tbl <- vanilla.table(all_tables[[i]])
      flex_tbl <- setZebraStyle(flex_tbl, odd = '#eeeeee', even = 'white')
      flex_tbl[,] <- textProperties( font.style="italic", font.size = 13)

      len_tbl <- ncol(all_tables[[i]])
      flex_tbl <- setFlexTableWidths( flex_tbl, widths = rep(4/len_tbl,len_tbl))
      doc <- addFlexTable(doc, flex_tbl, offx = 8,
                          offy = 1.5, width = 6, height = 4)

    }


  }

  if(n_plots_per_slide == 2) {

    iter <- 1
    for (i in seq(1,length(all_plots),2)) {

      doc <- addSlide(doc, "Two Content")
      title <- paste(section_label,"Slide ", iter, "of", ceiling(length(all_plots)/2))
      doc <- addTitle(doc, title, level = 6)


      doc <- addPlot(doc, fun = print, x = all_plots[[i]],
                     offx = 1, offy = 1.5, width = 5.5, height = 3.5,vector.graphic = FALSE,editable = FALSE)
      if (i != length(all_plots)) {
        doc <- addPlot(doc, fun = print, x = all_plots[[i+1]],
                       offx = 7, offy = 1.5, width = 5.5, height = 3.5,vector.graphic = FALSE,editable = FALSE)

      }


      flex_tbl <- vanilla.table(all_tables[[i]])
      flex_tbl <- setZebraStyle(flex_tbl, odd = '#eeeeee', even = 'white')
      flex_tbl[,] <- textProperties( font.style="italic", font.size = 13)

      len_tbl <- ncol(all_tables[[i]])
      flex_tbl <- setFlexTableWidths( flex_tbl, widths = rep(4/len_tbl,len_tbl))
      doc <- addFlexTable(doc, flex_tbl, offx = 2,
                          offy = 5.5, width = 10, height = 4)


      if (i != length(all_plots)) {

        flex_tbl <- vanilla.table(all_tables[[i+1]])
        flex_tbl <- setZebraStyle(flex_tbl, odd = '#eeeeee', even = 'white')
        flex_tbl[,] <- textProperties( font.style="italic", font.size = 14)

        len_tbl <- ncol(all_tables[[i+1]])
        flex_tbl <- setFlexTableWidths( flex_tbl, widths = rep(4/len_tbl,len_tbl))
        doc <- addFlexTable(doc, flex_tbl, offx = 8,
                            offy = 5.5, width = 10, height = 4)

      }
      iter <- iter + 1


    }

  }
  return(doc)

}

input_validation <- function(df, target_var,
                             n_plots_per_slide, group_names,
                             plot_theme) {


  if(!(target_var %in% colnames(df))  ) {
      stop("Target variable not present in dataframe")
  }

  if (!is.null(group_names) & any(sapply(group_names, function(x) x %in% colnames(df)) == F)) {
    stop("One or more of the Grouping variables not present in the dataframe")
  }

  if (!(n_plots_per_slide %in% c("1","2"))) {
    stop("n_plots_per_slide should be set to either '1' or '2'")
  }

  if (!(plot_theme %in% as.character(unlist(themes)))) {
    stop("plot_theme not available. Please launch the app to view the list of valid plot themes")
  }

}
