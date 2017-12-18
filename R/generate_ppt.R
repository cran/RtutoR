
#' Generate Exploratory Analysis Report
#'
#' @param df Dataframe object for exploratory analysis
#' @param target_var Dependent Variable Name
#' @param output_file_name File Name for the Output presentation. Saves the output in the current working
#' directory if only the file name is provided
#' @param plot_theme Specify a theme for ggplot. Includes themes provided as part of ggplot2 as well as themes
#' available in the ggthemes package. Launch the shiny app to view the valid set of themes available
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
                                              top_k_features = NULL,
                                              f_screen_model = c("chi.squared","information.gain","gain.ratio","symmetrical.uncertainty"),
                                              max_levels_cat_var = 10) {

  # remove variables with more than specified no. of levels
  df = df[,sapply(df, function(x) length(levels(x))) <= max_levels_cat_var]
  drop_cols = colnames(df)[sapply(df, function(x) ifelse("character" %in% class(x) & length(unique(x)) > max_levels_cat_var,T,F))]
  df <- df[,!(colnames(df) %in% drop_cols)]

  if(!is.null(top_k_features)) {
    var_imp <- eval(parse(text = f_screen_model))(substitute(target_var ~ ., list(target_var = as.name(target_var))), df)
    top_k_vars <- cutoff.k(var_imp,top_k_features)
    retain_cols <- c(top_k_vars,target_var)
    df <- df[,colnames(df) %in% retain_cols]
  }

  vars <- colnames(df)

  data_type <- sapply(df,class)
  target_var_type <- data_type[which(names(data_type) == target_var)]

  data_type = data_type[-which(names(data_type) == target_var)]

  numeric_features <- names(data_type)[sapply(data_type,function(x) any(x %in% c("numeric","integer","double")))]
  categorical_features <- names(data_type)[sapply(data_type,function(x) any(x %in% c("factor","character","logical")))]


  # Univariate Analysis
  univariate_hist_plots <- lapply(numeric_features, function(var_name) histogram_func(
    data = df,var_name = var_name, plot_theme = plot_theme))
  names(univariate_hist_plots) <- lapply(numeric_features, function(x) paste("Histogram -",x))

  univar_num_features_summary <- lapply(numeric_features, function(var_name) {
    val <- data.frame(t(fivenum(df[[var_name]])))
    val[] = lapply(val, function(x) round(x,2))
    colnames(val) <- c("min","25th","50th","75th","max")
    return(val)

  })

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

  all_plots_univar <- c(univariate_hist_plots,univariate_bar_plots)
  all_tables_univar <- c(univar_num_features_summary,univar_cat_features_summary)


  if (target_var_type == "factor") {

    boxplot_list <- lapply(numeric_features, function(var_name) boxplot_categorical_target(
      data = df,target_var = target_var, var_name = var_name, plot_theme = plot_theme))
    names(boxplot_list) <- lapply(numeric_features, function(x) paste(x, "VS",target_var))

    numeric_features_summary <- lapply(numeric_features, function(var_name) {
      df %>% group_by(!!sym(target_var)) %>% summarise(mean_val = round(mean(!!sym(var_name),na.rm = T),2),
                                                       median_val = round(median(!!sym(var_name),na.rm = T),2))})

    barplot_list <- lapply(categorical_features, function(var_name) barplot_func(
      data = df,target_var = target_var, var_name = var_name, plot_theme = plot_theme))
    names(barplot_list) <- lapply(categorical_features, function(x) paste(x, "VS",target_var))

    categorical_features_summary <- lapply(categorical_features, function(var_name) {
      count_df <- df %>% count(!!sym(target_var),!!sym(var_name)) %>% spread(!!sym(var_name), n,fill =0)
      perct_df <- round(prop.table(as.matrix(count_df[,c(2:ncol(count_df))]),2),4)*100
      perct_df <- as.data.frame(perct_df)
      perct_df[] = lapply(perct_df,function(x) paste0(x,"%"))
      res <- cbind(count_df,perct_df)
      return(res)}
    )

    all_plots <- c(all_plots_univar,boxplot_list,barplot_list)
    all_tables <- c(all_tables_univar, numeric_features_summary,categorical_features_summary)


    res <- list(plots = all_plots,tables = all_tables)
    generate_ppt(res,n_plots_per_slide = n_plots_per_slide, output_file_name = output_file_name)
    return(res)


  } else if (target_var_type %in% c("integer","numeric")) {

    boxplot_list <- lapply(categorical_features, function(var_name) boxplot_numeric_target(
      data = df,target_var = target_var, var_name = var_name, plot_theme = plot_theme))
    names(boxplot_list) <- lapply(categorical_features, function(x) paste(x, "VS",target_var))

    cat_features_summary <- lapply(categorical_features, function(var_name) {
      df %>% group_by(!!sym(var_name)) %>% summarise(mean_val = round(mean(!!sym(target_var),na.rm = T),2),
                                                     median_val = round(median(!!sym(target_var),na.rm = T),2))})


    scatterplot_list <- lapply(numeric_features, function(var_name) scatterplot_func(
      data = df,target_var = target_var, var_name = var_name, plot_theme = plot_theme))
    names(scatterplot_list) <- lapply(numeric_features, function(x) paste(x, "VS",target_var))

    corr_numeric_features <- lapply(numeric_features, function(var_name) {
      cor_val <- round(cor(df[[target_var]],df[[var_name]]),3)
      res <- data.frame(Feature = var_name, Correlation = cor_val)
      return(res)
    })

    all_plots <- c(all_plots_univar, boxplot_list,
                   scatterplot_list)
    all_tables <- c(all_tables_univar, cat_features_summary,corr_numeric_features)


    res <- list(plots = all_plots,tables = all_tables)
    generate_ppt(res,n_plots_per_slide = n_plots_per_slide, output_file_name = output_file_name)
    return(res)

  }
}



# Plotting functions

boxplot_categorical_target = function (data, target_var, var_name, plot_theme) {
  g <- ggplot(data = data, aes_string(x = target_var, y = var_name, fill = target_var)) + geom_boxplot() +
    xlab(target_var)
  g <- g + ggtitle(paste(g$labels$y," vs ", g$labels$x))
  eval(parse(text = paste("g + ",plot_theme)))
}

boxplot_numeric_target = function (data, target_var, var_name, plot_theme) {
  g <- ggplot(data = data, aes_string(x = var_name, y = target_var, fill = var_name)) + geom_boxplot() +
    xlab(var_name)
  g <- g + ggtitle(paste(g$labels$y," vs ", g$labels$x))
  eval(parse(text = paste("g + ",plot_theme)))
}


barplot_func = function (data, target_var, var_name,plot_theme) {
  g <- ggplot(data = data, aes_string(x = target_var, fill = var_name)) + geom_bar(position = "fill") +
    xlab(target_var)
  g <- g + ggtitle(paste(g$labels$y," vs ", g$labels$x))
  eval(parse(text = paste("g + ",plot_theme)))
}

scatterplot_func = function (data, target_var, var_name, plot_theme) {
  g <- ggplot(data = data, aes_string(x = target_var, y = var_name)) + geom_point() + geom_smooth() +
    xlab(target_var)
  g <- g + ggtitle(paste(g$labels$y," vs ", g$labels$x))
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





generate_ppt <- function(plot_table_list,n_plots_per_slide = c("1","2"),
                         output_file_name = "res.pptx") {

  all_plots <- plot_table_list$plots
  all_tables <- plot_table_list$tables

  doc = pptx()

  # Slide 1 : Title slide
  doc <- addSlide(doc, "Title Slide")
  doc <- addTitle(doc,"Exploratory Analysis Report")

  if (n_plots_per_slide == "1") {
    for (i in 1:length(all_plots)) {
      doc <- addSlide(doc, "Two Content")
      title <- paste("Exploratory Analyis: Slide ", i, "of", length(all_plots))
      doc <- addTitle(doc, title, level = 2)

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


  } else if (n_plots_per_slide == "2") {

    iter <- 1
    for (i in seq(1,length(all_plots),2)) {

      doc <- addSlide(doc, "Two Content")
      title <- paste("Exploratory Analyis: Slide ", iter, "of", ceiling(length(all_plots)/2))
      doc <- addTitle(doc, title, level = 2)


      doc <- addPlot(doc, fun = print, x = all_plots[[i]],
                      offx = 1, offy = 1.5, width = 5.5, height = 3.5,vector.graphic = FALSE,editable = FALSE)
      doc <- addPlot(doc, fun = print, x = all_plots[[i+1]],
                      offx = 7, offy = 1.5, width = 5.5, height = 3.5,vector.graphic = FALSE,editable = FALSE)



      flex_tbl <- vanilla.table(all_tables[[i]])
      flex_tbl <- setZebraStyle(flex_tbl, odd = '#eeeeee', even = 'white')
      flex_tbl[,] <- textProperties( font.style="italic", font.size = 13)

      len_tbl <- ncol(all_tables[[i]])
      flex_tbl <- setFlexTableWidths( flex_tbl, widths = rep(4/len_tbl,len_tbl))
      doc <- addFlexTable(doc, flex_tbl, offx = 2,
                          offy = 5.5, width = 10, height = 4)

      flex_tbl <- vanilla.table(all_tables[[i+1]])
      flex_tbl <- setZebraStyle(flex_tbl, odd = '#eeeeee', even = 'white')
      flex_tbl[,] <- textProperties( font.style="italic", font.size = 14)

      len_tbl <- ncol(all_tables[[i+1]])
      flex_tbl <- setFlexTableWidths( flex_tbl, widths = rep(4/len_tbl,len_tbl))
      doc <- addFlexTable(doc, flex_tbl, offx = 8,
                          offy = 5.5, width = 10, height = 4)

      iter <- iter + 1


    }

  }


  writeDoc(doc, file = output_file_name)


}

