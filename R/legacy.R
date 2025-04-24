#' Compare and benchmark disease outbreak risk among and within groups
#'
#' @param data A \code{character} object name of data frame
#' @param Y A \code{character} column name of variable containing outcome
#' @param pred A \code{character} column name of variable containing model
#' predicted values
#' @param group A \code{character} column name of variable that individuals
#' should be grouped by
#' @param type A \code{character} specify within group "internal" or among group
#' "external" benchmarking
#' @param label_by A \code{character} column name of variable representing the
#' individual units. If stated, these will be labeled on the ggplot. By default
#' labels will not be included
mrBenchmark <- function(data = "data",
                        Y = "class",
                        pred = "predicted",
                        group = "group1",
                        label_by = "ID",
                        type = "internal") {
  # create objects from character strings
  data1 <- as.data.frame(eval(parse(text = data)))
  outcome1 <- as.factor(eval(parse(text = paste("data1$", Y, sep = ""))))
  pred1 <- eval(parse(text = paste("data1$", pred, sep = "")))
  group1 <- as.factor(eval(parse(text = paste("data1$", group, sep = ""))))

  c <- discretize(
    as.numeric(pred1),
    cuts = 3,
    labels = c("Low risk", "Medium risk", "High risk"),
    keep_na = FALSE,
    infs = FALSE
  )

  ## among group benchmarking
  if (type == "external") {
    print(ggplot(data = data1, aes(x = "", y = pred1, color = as.factor(outcome1))) +
      geom_boxplot(outlier.size = -1, lwd = 1.3) +
      facet_grid(as.formula(paste(".~", group))) + # facetting variable
      scale_y_continuous(limits = c(0, 1)) +
      ylab(pred) +
      geom_hline(aes(lty = "High risk", yintercept = c$breaks[3])) +
      geom_hline(aes(lty = "Low risk", yintercept = c$breaks[2])) +
      scale_linetype_manual(name = "Risk level", values = c(1, 2)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(
        text = element_text(size = 17, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_color_uchicago(palette = "dark", name = Y))
  }

  if (type == "internal") {
    if (is.null(label_by) == FALSE) {
      label1 <- as.factor(eval(parse(text = paste("data1$", label_by, sep = ""))))

      # determine number of groups
      n <- levels(group1)

      # plot per group
      for (i in n) {
        # only consider data from specified group
        data2 <- data1 %>%
          filter(group1 == i)

        # create more objects
        outcome2 <- as.factor(eval(parse(text = paste("data2$", Y, sep = ""))))
        pred2 <- eval(parse(text = paste("data2$", pred, sep = "")))
        label2 <- as.factor(eval(parse(text = paste("data2$", label_by, sep = ""))))

        # internal plots
        print(ggplot(data2, aes(x = outcome2, y = pred2, color = as.factor(outcome2))) +
          geom_point(aes(color = as.factor(outcome2))) +
          xlab(Y) +
          ylab(pred) +
          geom_hline(aes(lty = "High risk", yintercept = c$breaks[3])) +
          geom_hline(aes(lty = "Low risk", yintercept = c$breaks[2])) +
          scale_linetype_manual(name = "Risk level", values = c(1, 2)) +
          ggtitle(i) +
          guides(colour = guide_legend(title = Y)) +
          geom_label_repel(aes(label = label2),
            fontface = "bold",
            force = 10,
            box.padding = unit(0.35, "lines"),
            point.padding = unit(0.5, "lines"),
            arrow = arrow(
              length = unit(0.01, "npc"),
              type = "open", ends = "last"
            ),
            size = 5, max.overlaps = 20, show.legend = F
          ) +
          theme(
            text = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 18, hjust = 0.5),
            axis.line = element_line(size = 0.8, colour = "black"),
            axis.text.x = element_text(colour = "black", size = 18),
            axis.text.y = element_text(colour = "black", size = 18),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            strip.background = element_rect(color = "black", size = 1.5, linetype = "solid")
          ) +
          scale_color_uchicago(palette = "dark") +
          scale_fill_uchicago(palette = "dark"))
      }
    }

    if (is.null(label_by) == TRUE) {
      # determine number of groups
      n <- levels(group1)

      # plot per group
      for (i in n) {
        # only consider data from specified group
        data2 <- data1 %>%
          filter(group1 == i)

        # create more objects
        outcome2 <- as.factor(eval(parse(text = paste("data2$", Y, sep = ""))))
        pred2 <- eval(parse(text = paste("data2$", pred, sep = "")))

        # internal plots
        print(ggplot(data2, aes(x = outcome2, y = pred2, color = as.factor(outcome2))) +
          geom_point(aes(color = as.factor(outcome2))) +
          xlab(Y) +
          ylab(pred) +
          ggtitle(i) +
          geom_hline(aes(lty = "High risk", yintercept = c$breaks[3])) +
          geom_hline(aes(lty = "Low risk", yintercept = c$breaks[2])) +
          scale_linetype_manual(name = "Risk level", values = c(1, 2)) +
          guides(colour = guide_legend(title = Y)) +
          theme(
            text = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 18, hjust = 0.5),
            axis.line = element_line(size = 0.8, colour = "black"),
            axis.text.x = element_text(colour = "black", size = 18),
            axis.text.y = element_text(colour = "black", size = 18),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            strip.background = element_rect(color = "black", size = 1.5, linetype = "solid")
          ) +
          scale_color_uchicago(palette = "dark") +
          scale_fill_uchicago(palette = "dark"))
      }
    }
  }
}

#' Generate SHAP (SHapley Additive exPlanations) Plots for Multiple Models and
#' Responses
#'
#' This function generates SHAP (SHapley Additive exPlanations) plots for
#' multiple models and responses.
#'
#' @param yhats A list of model prediction objects. Each object should contain a
#' model, data, and class information.
#' @param MultRespVars A data frame containing response variables for the
#' prediction.
#' @param taxa An optional vector specifying which responses to include based on
#' their indices.
#' @param x_features A character vector specifying the features to consider in
#' the plots.
#' @param y_features A character vector specifying the response features for
#' interaction plots.
#' @param kind A character string specifying the type of plots (e.g., "beeswarm"
#' for feature effect plot, "bar" for variable importance plot, or "both").
#' @param max_display An integer specifying the maximum number of features to
#' display.
#' @param interactions A logical indicating whether to create interaction effect
#' plots.
#' @param color_var A variable to use for coloring in the dependency plots.
#' @param getFeaturePlot A logical indicating whether to generate feature effect
#' plots.
#' @param getDependencyPlot A logical indicating whether to generate dependency
#' plots.
#' @param getInteractionPlot A logical indicating whether to generate
#' interaction plots.
#' @param num_cores An integer specifying the number of CPU cores to use for
#' parallel processing.
#' @param class_selection An optional vector specifying which classes to include
#' in the plots.
#'
#' @return A ggplot object containing SHAP plots for the specified responses and
#' features. Note that this function may not work for some algorithm classe
#' (e.g., neural nets)
MrShapely <- function(yhats, MultRespVars = Resp,
                      taxa = NULL,
                      kind = "beeswarm",
                      max_display = 15L,
                      color_var = NULL, getFeaturePlot = TRUE,
                      getDependencyPlot = TRUE, get2DDependencyPlot = TRUE,
                      num_cores = 2,
                      class_selection = NULL) {
  # Parallelization
  plan(future::multisession, workers = num_cores)

  # Extract model, data, and response information
  mod_list <- future_lapply(
    yhats,
    function(yhat) yhat$mod1_k,
    future.seed = TRUE
  )
  # Not working w/ future.apply
  model_list <- lapply(mod_list, extract_fit_parsnip)
  Xdata_list <- future_lapply(
    yhats,
    function(yhat) as.data.frame(yhat$data),
    future.seed = TRUE
  )
  X_i_list <- future_lapply(
    yhats,
    function(yhat) as.data.frame(select(yhat$data, -class)),
    future.seed = TRUE
  )
  Y_i_list <- future_lapply(
    yhats,
    function(yhat) yhat$data$class,
    future.seed = TRUE
  )

  # Calculate Shap_kernel and shapobj for all responses using future_lapply
  shapobj_list <- future_lapply(1:length(model_list), function(i) {
    model <- model_list[[i]]
    X_i <- X_i_list[[i]]
    Xdata <- Xdata_list[[i]]
    Y_i <- Y_i_list[[i]]

    # Deals with classification cases
    if (model$spec$mode == "classification") {
      predfun <- function(model, newdata) {
        preds <- predict(
          model,
          as.data.frame(newdata),
          type = "response" # probabilities
        )
        
        cbind(1 - preds, preds) # for both classes
      }

      if (inherits(model$fit, "glm")) {
        Shap_kernel <- kernelshap(model$fit,
          X = X_i,
          pred_fun = predfun,
          bg_X = Xdata
        )
        shapobj <- shapviz(Shap_kernel)
        names(shapobj) <- levels(Y_i)
      } else if (inherits(model$fit, "randomForest") ||
                 inherits(model$fit, "ranger")) {
        Shap_kernel <- kernelshap(model,
          X = X_i,
          bg_X = Xdata,
          type = "prob"
        )
        shapobj <- shapviz(Shap_kernel)
        names(shapobj) <- levels(Y_i)
      } else {
        shapobj <- shapviz(model$fit,
          X_pred = data.matrix(X_i),
          X = Xdata
        )
        names(shapobj) <- levels(Y_i)
      }

      shapobj
    }

    # Deals with regression cases
    if (model$spec$mode == "regression") {
      if (inherits(model$fit, "lm")) {
        Shap_kernel <- kernelshap(model$fit,
          X = X_i,
          bg_X = Xdata
        )
        shapobj <- shapviz(Shap_kernel)
      } else if (inherits(model$fit, "randomForest") ||
                 inherits(model$fit, "ranger")) {
        Shap_kernel <- kernelshap(model,
          X = X_i,
          bg_X = Xdata
        )
        shapobj <- shapviz(Shap_kernel)
      } else {
        shapobj <- shapviz(model$fit,
          X_pred = data.matrix(X_i)
        )
      }

      shapobj
    }
  }, future.seed = TRUE)


  # Subset the results based on the 'taxa' parameter
  if (!is.null(taxa)) {
    if (is.numeric(taxa)) {
      if (length(taxa) == 1) {
        # print("Subset shapobj_list based on 'taxa'.")
        shapobj_list <- list(shapobj_list[[taxa]])
        ResponseNames <- colnames(MultRespVars)[taxa]
      } else if (length(taxa) > 0 && 
                 all(taxa > 0 & taxa <= length(shapobj_list))) {
        # print("Subset shapobj_list based on 'taxa'.")
        shapobj_list <- shapobj_list[taxa]
        ResponseNames <- colnames(MultRespVars)[taxa]
      } else {
        stop("Invalid value for 'taxa'. Please provide valid indices.")
      }
    } else {
      stop("Invalid value for 'taxa'. Please provide numeric indices.")
    }
  } else {
    ResponseNames <- colnames(MultRespVars)
    taxa <- seq_along(shapobj_list)
  }

  # Initialize an empty list to store plots with labels
  plots_with_labels <- list()

  # [1] FEATURE EFFECT PLOT FUNCTION
  if (isTRUE(getFeaturePlot)) {
    if (is.null(taxa)) {
      taxa_to_iterate <- seq_along(shapobj_list)
    } else {
      taxa_to_iterate <- seq_along(shapobj_list)
    }
    s
    feature_plots_with_labels <- future_lapply(taxa_to_iterate, function(i) {
      response_name <- ifelse(is.null(ResponseNames[i]), "", ResponseNames[i])

      if (model_list[[i]]$spec$mode == "regression") {
        label <- response_name
        shapobj <- shapobj_list[[i]]
        plot_obj <- sv_importance(
          shapobj,
          kind = kind,
          max_display = max_display
        ) +
          labs(title = label)
      } else {
        class_list <- shapobj_list[[i]]
        print(
          paste("Index:", i, "Length of shapobj_list:", length(shapobj_list))
        )
        class_feature_plots <- future_lapply(
          names(class_list),
          function(class_name) {
            if (!is.null(class_selection) &&
                !(class_name %in% class_selection)) {
              return(NULL)
            }
  
            class_obj <- class_list[[class_name]]
            label <- ifelse(
              is.null(class_name),
              response_name,
              paste(response_name, class_name, sep = " - ")
            )
            plot_obj <- sv_importance(
              class_obj,
              kind = kind,
              max_display = max_display
            ) + 
              labs(title = label)
            
            plot_obj
          },
          future.seed = TRUE
        )

        class_feature_plots <- class_feature_plots[!future_sapply(class_feature_plots, is.null)]

        if (length(class_feature_plots) > 0) {
          return(class_feature_plots)
        } else {
          return(NULL)
        }
      }
    }, future.seed = TRUE)

    plots_with_labels <- c(plots_with_labels, feature_plots_with_labels)
  }
  # END OF FEATURE EFFECT PLOT FUNCTION

  # [2] DEPENDENCY PLOT FUNCTION
  if (isTRUE(getDependencyPlot)) {
    if (is.null(taxa)) {
      taxa_to_iterate <- seq_along(shapobj_list)
    } else {
      taxa_to_iterate <- seq_along(shapobj_list)
    }

    dependency_plots_with_labels <-
      future_lapply(taxa_to_iterate, function(i) {
        response_name <- ifelse(is.null(ResponseNames[i]), "", ResponseNames[i])

        if (model_list[[i]]$spec$mode == "regression") {
          label <- response_name
          shapobj <- shapobj_list[[i]]
          colnames_shapobj <- colnames(shapobj)
          dependency_plots <- future_lapply(
            colnames_shapobj,
            function(x_feature) {
              sv_dependence(
                shapobj,
                x_feature,
                color_var = NULL
              ) +
                labs(title = label)
          })
          ncol <- length(colnames_shapobj)
          arranged_plots <- do.call(
            gridExtra::grid.arrange,
            c(dependency_plots, ncol = ncol)
          )
          return(arranged_plots)
        } else {
          class_list <- shapobj_list[[i]]
          class_dependency_plots <- future_lapply(
            names(class_list),
            function(class_name) {
            if (!is.null(class_selection) && !(class_name %in% class_selection)) {
              return(NULL)
            }
            class_obj <- class_list[[class_name]]
            label <- ifelse(is.null(class_name), response_name, paste(response_name, class_name, sep = " - "))
            colnames_class_obj <- colnames(class_obj)
            dependency_plots <- future_lapply(colnames_class_obj, function(x_feature) {
              plot_obj <- sv_dependence(class_obj, x_feature, color_var = NULL) + labs(title = label)
              return(plot_obj)
            }, future.seed = TRUE)
            ncol <- length(colnames_class_obj)
            arranged_plots <- do.call(gridExtra::grid.arrange, c(dependency_plots, ncol = ncol))
            return(arranged_plots)
          }, future.seed = TRUE)

          class_dependency_plots <- class_dependency_plots[!future_sapply(class_dependency_plots, is.null)]

          if (length(class_dependency_plots) > 0) {
            return(class_dependency_plots)
          } else {
            return(NULL)
          }
        }
      }, future.seed = TRUE)

    plots_with_labels <- c(plots_with_labels, dependency_plots_with_labels)
  }

  # [3] INTERACTION EFFECT PLOT FUNCTION
  if (isTRUE(get2DDependencyPlot)) {
    if (is.null(taxa)) {
      taxa_to_iterate <- seq_along(shapobj_list)
    } else {
      taxa_to_iterate <- seq_along(shapobj_list)
    }


    interaction_plots_with_labels <-
      future_lapply(taxa_to_iterate, function(i) {
        response_name <- ifelse(is.null(ResponseNames[i]), "", ResponseNames[i])

        if (model_list[[i]]$spec$mode == "regression") {
          label <- response_name
          shapobj <- shapobj_list[[i]]
          colnames_shapobj <- colnames(shapobj)
          interactions <- combn(colnames_shapobj, 2, simplify = FALSE) # Get all combinations of features
          interaction_plots <- lapply(interactions, function(int) {
            x_feature <- int[1]
            y_feature <- int[2]
            interaction_label <- paste(response_name, sep = " - ")
            interaction_plot <- sv_dependence2D(
              shapobj, x_feature,
              y_feature
            ) + labs(title = interaction_label)
            return(interaction_plot)
          })
          return(interaction_plots)
        } else {
          class_list <- shapobj_list[[i]]
          interaction_plots <- list()

          for (class_name in names(class_list)) {
            if (!is.null(class_selection) && !(class_name %in% class_selection)) {
              next
            }
            class_obj <- class_list[[class_name]]
            colnames_class_obj <- colnames(class_obj)
            interactions <- combn(colnames_class_obj, 2, simplify = FALSE) # Get all combinations of features
            interaction_plots_for_feature <- lapply(interactions, function(int) {
              x_feature <- int[1]
              y_feature <- int[2]
              interaction_label <- paste(response_name, class_name, sep = " - ")
              interaction_plot <- sv_dependence2D(class_obj, x_feature, y_feature) + labs(title = interaction_label)
              return(interaction_plot)
            })
            interaction_plots <- c(interaction_plots, interaction_plots_for_feature)
          }

          if (length(interaction_plots) > 0) {
            return(interaction_plots)
          } else {
            return(NULL)
          }
        }
      })

    plots_with_labels <- c(plots_with_labels, interaction_plots_with_labels)
  }

  # Flatten the list
  plots_with_labels <- unlist(plots_with_labels, recursive = FALSE)

  # Remove NULL elements
  plots_with_labels <- plots_with_labels[!sapply(plots_with_labels, is.null)]

  # Stop parallel processing
  future::plan(future::sequential)

  ncol <- max(1, ceiling(sqrt(length(plots_with_labels))))

  # Create the final plot by stacking responses
  final_plot <- do.call(gridExtra::grid.arrange, c(plots_with_labels, ncol = ncol))

  final.plot <- as.ggplot(final_plot)


  if (getFeaturePlot) {
    if (kind == "beeswarm") {
      final.plot <- final.plot +
        labs(title = "Feature Effect Plot") +
        theme(plot.title.position = "plot")
    } else if (kind == "bar") {
      final.plot <- final.plot +
        labs(title = "Feature Importance Plot") +
        theme(plot.title.position = "plot")
    } else if (kind == "both") {
      final.plot <- final.plot +
        labs(title = "Feature Effect and Importance Plot") +
        theme(plot.title.position = "plot")
    }
  }

  if (getDependencyPlot) {
    final.plot <- final.plot +
      labs(title = "Dependency Plot") +
      theme(plot.title.position = "plot")
  }

  if (get2DDependencyPlot) {
    final.plot <- final.plot +
      labs(title = "Interaction Effect Plot") +
      theme(plot.title.position = "plot")
  }

  return(final.plot)
}

#' Run local explanation methods for individual data points
#'
#' @param X A \code{dataframe} data frame of predictor variables
#' @param Model A \code{workflow} workflow object containing the machine learning model
#' @param Y \code{vector} vector containing the outcome for each data instance
mrLocalExplainer <- function(X, Model, Y){
  
  #create list objects needed later  
  indiv_plots <- list()
  mat1 <- list()
  
  #ensure outcome is stored as a factor
  Y <- as.factor(Y)
  
  #ensure that data is stored as a data.frame only
  dat <- as.data.frame(X)
  
  #create colors needed for each outcome
  num_levels <- length(levels(Y))#number of levels in the outcome factor
  
  my_colors <- pal_uchicago("dark")(9)[1:num_levels] #select same number of colors as there are levels 
  
  #create vector of colors according to outcome level 
  color_levels <- as.numeric(as.factor(Y)) 
  
  for (i in 1:num_levels) {
    color_levels[which(color_levels == i)] <- my_colors[i]
  }
  
  #define prediction function
  predict.function = function(model, new_observation) {
    predict(model, new_observation, "prob")[,2]
  }
  
  #pull the model fit from a workflow object
  model1 <- pull_workflow_fit(Model[[1]]$mod1_k)
  
  #create explainer object
  model_explained <- explain.default(model1$fit, dat, predict.function = predict.function)
  
  #determine number of individuals
  n <- length(dat[,1])
  
  #number of features
  n_feat <- ncol(dat)
  
  #breakDown model and individual plots
  for (i in 1:n) {
    
    #calculate contribution values 
    f1 = broken(model = model_explained, 
                new_observation = dat[i,], 
                data = dat, 
                predict.function = model_explained$predict_function, 
                keep_distributions = TRUE)
    
    #create data frame of variables and relative contribution values 
    data1<-data.frame(y = f1$contribution,
                      x = f1$variable) %>%
      filter(!x=="(Intercept)" &!x=="final_prognosis") #remove unnecessary results
    
    #create feature, value and fplot columns
    data1<-data1 %>% mutate(x = paste0(map_chr(str_split(x, "\\+"), 2)))
    data1$feature<-str_split(data1$x,'=', simplify = TRUE)[,1]
    data1$feature<-trimws(data1$feature, which = c("both"))
    data1$svalue<-str_split(data1$x, '=', simplify = TRUE)[,2]
    data1$svalue<-trimws(data1$svalue, which = c("both"))
    data1$feature<-as.character(data1$feature)
    data1$fplot <- paste(data1$feature, "==", data1$svalue)
    
    #create final data frame of contribution results 
    values <- data1$y
    f <- data1$feature
    obs <- data1$svalue
    class <- rep(as.character(Y[i], times = n_feat))
    ex_data <- cbind(class, f, values, obs)
    mat1[[i]] <- ex_data #store in list object
    
    #produce individual waterfall plots
    indiv_plots[[i]] <- ggplot(data = data1, aes(x = reorder(fplot, -y), y = y)) +
      labs(y = "phi")+
      labs(x = "",subtitle = Y[i]) +
      geom_bar(stat = "identity", fill = color_levels[i]) + #color will depend on individuals outcome class
      coord_flip() +
      guides(fill = FALSE)+
      theme(text = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14, lineheight = 0.7), 
            legend.position = "none") + 
      scale_color_uchicago(palette = "dark") +
      scale_fill_uchicago(palette = "dark")
    
  }
  
  #combine list object into a dataframe
  tab1 <- do.call(rbind, mat1[1:n])
  tab1 <- as.data.frame(tab1) 
  tab1$values <- as.numeric(as.character(tab1$values))
  
  tab2 <- tab1 %>% 
    mutate(values_abs = abs(values)) #add absolute values - allows us to observe contributions size regardless of direction
  
  #calculate mean values of variable contributions
  tab3 <- aggregate(tab2[,5], list(tab2$f), mean) #aggregates variables and presents the mean values for numeric columns
  colnames(tab3)[1] <- "f" 
  tab3_ordered <- tab3[order(tab3$x),] #order variables by size
  order_only <- tab3_ordered$f #character vector of variable order
  
  #apply variable order to final data frame
  tab4 <- tab1
  tab4$f <- as.factor(tab4$f)
  tab4$f <- factor(tab4$f, levels = order_only) 
  
  o_lvl<- levels(Y)
  tab4$class <- relevel(as.factor(tab4$class), ref = o_lvl[1])
  
  #summary plot of variable contributions
  print(ggplot(tab4, aes(x = f, y = values, fill = as.factor(class))) + 
          geom_boxplot(position = position_dodge(.9)) + 
          geom_hline(aes(yintercept = 0.00), linetype = "dashed") +
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16, lineheight = 0.7),
                text = element_text(size = 18, face = "bold"), 
                legend.position = "bottom") + 
          labs(y = "phi", x = NULL) +
          guides(fill=guide_legend(title="Outcome")) +
          coord_flip() + 
          scale_color_uchicago() + 
          scale_fill_uchicago())
  
  #return the following objects to the global environment
  LE_matrix <<- mat1
  LE_indiv_plots <<- indiv_plots 
  
}

