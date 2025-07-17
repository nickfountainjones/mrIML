#' An example mrIML model fit to [MRFcov::Bird.parasites]
#' 
#' ```
#' data <- MRFcov::Bird.parasites
#' Y <- data %>%
#'   dplyr::select(-scale.prop.zos) %>%
#'   dplyr::select(order(everything()))
#' X <- data %>%
#'   dplyr::select(scale.prop.zos)
#' 
#' model_lm <- logistic_reg() %>%
#'   set_engine("glm")
#' 
#' mrIML_bird_parasites_LM <- mrIMLpredicts(
#'   X = X,
#'   Y = Y,
#'   X1 = Y,
#'   Model = model_lm,
#'   prop = 0.7,
#'   k = 2,
#'   racing = FALSE
#' )
#' ```

"mrIML_bird_parasites_LM"