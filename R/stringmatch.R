#' Generate a string distance score for all combinations of two vectors
#'
#' @param string1 A character vector
#' @param string2 A character vector
#' @param trainingset If NULL, use the base model. Otherwise, a data frame with columns A, B, and y corresponding to string1, string2, and a training label
#' @param feature.importances If TRUE, returns feature importances.
#' @return A data frame of rows length(string1) * length(string2) with columns for each string distance metric as well as a model-produced score
#' @examples
#' stringmatch(string1 = c("hello", "goodbye"),
#'              string2 = c("alpha", "beta"))



stringmatch <-
  function(string1, string2,
           trainingset = NULL,
           feature.importances = FALSE){
    if(!is.null(trainingset)){
      ## set some warnings for type: make sure the columns are A,B,y
      if(!c("A", "B", "y") %in% colnames(trainingset)){
        stop("The columns of trainingset must include 'A', 'B', and 'y'")
      }
      
      ## Calculate stringdist for new training set
      newtrain = get_features(trainingset$A, trainingset$B)
      ## Append the new trainingset to the old one and generate the model
      train = rbind(train, newtrain)
      m = ranger::ranger(x = train %>% select(-y),
                       y = factor(train$y),
                       interaction.depth=2,
                        importance = "impurity")
      
    } 
    
    ## Calculate stringdist for string1, string2
    feats = get_features(string1, string2)
    
    ## Run the prediction
    preds = predict(m, data = feats)
    
    ## Return feature importances too
    if(feature.importances){
      out = list(preds$predictions, ranger::importance(m))
    } else {
      out = list(preds$predictions)
    }
    
    ## Return a data set of predictions
    return(out)
    
  }

