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
    data(train)
    data(m)
    if(!is.null(trainingset)){
      ## set some warnings for type: make sure the columns are A,B,y
      if(!all(c("A", "B", "y") %in% colnames(trainingset))){
        stop("The columns of trainingset must include 'A', 'B', and 'y'")
      }
      
      ## Calculate stringdist for new training set
      newtrain = get_features(trainingset$A, trainingset$B)
      colnames(newtrain)[1:2] = c("amicus", "bonica")
      
      ## Add in the truth vector
      hash = paste0(trainingset$A, trainingset$B)
      hash2 = paste0(newtrain$amicus, newtrain$bonica)
      newtrain$match = 0
      newtrain$match[hash2 %in% hash] = 1
      newtrain = newtrain[,colnames(train)]
      
      ## Append the new trainingset to the old one and generate the model
      train = rbind(train, newtrain)
      m = ranger::ranger(x = train %>% select(osa:soundex),
                       y = factor(train$match),
                      probability = TRUE)
      
    } 
    
    ## Calculate stringdist for string1, string2
    feats = get_features(string1, string2)
    feats = feats %>% filter(!is.na(Var1),!is.na(Var2))
    
    ## Run the prediction
    preds = predict(m, data = feats)
    
    feats$pred = preds$predictions[,2]
    
    ## Return feature importances too
    if(feature.importances){
      out = list(feats, ranger::importance(m))
    } else {
      out = list(feats)
    }
    
    ## Return a data set of predictions
    return(out)
    
  }

