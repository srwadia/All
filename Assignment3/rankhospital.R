rankhospital = function(state, outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  state_list = data[,7]
  acceptable_outcomes = c("heart attack","heart failure", "pneumonia")
  count_state = valid_state(state)
  if(count_state == 0){
    stop("invalid state")
  }
  count_outcome = valid_outcome(outcome)
  if(count_outcome == 0){
    stop("invalid outcome")
  }
  
  
  ## Return hospital name in that state with the given rank 30-day death rate
  rel_df = select_df(state)
  names = rel_df$X2 
  
  rel_df_HA = rel_df[order(as.numeric(rel_df$X11),rel_df$X2),]
  rel_df_HA[rel_df_HA == "Not Available"] = NA
  rel_df_HA = rel_df_HA[!is.na(rel_df_HA$X11),]

  
  rel_df_HF = rel_df[order(as.numeric(rel_df$X17),rel_df$X2),]
  rel_df_HF[rel_df_HF == "Not Available"] = NA
  rel_df_HF = rel_df_HF[!is.na(rel_df_HF$X17),]
  
  rel_df_Pneumo = rel_df[order(as.numeric(rel_df$X23),rel_df$X2),]
  rel_df_Pneumo[rel_df_Pneumo == "Not Available"] = NA
  rel_df_Pneumo = rel_df_Pneumo[!is.na(rel_df_Pneumo$X23),]
  
  ## ^ All the above generate the right data frames...
  
  if(outcome == "heart attack"){
    if(num == "best"){
      return(rel_df_HA$X2[1])
    }
    if(num == "worst"){
      count = nrow(rel_df_HA)
      return(rel_df_HA$X2[count])
    }
    return(rel_df_HA$X2[num])
  }
  
  if(outcome == "heart failure"){
    if(num == "best"){
      return(rel_df_HF$X2[1])
    }
    if(num == "worst"){
      count = nrow(rel_df_HA)
      return(rel_df_HF$X2[count])
    }
    return(rel_df_HF$X2[num])
  }
  
  if(outcome == "pneumonia"){
    if(num == "best"){
      return(rel_df_Pneumo$X2[1])
    }
    if(num == "worst"){
      count = nrow(rel_df_HA)
      return(rel_df_Pneumo$X2[count])
    }
    return(rel_df_Pneumo$X2[num])
  }
} 
  