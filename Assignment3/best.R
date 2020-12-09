best = function(state, outcome){
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  rel_df = select_df(state) # returning the relevant dataframe for one state
  if(outcome == "heart attack"){
    HA_list = rel_df[,11]
    HA_list = as.numeric(HA_list)
    HA_list = as.vector(HA_list)
    best_vector = which(HA_list == min(HA_list, na.rm = TRUE))
  }
  
  if(outcome == "heart failure"){
    HF_list = rel_df[,17]
    HF_list = as.numeric(HF_list)
    HF_list = as.vector(HF_list)
    best_vector = which(HF_list == min(HF_list, na.rm = TRUE))
  }
  
  if(outcome == "pneumonia"){
    Pneumo_list = rel_df[,23]
    Pneumo_list = as.numeric(Pneumo_list)
    Pneumo_list = as.vector(Pneumo_list)
    best_vector = which(Pneumo_list == min(Pneumo_list, na.rm = TRUE))
  }

  names = sort(rel_df$X2[best_vector]) # alphabetically sorted
  return(names[1])
}
