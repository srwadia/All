rankall = function(outcome, num = "best"){
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid

  count_outcome = valid_outcome(outcome)
  if(count_outcome == 0){
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  data_HA = data[order(data$State, as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), data$Hospital.Name),]
  data_HF = data[order(data$State, as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), data$Hospital.Name),]
  data_Pneumo = data[order(data$State, as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), data$Hospital.Name),]
  data_HA[data_HA == "Not Available"] = NA
  data_HF[data_HF == "Not Available"] = NA
  data_Pneumo[data_Pneumo == "Not Available"] = NA
  data_HA = data_HA[!is.na(data_HA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
  data_HF = data_HF[!is.na(data_HF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
  data_Pneumo = data_Pneumo[!is.na(data_Pneumo$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
  
  state_unique = as.vector(data$State)
  state_unique = unique(state_unique)
  state_unique = sort(state_unique)
 
  
  hosp_state = character()
  rel_df = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(rel_df) = c("hospital","state")
  
  if(outcome == "heart attack"){
    if(num == "best"){
      for(i in 1:length(state_unique)){
        subset_hosp = data_HA[which(data_HA$State == state_unique[i]),names(data_HA) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[1])
      }
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    
    if(num == "worst"){
      for(i in 1:length(state_unique)){
        subset_hosp = data_HA[which(data_HA$State == state_unique[i]),names(data_HA) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[length(subset_hosp)])
      }
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    
    else{
      for(i in 1:length(state_unique)){
        subset_hosp = data_HA[which(data_HA$State == state_unique[i]),names(data_HA) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[num])
      }
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    return(rel_df)
  }
  
  if(outcome == "heart failure"){
    if(num == "best"){
      for(i in 1:length(state_unique)){
        subset_hosp = data_HF[which(data_HF$State == state_unique[i]),names(data_HF) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[1])
      }
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    if(num == "worst"){
      for(i in 1:length(state_unique)){
        subset_hosp = data_HF[which(data_HF$State == state_unique[i]),names(data_HF) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[length(subset_hosp)])
      }
      
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    else{
      for(i in 1:length(state_unique)){
        subset_hosp = data_HF[which(data_HF$State == state_unique[i]),names(data_HF) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[num])
      }
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    return(rel_df)
  }
  
  if(outcome == "pneumonia"){
    if(num == "best"){
      for(i in 1:length(state_unique)){
        subset_hosp = data_Pneumo[which(data_Pneumo$State == state_unique[i]),names(data_Pneumo) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[1])
      }
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    if(num == "worst"){
      for(i in 1:length(state_unique)){
        subset_hosp = data_Pneumo[which(data_Pneumo$State == state_unique[i]),names(data_Pneumo) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[length(subset_hosp)])
      }
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    
    else{
      for(i in 1:length(state_unique)){
        subset_hosp = data_Pneumo[which(data_Pneumo$State == state_unique[i]),names(data_Pneumo) %in% "Hospital.Name"]
        hosp_state = append(hosp_state, subset_hosp[num])
      }
      for(i in 1:length(state_unique)){
        hosp = hosp_state[i]
        rel_df[nrow(rel_df)+1,] = c(hosp_state[i], state_unique[i])
      }
    }
    return(rel_df)
  }
}