valid_outcome = function(outcome){
  acceptable_outcomes = c("heart attack","heart failure", "pneumonia")
  count_outcome = 0
  for(i in 1:length(acceptable_outcomes)){
    if(outcome == acceptable_outcomes[i]){
      count_outcome = count_outcome + 1
    }
  }
  count_outcome
}