new_df = function(state, outcome_list){
  rel_df = select_df(state)
  for(i in 1:length(outcome_list)){
    if(is.na(outcome_list[i])){
      rel_df = rel_df[-i,]
    }
  }
  rel_df
}