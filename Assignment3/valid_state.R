valid_state = function(state){
  count_state = 0
  for(i in 1:length(state_list)){
    if(state == state_list[i]){
      count_state = count_state + 1
    }
  }
  count_state
}