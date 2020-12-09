select_df = function(state){
  rel_df = data.frame(matrix(nrow = 0, ncol = 46))
  for(i in 1:length(state_list)){
    if(state == state_list[i]){
      rel_df[nrow(rel_df)+1,] = data[i,]
    }
  }
  rel_df
}