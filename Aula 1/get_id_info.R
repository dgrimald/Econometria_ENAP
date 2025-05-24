get_id_info <-
function(temp.id, source=data, s=FALSE){
  temp <- filter(source,id==temp.id) %>%
    select(id, elegivel) %>% 
    mutate(tratado=case_when(elegivel==1 ~ rbinom(1, 1, 0.9),
                             TRUE ~ rbinom(1, 1, 0.1)))
  
  if(s){output <- temp$tratado}else{
    output <- list("id"=temp$id, "status"=temp$elegivel,
                   "tratamento"=temp$tratado)}
  output
}
