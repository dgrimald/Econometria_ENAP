gen_acesso_digital <-
function(local, status.renda){
  
  prob <- case_when(status.renda==1 & local=="urbano"~ 0.4,
                    status.renda==1 & local=="rural"~ 0.2,
                    status.renda==0 & local=="urbano"~ 0.75,
                    status.renda==0 & local=="rural"~ 0.45,
                    TRUE ~ 0.8)
}
