pgd <-
function(n.id=5570, n.t=6, beta=c(0.5, 0.1), delta=5, mu_x=c(7, 40), sigma_x=c(0.5, 10), mu_alpha=0, sigma_alpha=2, theta_1=0, theta_2=0, zeta=1){

  # Define número de obs
  n.obs=n.id*n.t
  
  # Criando variáveis primárias
  ## Criando identificadores de individuos e períodos de tempo
  pgd <- data.frame("id"=c(1:n.id),
                    "t"=rep(c(1:n.t), each=n.id))
  
  ## Criando Matriz de Covariância associada a X
  var.cov.x <- diag(sigma_x)
  
  ## Criando a matriz X_i
  X_i <- rmvnorm(n=n.id, mean=mu_x, sigma=var.cov.x) %>% 
    as.data.frame()
  names(X_i) <- gsub("V", "x", names(X_i))
  
  ## Criando a variável alpha_i 
  alpha <- data.frame("id"=c(1:n.id),
                      alpha=rnorm(n=n.id, mu_alpha,sigma_alpha))
  
  ## Criando a variável gamma_0 
  gamma_0 <- rnorm(1, 0, 2)    
  
  ## Criando a variável v1_it
  v1_it <- data.frame("v1"=rnorm(n.obs, mean=0, sd=1))
  
  ## Criando a variável v2_it
  v2_it <- data.frame("v2"=rnorm(n.obs, mean=0, sd=5))
  
  ## Criando a variável v3_t
  v3_t <- data.frame(t=1:n.t,
                     "v3"=rnorm(n.t, mean=0, sd=1))    
  
  ## Criando a variável epsilon_it
  e_it <- data.frame("e"=rnorm(n.obs, mean=0, sd=1))
  
  ## consolidando informação de variáveis primárias dentro do pgd
  pgd %<>%
    cbind(X_i) %>%
    left_join(alpha, by="id") %>% 
    cbind(v1_it) %>%
    cbind(v2_it) %>% 
    left_join(v3_t, by="t") %>%     
    cbind(e_it) %>% 
    mutate(x1 = x1+v1,
           x2 = x2+v2,
           alpha = alpha * (1+theta_1)^t,
           gamma = gamma_0 * (1+theta_2)^t + v3)

  ## criando s_it e P
  pgd %<>%
    group_by(id) %>% 
    mutate(s = 1/(1 + exp(mean(0 + zeta*(alpha-mu_alpha)))),
           D = rbinom(1, 1, s)) %>%
    ungroup() %>% 
    relocate(s, D, .after=alpha) %>% 
    mutate(pos=case_when(t<=n.t/2 ~ 0, 
                         TRUE ~ 1),
           P = D * pos) %>% 
    relocate(P, .after=alpha)
  
  #Criando y_it
  pgd %<>%
    mutate(y = alpha + beta[1]*x1 + beta[2]*x2 + delta*P + e,
           y = y + abs(min(y))) %>% 
    relocate(y, .after=t)
  
 pgd
}
