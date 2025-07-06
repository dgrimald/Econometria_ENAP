dgp <- function(n.id=4000, delta_universidade=4000, delta_publica=-1000, peso_alpha=1){
  
  # cria estrutura da base
  set.seed(13)
  sample.data <- data.frame(id=unlist(lapply(1:n.id, rep, 2)), t=rep(0:1, n.id))
  n.sample <- nrow(sample.data)
  
  # cria habilidade intrínseca (não observável) do individuo
  alpha_i = rnorm(n.id, 0, 4)
  alpha_i.vector <- lapply(alpha_i, rep, 2) %>% unlist()
  
  # cria efeito aleatório de periodos
  alpha_0 <- rnorm(1, 1000, 1)
  alpha_1 <- rnorm(1, 1100, 1)

  # consolida base sobre alphas
  sample.data %<>% 
    mutate(alpha_t = case_when(t==0 ~ alpha_0,
                               t==1 ~ alpha_1),
           alpha_id = alpha_i.vector)
  
  # cria probabilidade de concluir 1 ano de estudo (depende de habilidade não observável)
  p_estudar <- 1/(1+exp(1-alpha_i))
  p_estudar.vector <- lapply(p_estudar, rep, 2) %>% unlist()
  
  # cria x_1: anos de estudo (0 a 9 anos) em t0 e D_u: dummy de acesso a universidade em t0
  x_1.t0.data <- data.frame(id = 1:n.id,
                            t = 0,
                            D_u = 0,
                            x_1 = lapply(p_estudar, rbinom, n=1, size=9) %>% unlist())
  
  # cria x_1: anos de estudo e D_u: dummy acesso a universidade em t1
  # (todos que concluem ensino médio, fazem universidade)
  x_1.t1.data <- x_1.t0.data %>% 
    mutate(t = 1,
           D_u = case_when(x_1==9 ~ 1,
                           TRUE ~ 0),
           x_1 = 4*D_u+x_1)
  
  # consolida base sobre anos de estudo
  sample.data %<>%
    left_join(rbind(x_1.t0.data, x_1.t1.data), by=c("id", "t")) %>% 
    mutate(p_estudar = p_estudar.vector)

  # Implementa processo seletivo para universidade pública (vestibular)
  score_vestibular <- peso_alpha * scale(alpha_i) + (1-peso_alpha)*scale(rnorm(length(alpha_i)))
  score_vestibular.vector <- lapply(score_vestibular, rep, 2) %>% unlist()
  
  # cria D_p: dummy de acesso a universidade pública
  sample.data %<>% 
    mutate(score_vestibular = score_vestibular.vector) %>% 
    group_by(D_u) %>% 
    mutate(cut.off = median(score_vestibular)) %>% 
    ungroup() %>% 
    mutate(D_p = (t==1)*(D_u)*(score_vestibular > cut.off))

  # cria Y: renda familiar mensal
  sample.data %<>%
    mutate(e = rnorm(n=n.sample, sd=1),
           y = 5000 + 5500*(alpha_id+abs(min(alpha_i))) + alpha_t + delta_universidade*D_u + delta_publica*D_p + e)
  
  sample.data %<>%
    select(-p_estudar, -score_vestibular, -e, -alpha_t)
}