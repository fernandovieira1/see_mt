########################### MODELO MATEMÁTICA 1: DiD com EF ########################### 
## Modelo matemática 1: Diferenças em Diferenças com Efeitos Fixos
# Resultados em Matemática

#### 4.1 Diferenças em Diferenças (DiD) ####
cat('\014')  # Limpar a tela do console

print("####################### RESULTADOS DIFERENÇAS EM DIFERENÇAS - MATEMÁTICA #######################")

# Carregar pacotes
library(fixest)  # Para regressões com efeitos fixos
library(lmtest)  # Para testes estatísticos
library(broom)   # Para manipular outputs de modelos
library(plm)     # Para análise em painel
library(dplyr)   # Para manipulação de dados
library(ggplot2) # Para visualizações

# Verificar estrutura dos dados
glimpse(base_final)

# Converter `ano_q` para numérico, garantindo que valores são preservados
base_final <- base_final %>%
  mutate(ano_q = as.numeric(as.character(ano_q))) 

# Criando uma variável de tempo (1 para anos pós-SEE, 0 para pré-SEE)
base_final <- base_final %>%
  mutate(post = ifelse(ano_q >= 2022, 1, 0))

# Criando a interação tratamento x tempo
base_final <- base_final %>%
  mutate(trat_post = see * post)

## **Verificando colinearidade**
# Verificar correlação entre as variáveis
cor_matrix <- cor(base_final %>% select(post, see, infra, idh_educacao), use = "complete.obs")
print(cor_matrix)

# Rodar uma regressão auxiliar para identificar colinearidade
aux_model <- lm(post ~ see + infra + idh_educacao, data = base_final)
summary(aux_model)

# Verificar a distribuição da variável `post`
table(base_final$post)

# **Testando se `post` é removida por colinearidade**
# Rodar um modelo simples sem efeitos fixos para checar se `post` ainda é relevante
did_lm <- lm(nota_mt_q ~ see + post + trat_post + infra + idh_educacao, data = base_final)
summary(did_lm)

## **Rodando o modelo Diferenças em Diferenças (DiD)**
# Modelo DiD com efeitos fixos (municipal e ano)
did_model <- feols(nota_mt_q ~ see + trat_post + infra + idh_educacao | ibge_id_q + ano_q, data = base_final)

# Verificar se variáveis foram removidas por colinearidade
if (!is.null(did_model$collin.var)) {
  print("Variáveis removidas por colinearidade no modelo principal:")
  print(did_model$collin.var)
}

# **Modelo alternativo: efeitos fixos interagidos**
did_model_interacted <- feols(nota_mt_q ~ see + trat_post + infra + idh_educacao | ibge_id_q, 
                              data = base_final)

# Verificar se variáveis foram removidas por colinearidade
if (!is.null(did_model_interacted$collin.var)) {
  print("Variáveis removidas por colinearidade no modelo interagido:")
  print(did_model_interacted$collin.var)
}

summary(did_model_interacted)

# **Teste de placebo: Criar um `post_fake` antes do SEE**
base_final <- base_final %>%
  mutate(post_fake = ifelse(ano_q == 2019, 1, 0),
         trat_post_fake = see * post_fake)

did_placebo <- feols(nota_mt_q ~ see + trat_post_fake + infra + idh_educacao | ibge_id_q + ano_q, 
                     data = base_final)

# Verificar se variáveis foram removidas por colinearidade no teste de placebo
if (!is.null(did_placebo$collin.var)) {
  print("Variáveis removidas por colinearidade no teste de placebo:")
  print(did_placebo$collin.var)
}

summary(did_placebo)

# **Visualização dos efeitos antes e depois da implementação**
ggplot(base_final, aes(x = ano_q, y = nota_mt_q, color = factor(see))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Evolução das Notas de Matemática (SEE vs Controle)",
       x = "Ano", y = "Nota Média de Matemática",
       color = "SEE") +
  theme_minimal()

# **Resumo final dos resultados**
summary(did_model)
summary(did_model_interacted)
summary(did_placebo)
