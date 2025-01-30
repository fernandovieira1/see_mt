#################### 4. PREPARAÇÃO DOS MODELOS ####################
glimpse(base_final)
summary(base_final)

## MELHORIA 1. Imputação dos Valores Ausentes ####
# Carregar pacotes necessários
library(dplyr)
library(tidyr)
library(mice)

# Função para imputação condicional dentro do município e ciclo escolar
imputar_media_condicional <- function(df, var) {
  df <- df %>%
    group_by(ibge_id_q, ciclo_id_q) %>%
    mutate(!!sym(var) := ifelse(is.na(!!sym(var)), mean(!!sym(var), na.rm = TRUE), !!sym(var))) %>%
    ungroup()
  return(df)
}

# Aplicando imputação condicional para notas
base_final <- imputar_media_condicional(base_final, "nota_mt_q")
base_final <- imputar_media_condicional(base_final, "nota_lp_q")

# Criando um modelo de regressão para imputação em notas (caso ainda tenha NAs)
modelo_mt <- lm(nota_mt_q ~ ideb_q + idh_educacao + idh_renda + infra + abandono_q, data = base_final, na.action = na.exclude)
modelo_lp <- lm(nota_lp_q ~ ideb_q + idh_educacao + idh_renda + infra + abandono_q, data = base_final, na.action = na.exclude)

# Preenchendo os valores ausentes usando predição do modelo
base_final$nota_mt_q[is.na(base_final$nota_mt_q)] <- predict(modelo_mt, newdata = base_final[is.na(base_final$nota_mt_q), ])
base_final$nota_lp_q[is.na(base_final$nota_lp_q)] <- predict(modelo_lp, newdata = base_final[is.na(base_final$nota_lp_q), ])

# Criando uma função para imputação de IDEB, abandono e aprovação com média móvel
imputar_media_movel <- function(df, var) {
  df <- df %>%
    arrange(ibge_id_q, ano_q) %>%
    group_by(ibge_id_q) %>%
    mutate(!!sym(var) := zoo::na.locf(!!sym(var), na.rm = FALSE)) %>%
    ungroup()
  return(df)
}

# Aplicando imputação para IDEB, abandono e aprovação
base_final <- imputar_media_movel(base_final, "ideb_q")
base_final <- imputar_media_movel(base_final, "abandono_q")
base_final <- imputar_media_movel(base_final, "aprovacao_q")

# Salvar a base final imputada
# write.csv(base_final, "base_final_imputada.csv", row.names = FALSE)

# Verificando se ainda há NAs
sum(is.na(base_final))

# Passo 1: Substituir valores ausentes em notas com a média global
base_final <- base_final %>%
  mutate(nota_mt_q = ifelse(is.na(nota_mt_q), mean(nota_mt_q, na.rm = TRUE), nota_mt_q),
         nota_lp_q = ifelse(is.na(nota_lp_q), mean(nota_lp_q, na.rm = TRUE), nota_lp_q))

# Passo 2: Imputação do IDEB, abandono e aprovação usando a mediana do município
imputar_mediana_municipal <- function(df, var) {
  df <- df %>%
    group_by(ibge_id_q) %>%
    mutate(!!sym(var) := ifelse(is.na(!!sym(var)), median(!!sym(var), na.rm = TRUE), !!sym(var))) %>%
    ungroup()
  return(df)
}

# Aplicar a imputação por mediana para IDEB, abandono e aprovação
base_final <- imputar_mediana_municipal(base_final, "ideb_q")
base_final <- imputar_mediana_municipal(base_final, "abandono_q")
base_final <- imputar_mediana_municipal(base_final, "aprovacao_q")

# Passo 3: Se ainda houver NAs, substituir pela média geral
base_final <- base_final %>%
  mutate(ideb_q = ifelse(is.na(ideb_q), mean(ideb_q, na.rm = TRUE), ideb_q),
         abandono_q = ifelse(is.na(abandono_q), mean(abandono_q, na.rm = TRUE), abandono_q),
         aprovacao_q = ifelse(is.na(aprovacao_q), mean(aprovacao_q, na.rm = TRUE), aprovacao_q))

# Passo 4: Verificar se ainda existem NAs
sum(is.na(base_final))

## MELHORIA 2. Teste de Multicolinearidade ####
# Carregar pacotes necessários
library(dplyr)
library(car)  # Para calcular o VIF

# Criar um subconjunto apenas com os índices do IDH
idh_vars <- base_final %>%
  select(idh_municipal, idh_renda, idh_longevidade, idh_educacao)

# Passo 1: Matriz de correlação
cor_matrix <- cor(idh_vars, use = "complete.obs")
print("Matriz de Correlação entre os Subíndices do IDH:")
print(cor_matrix)

# Passo 2: Calcular o VIF para testar multicolinearidade
vif_values <- vif(lm(idh_municipal ~ idh_renda + idh_longevidade + idh_educacao, data = idh_vars))
print("Variance Inflation Factor (VIF) para os Subíndices do IDH:")
print(vif_values)

# Passo 3: Interpretação
# Se alguma variável tiver VIF > 10, recomendamos sua remoção
if (any(vif_values > 10)) {
  print("Atenção! Algumas variáveis apresentam alta multicolinearidade (VIF > 10). Considere removê-las.")
} else if (any(vif_values > 5)) {
  print("Há moderada multicolinearidade (VIF entre 5 e 10). Avaliar necessidade de ajustes.")
} else {
  print("Não há problemas significativos de multicolinearidade (VIF < 5).")
}

## MELHORIA 3. Criar a Variável de Interação ####

# Criar a variável de interação
base_final <- base_final %>%
  mutate(see_idh_educacao = see * idh_educacao)

# Verificar a distribuição da nova variável
summary(base_final$see_idh_educacao)

# Testar correlação entre a nova variável e as já existentes para evitar redundância
cor_matrix <- cor(base_final %>% select(see, idh_educacao, see_idh_educacao), use = "complete.obs")
print("Matriz de Correlação entre SEE, IDH_Educacao e SEE_IDH_Educacao:")
print(cor_matrix)

# Verificar se a variável foi criada corretamente e está dentro da base
glimpse(base_final)



