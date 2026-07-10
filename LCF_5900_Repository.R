#caso ja não tiver, instala pacote pacman, que agiliza instalação e chamada de outros pacotes
if(!require(pacman))
  install.packages("pacman") 
library(pacman)

# função do pacote pacman que instala/carrega outros pacotes
pacman::p_load(
  tidyverse,
  bestNormalize,
  easyanova,
  ggpubr,
  extrafont,
  devEMF
)

# Limpeza da memória
rm(list=ls(all=TRUE))
gc()

# Importando a fonte
font_import(pattern = "DejaVuSansCondensed")
loadfonts()

# Carregando os dados com um separador específico (se necessário)
raw_url <- "https://raw.githubusercontent.com/jmalonso55/fosfatos/refs/heads/main/Artigo_fosfatos.csv"
dados <- read.csv(raw_url, sep = ",")

# Organizando dados
dados <- dados %>%
  mutate(cult = factor(cult, levels = c("Primeiro", "Segundo", "Acumulado"), ordered = TRUE)) %>% 
  mutate(trat = factor(trat, levels = c("Controle", "Bonito", "Pratápolis", "Catalão", "Arraias", "Registro", "Digestato", "Marrocos", "Argélia", "Bayóvar", "STP", "BoneChar"), ordered = TRUE))

# Função para verificar pressupostos e retornar a ANOVA
pressupostos <- function(y, x) {
  mod <- aov(y ~ x)
  norm <- shapiro.test(mod$residuals)
  hom <- bartlett.test(y ~ x)
  print(norm)
  print(hom)
  
  if (norm$p.value > 0.05 && hom$p.value > 0.05) {
    cat("\nResumo da ANOVA (Pressupostos atendidos):\n")
    print(summary(mod))
  } else {
    cat("Pressupostos não atendidos. Não foi possível realizar a ANOVA.\n")
  }
}

# Função para a média dos grupos
grupos_media <- function(cultivo, variavel, dados, label) {
  dados %>%
    filter(cult == cultivo) %>%
    group_by(trat) %>%
    summarise(!!label := mean(.data[[variavel]], na.rm = TRUE), .groups = "drop")
}

# Função para a eficiência relativa
eficiencia_relativa <- function(x, dat) {
  controle <- dat %>% filter(trat == "Controle") %>% pull(mspa)
  stp <- dat %>% filter(trat == "STP") %>% pull(mspa)
  round((x - controle) * 100 / (stp - controle), 2)
}

# Função para a eficiência no acúmulo de fósforo
eficiencia_fosforo <- function(x, dat) {
  controle <- dat %>% filter(trat == "Controle") %>% pull(ap)
  stp <- dat %>% filter(trat == "STP") %>% pull(ap)
  round((x - controle) * 100 / (stp - controle), 2)
}

# Estabelecendo tema para o gráfico
tema <- theme(panel.background = element_rect(fill = "gray97"),
              panel.grid.major.x = element_line(color = "grey85", linetype = "dotted"),
              panel.grid.major.y = element_line(color = "grey85", linetype = "dotted"),
              strip.background = element_blank(), 
              legend.position = "none",
              axis.title.y = element_text(margin = margin(r = 10)),
              axis.title.x = element_text(margin = margin(t = 10)),
              text = element_text(family = "DejaVu Sans Condensed", size = 26),
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 22))

# Estabelecendo outro tema para o gráfico
tema2 <- theme(text = element_text(family = "DejaVu Sans Condensed", size = 14), 
               axis.text = element_text(family="DejaVu Sans Condensed"),
               panel.grid.major.x = element_line(color = "grey85", linetype = "dotted"),
               panel.background = element_rect(fill = "gray97"),
               strip.background = element_blank(), 
               legend.position = "none",
               axis.title.y = element_text(margin = margin(r = 10)),
               axis.title.x = element_text(margin = margin(t = 10)),
               panel.spacing = unit(2, "lines"),
               strip.text = element_text(size = 14))

# Estabelecendo padrões para gráficos
facet <- c("Primeiro" = "First crop", "Segundo" = "Second crop", "Acumulado" = "Accumulated in two crops")
cores <- c("#9e2943", "#0a6c69", "#75a741", "#d18525", "#d86c67", "#5ab5b2")
eixo_x <- c("Control", "Bonito", "Pratápolis", "Catalão", "Arraias", "Registro", "ERCP", "Morocco", "Algeria", "Bayovar", "TSP", "Bonechar")

# Filtrando os dados
dapri <- dados %>% filter(cult == "Primeiro")

# Análise dos pressupostos
with(dapri, pressupostos(mspa, trat))

# Transformando por log
with(dapri, pressupostos(log(mspa), trat))

# Gerando uma nova tabela para receber os dados transformados
dapri_t <- dapri %>% 
  reframe(trat = trat, mspa = log(mspa))

# Fazendo a análise pelo easyanova
respri <- ea1(dapri_t, 1, alpha = 0.01)

# Resultados do teste de médias
respri$Means

# Filtrando os dados
daseg <- dados %>% filter(cult == "Segundo")

# Análise dos pressupostos
with(daseg, pressupostos(mspa, trat))

# Transformando por log
with(daseg, pressupostos(log(mspa), trat))

# Gerando uma nova tabela para receber os dados transformados
daseg_t <- daseg %>% 
  reframe(trat = trat, mspa = log(mspa))

# Fazendo a análise pelo easyanova
reseg <- ea1(daseg_t, 1, alpha = 0.01)

# Resultados do teste de médias
reseg$Means

# Análise dos pressupostos
dac <- dados %>% filter(cult == "Acumulado")

# Análise dos pressupostos
with(dac, pressupostos(mspa, trat))

# Transformando por log
with(dac, pressupostos(log(mspa), trat))

# Gerando uma nova tabela para receber os dados transformados
dac_t <- dac %>% 
  reframe(trat = trat, mspa = log(mspa))

# Fazendo a análise pelo easyanova
resac <- ea1(dac_t, 1, alpha = 0.01)

# Resultados do teste de médias
resac$Means

# Adicionando resultados do teste para os ciclos

# Primeiro
primeiro <- dados %>%
  filter(cult == "Primeiro") %>% 
  mutate(teste = case_when(
    trat == "BoneChar" ~ "a",
    trat == "STP" ~ "a",
    trat == "Bayóvar" ~ "a",
    trat == "Argélia" ~ "a",
    trat == "Marrocos" ~ "b",
    trat == "Digestato" ~ "b",
    trat == "Registro" ~ "b",
    trat == "Arraias" ~ "b",
    trat == "Catalão" ~ "b",
    trat == "Pratápolis" ~ "c",
    trat == "Bonito" ~ "c",
    trat == "Controle" ~ "c",
    TRUE ~ as.character(trat)
  ))

# Segundo
segundo <- dados %>%
  filter(cult == "Segundo") %>% 
  mutate(teste = case_when(
    trat == "BoneChar" ~ "a",
    trat == "STP" ~ "a",
    trat == "Bayóvar" ~ "b",
    trat == "Argélia" ~ "b",
    trat == "Marrocos" ~ "a",
    trat == "Digestato" ~ "a",
    trat == "Registro" ~ "b",
    trat == "Arraias" ~ "b",
    trat == "Catalão" ~ "b",
    trat == "Pratápolis" ~ "b",
    trat == "Bonito" ~ "b",
    trat == "Controle" ~ "c",
    TRUE ~ as.character(trat)
  ))

# Acumulado
acumulado <- dados %>%
  filter(cult == "Acumulado") %>% 
  mutate(teste = case_when(
    trat == "BoneChar" ~ "a",
    trat == "STP" ~ "a",
    trat == "Bayóvar" ~ "b",
    trat == "Argélia" ~ "b",
    trat == "Marrocos" ~ "c",
    trat == "Digestato" ~ "c",
    trat == "Registro" ~ "c",
    trat == "Arraias" ~ "d",
    trat == "Catalão" ~ "d",
    trat == "Pratápolis" ~ "e",
    trat == "Bonito" ~ "e",
    trat == "Controle" ~ "f",
    TRUE ~ as.character(trat)
  ))

# Gerando a coluna
dados$teste <- c(primeiro$teste, segundo$teste, acumulado$teste)

# Calcular as médias e adicionar os rótulos
dados_resumo <- dados %>% 
  group_by(trat, cult) %>%
  summarise(mean_mspa = mean(mspa), teste = first(teste), .groups = 'drop') %>%
  mutate(label = paste0(sprintf("%.2f", mean_mspa), teste))

# Boxplot para MSPA
box_mspa = ggplot(data = dados, aes(x = trat, y = mspa, color = teste)) +
  geom_boxplot(size = 0.8, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  scale_color_manual(values = cores) +
  scale_y_continuous(breaks = seq(0, 45, by =5)) +
  scale_x_discrete(labels = eixo_x) +
  labs(x = "Treatments", y = expression("Shoot dry mass (g" ~ pot^-1*")")) +
  facet_wrap(~ cult, nrow = 3, ncol = 1, labeller = labeller(cult = facet)) +
  theme_classic() +
  tema

ggsave("box_mspa.png", box_mspa, width = 10, height = 5, dpi = 150)

# Diagrama de pontos para a MSPA
d_mspa = ggplot(data = dados, aes(x = trat, y = mspa, color = teste)) +
  geom_point(alpha = 0.6, size = 5) +
  geom_text(data = dados_resumo, aes(x = trat, y = mean_mspa, label = label), size = 4.5, position = position_nudge(x = 0.45), fontface = "bold") +  
  stat_summary(fun = mean, geom = "point", size = 3, shape = 15) +
  scale_color_manual(values = cores) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_discrete(labels = c(eixo_x), expand = expansion(mult = c(0.05, 0.075))) +
  labs(x = "Treatments", y = expression("Shoot dry mass (g" ~ pot^-1*")")) +
  facet_wrap(~ cult, nrow = 3, ncol = 1, labeller = labeller(cult = facet)) +
  coord_flip() +
  theme_classic() +
  tema

ggsave("d_mspa.png", d_mspa, width = 10, height = 5, dpi = 150)

# Análise dos pressupostos
with(dapri, pressupostos(ap, trat))

# Transformando por log
with(dapri, pressupostos(log(ap), trat))

# Gerando uma nova tabela para receber os dados transformados
dapri_tp <- dapri %>% 
  reframe(trat = trat, ap = log(ap))

# Fazendo a análise pelo easyanova
resprip <- ea1(dapri_tp, 1, alpha = 0.01)

# Resultados do teste de médias
resprip$Means

# Análise dos pressupostos
with(daseg, pressupostos(ap, trat))

# Transformando por log
with(daseg, pressupostos(log(ap), trat))

# Gerando uma nova tabela para receber os dados transformados
daseg_tp <- daseg %>% 
  reframe(trat = trat, ap = log(ap))

# Fazendo a análise pelo easyanova
resegp <- ea1(daseg_tp, 1, alpha = 0.01)

# Resultados do teste de médias
resegp$Means

# Análise dos pressupostos
with(dac, pressupostos(ap, trat))

# Transformando por log
with(dac, pressupostos(log(ap), trat))

# Gerando uma nova tabela para receber os dados transformados
dac_tp <- dac %>% 
  reframe(trat = trat, ap = log(ap))

# Fazendo a análise pelo easyanova
resacp <- ea1(dac_tp, 1, alpha = 0.01)

# Resultados do teste de médias
resacp$Means

# Adicionando resultados do teste para os ciclos

# Primeiro
primeirop <- dados %>%
  filter(cult == "Primeiro") %>% 
  mutate(teste = case_when(
    trat == "BoneChar" ~ "a",
    trat == "STP" ~ "a",
    trat == "Bayóvar" ~ "a",
    trat == "Argélia" ~ "a",
    trat == "Marrocos" ~ "b",
    trat == "Digestato" ~ "b",
    trat == "Registro" ~ "b",
    trat == "Arraias" ~ "b",
    trat == "Catalão" ~ "c",
    trat == "Pratápolis" ~ "c",
    trat == "Bonito" ~ "d",
    trat == "Controle" ~ "e",
    TRUE ~ as.character(trat)
  ))

# Segundo
segundop <- dados %>%
  filter(cult == "Segundo") %>% 
  mutate(teste = case_when(
    trat == "BoneChar" ~ "a",
    trat == "STP" ~ "a",
    trat == "Bayóvar" ~ "b",
    trat == "Argélia" ~ "b",
    trat == "Marrocos" ~ "a",
    trat == "Digestato" ~ "a",
    trat == "Registro" ~ "a",
    trat == "Arraias" ~ "b",
    trat == "Catalão" ~ "b",
    trat == "Pratápolis" ~ "b",
    trat == "Bonito" ~ "b",
    trat == "Controle" ~ "c",
    TRUE ~ as.character(trat)
  ))

# Acumulado
acumuladop <- dados %>%
  filter(cult == "Acumulado") %>% 
  mutate(teste = case_when(
    trat == "BoneChar" ~ "a",
    trat == "STP" ~ "b",
    trat == "Bayóvar" ~ "b",
    trat == "Argélia" ~ "b",
    trat == "Marrocos" ~ "c",
    trat == "Digestato" ~ "c",
    trat == "Registro" ~ "c",
    trat == "Arraias" ~ "d",
    trat == "Catalão" ~ "d",
    trat == "Pratápolis" ~ "e",
    trat == "Bonito" ~ "e",
    trat == "Controle" ~ "f",
    TRUE ~ as.character(trat)
  ))

# Gerando a coluna
dados$testep <- c(primeirop$teste, segundop$teste, acumuladop$teste)

# Calcular as médias e adicionar os rótulos
dados_resumop <- dados %>% 
  group_by(trat, cult) %>%
  summarise(mean_ap = mean(ap), testep = first(testep), .groups = 'drop') %>%
  mutate(label = paste0(sprintf("%.2f", mean_ap), testep))

# Boxplot para APPA
box_apa = ggplot(data = dados, aes(x = trat, y = ap, color = testep)) +
  geom_boxplot(size = 0.8, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  scale_color_manual(values = cores) +
  scale_y_continuous(breaks = seq(0, 45, by =5)) +
  scale_x_discrete(labels = eixo_x) +
  labs(x = "Treatments", y = "Shoot phosphorus accumulation (mg" ~ pot^-1*")") +
  facet_wrap(~ cult, nrow = 3, ncol = 1, labeller = labeller(cult = facet)) +
  theme_classic() +
  tema

ggsave("box_apa.png", box_apa, width = 10, height = 5, dpi = 150)

# Diagrama de pontos para a APPA
d_apa = ggplot(data = dados, aes(x = trat, y = ap, color = testep)) +
  geom_point(alpha = 0.6, size = 5) +
  geom_text(data = dados_resumop, aes(x = trat, y = mean_ap, label = label), size = 4.5, position = position_nudge(x = 0.45), fontface = "bold") +  
  stat_summary(fun = mean, geom = "point", size = 3, shape = 15) +
  scale_color_manual(values = cores) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_discrete(labels = c(eixo_x), expand = expansion(mult = c(0.05, 0.075))) +
  labs(x = "Treatments", y = "Shoot phosphorus accumulation (mg" ~ pot^-1*")") +
  facet_wrap(~ cult, nrow = 3, ncol = 1, labeller = labeller(cult = facet)) +
  coord_flip() +
  theme_classic() +
  tema

ggsave("d_apa.png", d_apa, width = 10, height = 5, dpi = 150)

# Gerando médias para o cálculo
media <- grupos_media("Primeiro", "mspa", dados, label = "mspa")

# Aplicando a função na tabela
media <- media %>% 
  mutate(efi_rel = eficiencia_relativa(mspa, media))

# Verificando o resultado
media

# Gerando médias para o cálculo
media2 <- grupos_media("Segundo", "mspa", dados, label = "mspa")

# Aplicando a função na tabela
media2 <- media2 %>% 
  mutate(efi_rel = eficiencia_relativa(mspa, media2))

# Verificando o resultado
media2

# Gerando médias para o cálculo
medias <- grupos_media("Acumulado", "mspa", dados, label = "mspa")

# Aplicando a função na tabela
medias <- medias %>% 
  mutate(efi_rel = eficiencia_relativa(mspa, medias))

# Verificando o resultado
medias

# Gerando médias para o cálculo
medip <- grupos_media("Primeiro", "ap", dados, label = "ap")

# Aplicando a função na tabela
medip <- medip %>% 
  mutate(efi_pho = eficiencia_fosforo(ap, medip))

# Verificando o resultado
medip

# Gerando médias para o cálculo
medip2 <- grupos_media("Segundo", "ap", dados, label = "ap")

# Aplicando a função na tabela
medip2 <- medip2 %>% 
  mutate(efi_pho = eficiencia_fosforo(ap, medip2))

# Verificando o resultado
medip2

# Gerando médias para o cálculo
mediap <- grupos_media("Acumulado", "ap", dados, label = "ap")

# Aplicando a função na tabela
mediap <- mediap %>% 
  mutate(efi_pho = eficiencia_fosforo(ap, mediap))

# Verificando o resultado
mediap

# Unindo as duas tabelas em uma
dados_ea <- bind_rows(
  mutate(media, cult = "Primeiro"),
  mutate(medias, cult = "Acumulado")
)

# Ajustando os níveis da variável "cult"
dados_ea$cult <- factor(dados_ea$cult, levels = c("Primeiro", "Acumulado"))

# Visualizando
dados_ea

# Gerando o gráfico
fig_ea <- 
  ggplot(data = dados_ea) +
  geom_bar(aes(x = trat, y = efi_rel), stat = "identity", position = "stack", alpha = 0.60, fill = "#9e2943") +
  geom_text(aes(label = round(efi_rel, 0), y = efi_rel, x = reorder(trat, efi_rel), vjust = 0.5, hjust = ifelse(efi_rel > 0, 1.5, 0)), family = "DejaVu Sans Condensed") +
  scale_y_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 120)) +
  scale_x_discrete(labels = eixo_x) +
  labs(x = "Treatments", y = "Relative efficiency (%)") +
  facet_wrap(~ cult, labeller = labeller(cult = c("Primeiro" = "First crop", "Acumulado" = "Accumulated in two crops"))) +
  coord_flip() +
  theme_classic() +
  tema2

# Visualizando
fig_ea

ggsave("fig_ea.png", fig_ea, width = 10, height = 5, dpi = 150)

# Unindo as duas tabelas em uma
dados_ep <- bind_rows(
  mutate(medip, cult = "Primeiro"),
  mutate(mediap, cult = "Acumulado")
)

# Ajustando os níveis da variável "cult"
dados_ep$cult <- factor(dados_ep$cult, levels = c("Primeiro", "Acumulado"))

# Visualizando
dados_ep

# Gerando o gráfico
fig_ep <- 
  ggplot(data = dados_ep) +
  geom_bar(aes(x = trat, y = efi_pho), stat = "identity", position = "stack", alpha = 0.60, fill = "#d18525") +
  geom_text(aes(label = round(efi_pho, 0), y = efi_pho, x = reorder(trat, efi_pho), vjust = 0.5, hjust = ifelse(efi_pho >= 0.5, 1.3, ifelse(efi_pho > 0, 0.7, 0))), family = "DejaVu Sans Condensed") +
  scale_x_discrete(labels = eixo_x) +
  scale_y_continuous(breaks = c(0, 30, 60, 90, 120, 150, 180)) +
  labs(x = "Treatments", y = "P use efficiency (%)") +
  facet_wrap(~ cult, labeller = labeller(cult = c("Primeiro" = "", "Acumulado" = ""))) +
  coord_flip() +
  theme_classic() +
  tema2

# Visualizando
fig_ep

ggsave("fig_ep.png", fig_ep, width = 10, height = 5, dpi = 150)

