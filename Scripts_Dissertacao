# =================================================
# autor: Rafael A S Oliveira
# Script R para calcular a área (em ha) de diferentes classes de 
# Uso e cobertura do Solo e suas Métricas (Número de Fragmentos, Diversidade, Equitabilidade e Dominância )
# em Apiários
# Date: 2026-04-13
# =================================================

# =================================================
# 1. Carregar e instalar dependências do R
# =================================================

install.packages(c("landscapemetrics", "terra", "sf", "tidyverse", "scales"))

library(landscapemetrics)
library(terra)
library(sf)
library(tidyverse)
library(scales)

# =================================================
# 2. Carregar e definir dados
# =================================================

# Definir o tamanho do buffer (em metros) para cada Apiário
#buffer_size <- 500 #Tamanho do raio do buffer em metros
#buffer_size <- 1000 #Tamanho do raio do buffer em metros
#buffer_size <- 1500 #Tamanho do raio do buffer em metros
buffer_size <- 3000 #Tamanho do raio do buffer em metros

# Definir diretório de trabalho
setwd("D:/R/DISSERTACAO_FINAL")

# Utilizar numeros não cientificos (1e3)
options(scipen = 1e3)

# Arquivo Raster Classificado (TIFF)
r_lulc <- rast("TIFF/LULC_S2_Dissertacao_Final_1779398445467.tif") # 19/05/2026

# Arquivo de Pontos de Interesse (CSV) 
pontos <- readr::read_csv("CSV/PONTOS_APIARIOS.csv")
pontos

# Transformar em coordenadas
pontos_sf <- sf::st_as_sf(pontos, coords = c("longitude", "latitude"), crs = 4326)

plot(pontos_sf$geometry, pch = 20, axes = TRUE, main = "WGS84 Geo")

# Reprojetar coordenadas para o CSR do arquivo raster
pontos_proj <- st_transform(pontos_sf, crs(r_lulc))

plot(pontos_proj$geometry, pch = 20, axes = TRUE, main = crs(r_lulc, describe=TRUE, proj=TRUE)$name)

# ==============================================================================
# 3. Calcular a area total em hectares de cada buffer
# ==============================================================================

# O sample_lsm já cria o buffer automaticamente com o argumento 'size'
metricas_area <- sample_lsm(
  landscape = r_lulc,
  y = pontos_proj,
  shape = "circle",     # Formato geométrico do buffer
  size = buffer_size,   # Raio do buffer em metros
  level = "class",      # Nível de classe
  what = "lsm_c_ca",    # Métrica: Class Area (CA) -> Retorna em Hectares
  verbose = FALSE
)

# Visualizar o resultado bruto (formato longo)
print(head(metricas_area))

# ==============================================================================
# 4. Organizar a Tabela (Pivotar)
# ==============================================================================
# O output do sample_lsm vem em formato longo. Vamos transformar para formato largo
# (uma coluna para cada classe), no formato CSV original.

tabela_final <- metricas_area %>%
  # Selecionar apenas as colunas úteis
  select(plot_id, class, value) %>%
  
  # Adicionar o nome do apiário (plot_id é o índice sequencial 1, 2, 3...)
  left_join(pontos %>% select(id,apiarios), by = c("plot_id" = "id")) %>%
  
  # Pivotar: Transformar classes em colunas
  pivot_wider(
    names_from = class,       # As classes viram nomes de colunas
    values_from = value,      # Os valores de área preenchem as células
    names_prefix = "Col_"  # Prefixo para a coluna (ex: C_1, C_2)
  ) %>%
  
  # Substituir NAs por 0 (caso alguma classe não exista no buffer)
  mutate(across(starts_with("Col_"), ~replace_na(., 0)))

# ==============================================================================
# 5. Salvar ou Visualizar
# ==============================================================================
# Renomeação das colunas de classes de uso
tabela_final_csv <- tabela_final %>%
  rename(
    "id" = plot_id,
    "Apiarios" = apiarios,
    "Corpos Hidricos" = Col_1,
    "Cobertura Florestal" = Col_2,	
    "Solo Exposto" = Col_3,
    "Prado" = Col_4,
    "Agricultura"	= Col_5,
    "Area Costruida" = Col_6,
    "Silvicultura" = Col_7
  )

print(tabela_final_csv)

# Salvar em CSV para abrir no Excel:
nome_arquivo <- paste0("CSV/",buffer_size,"m/Areas_LULC_Apiarios_", buffer_size,"m_Calculado.csv")
write.csv2(tabela_final_csv, nome_arquivo, row.names = FALSE)

# ==============================================================================
# 6. Gerar o Gráfico
# ==============================================================================

# ==============================================================================
# A. Preparar os Dados (Transformação Wide -> Long)
# ==============================================================================

dados_grafico <- tabela_final %>%
  # Remover colunas desnecessárias para o gráfico (plot_id)
  select(-plot_id) %>%
  #select(-plot_id,-Col_1,-Col_3,-Col_4,-Col_5,-Col_6,-Col_7) %>% # Somente Floresta
  
  # "Pivotar" para formato longo
  pivot_longer(
    cols = starts_with("Col_"), # Pega todas as colunas de classe
    names_to = "Col_ID",        # Cria coluna com o nome antigo (ex: Col_1)
    values_to = "Area_Ha"       # Cria coluna com os valores
  ) %>%
  
  # Renomear as classes para nomes reais
  mutate(Classe_Nome = case_when(
    Col_ID == "Col_1" ~ "Corpos Hídricos",
    Col_ID == "Col_2" ~ "Cobertura Florestal",
    Col_ID == "Col_3" ~ "Solo Exposto",
    Col_ID == "Col_4" ~ "Prado",
    Col_ID == "Col_5" ~ "Agricultura",
    Col_ID == "Col_6" ~ "Área Construída",
    Col_ID == "Col_7" ~ "Silvicultura"
  ))

# ==============================================================================
# B. Definir as Cores para Legenda (Mesmas do Mapa)
# ==============================================================================
cores_personalizadas <- c(
  "Corpos Hídricos" = "#2532e4", # Azul
  "Cobertura Florestal" = "#1f8d49", # Verde
  "Solo Exposto" = "#db4d4f", # Vermelho Claro
  "Prado" = "#edde8e", # Bege
  "Agricultura" = "#e974ed", # Rosa
  "Área Construída" = "#d4271e", # Vermelho
  "Silvicultura" = "#7a5900"  # Marrom
)

# Ordenar a legenda
dados_grafico$Classe_Nome <- factor(dados_grafico$Classe_Nome, 
                                    levels = c("Corpos Hídricos",
                                               "Cobertura Florestal", 
                                               "Silvicultura",
                                               "Prado", 
                                               "Agricultura",
                                               "Solo Exposto",
                                               "Área Construída"))

# ==============================================================================
# C. Gerar o Gráfico
# ==============================================================================

ggplot(dados_grafico, aes(x = fct_inorder(apiarios), y = Area_Ha, fill = Classe_Nome)) +
  
  # Geometria de colunas empilhadas
  geom_col(position = "fill", width = 0.6) + 
  
  # Percentagens no eixo y
  scale_y_continuous(labels = scales::percent)+
  
  # Aplicação das cores personalizadas
  scale_fill_manual(values = cores_personalizadas, name = "Legenda") +
  
  # Ajustes de Eixos e Títulos
  labs(
    title = "Uso e Cobertura da Terra por Área de Estudo",
    subtitle = paste0("Buffer de ", buffer_size," m"),
    x = "Apiários",
    # y = "Área em hectares (ha)"
    y = "Percentagem (%)"
  ) +
  
  # Tema visual
  theme_minimal(ink = "#222",base_family = "serif") +
  theme(
    # Fonte serif (Times New Roman)
    text = element_text(family = "serif"),
    # Títulos e eixos
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    
    # Eixo X
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(face = "bold", size = 12),
        
    # Legenda
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    
    # Limpeza visual
    panel.grid.major.x = element_blank()
  )

# ==============================================================================
# D. Salvar Gráfico
# ==============================================================================
ggsave(paste0("PNG/",buffer_size,"m/Grafico_Composicao_Apiarios_",buffer_size,"m.png"), width = 10, height = 6, dpi = 300)

# ==============================================================================
# 7. Gerar Estatisticas de Paisagem
# ==============================================================================

# 7.1 Calcular Métricas de Paisagem (Landscape Level)
# NP = Number of Patches (Número de Fragmentos): Indica o grau de fragmentação da paisagem.
# SHDI = Shannon's Diversity Index (Diversidade de Shannon): Quanto maior o valor, mais diverso é o uso do solo no 
#       buffer.
# SHEI = Shannon's Evenness Index (Equitabilidade de Shannon): Varia de 0 a 1. Valores próximos a 1 indicam que as 
#       classes de uso estão distribuídas de forma igualitária (uniforme) na área.

metricas_paisagem <- sample_lsm(
  landscape = r_lulc,
  y = pontos_proj,
  shape = "circle",
  size = buffer_size,
  level = "landscape", # Nível da paisagem total do buffer
  what = c("lsm_l_np", "lsm_l_shdi", "lsm_l_shei"),
  verbose = FALSE
)

n_classes <- metricas_area %>%
  group_by(plot_id) %>%
  summarise(n_cat = n_distinct(class))

# 7.2 Organizar Tabela de Índices Ecológicos
estatisticas_ecologicas <- metricas_paisagem %>%
  select(plot_id, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  
  # Unir com os nomes dos apiários e número de classes
  left_join(pontos %>% mutate(id = row_number()) %>% select(id, apiarios), 
            by = c("plot_id" = "id")) %>%
  left_join(n_classes, by = "plot_id") %>%
  
  # Calcular a Dominância de Simpson ou Shannon Adaptada
  # Aqui calculamos a Dominância Relativa baseada em Shannon
  # Dominância: Indica o quanto uma ou poucas classes dominam a paisagem.
  mutate(dominancia = log(n_cat) - shdi) %>%
  
  # Renomear colunas para termos claros
  rename(
    "Apiario" = apiarios,
    "Num_Fragmentos" = np,
    "Diversidade_Shannon" = shdi,
    "Equitabilidade_Shannon" = shei,
    "Dominancia" = dominancia
  ) %>%
  select(Apiario, Num_Fragmentos, Diversidade_Shannon, 
         Equitabilidade_Shannon, Dominancia)

# 7.3 Visualizar no Console
print("--- Estatísticas de Paisagem por Apiário ---")
print(estatisticas_ecologicas)

# 7.4 Salvar Resultados
write.csv2(estatisticas_ecologicas, 
           paste0("CSV/",buffer_size,"m/Estatisticas_Paisagem_Apiarios_", buffer_size, "m.csv"), 
           row.names = FALSE)


# =================================================
# autor: Rafael A S Oliveira
# Script R para análise estatistica dos dados de Uso e Cobertura de Apiários
# Date: 2026-04-14
# =================================================

# ==============================================================================
# 1. Carregamento e Limpeza dos Dados
# ==============================================================================
install.packages(c("factoextra", "cluster","ggplot2"))

library(factoextra) # Para visualização elegante
library(cluster)    # Para algoritmos de cluster
library(ggplot2)    # Para salvar os gráficos

nome_arquivo <- paste0("CSV/", buffer_size,"m/Areas_LULC_Apiarios_", buffer_size,"m_Calculado.csv")

# Ler o arquivo (ajuste o caminho se necessário)
# sep=";" define o separador
# dec="." define o decimal (verifique se seu excel usa vírgula ou ponto)
dados <- read.csv(nome_arquivo, sep = ";", dec = ",")
# Visualizar as primeiras linhas
head(dados)

# Limpeza:
# 1. Definir a primeira coluna (AP01, AP02...) como nome das linhas
rownames(dados) <- dados[,2]

# 2. Remover a primeira coluna (agora que já virou índice) e as colunas vazias
# Vamos selecionar apenas as colunas de dados numéricos (ex: colunas 3 a 9)
df_clean <- dados[,3:9]

# 3. Remover linhas que contenham NA (se houver)
df_clean <- na.omit(df_clean)

# Visualizar dados limpos
print("Dados prontos para análise:")
print(head(df_clean))

# ==============================================================================
# 2. Padronização (Standardization)
# ==============================================================================
df_scaled <- scale(df_clean)

# ==============================================================================
# 3. Análise de Componentes Principais (PCA)
# ==============================================================================
# prcomp realiza a PCA. scale. = TRUE garante a normalização
res.pca <- prcomp(df_clean, scale. = TRUE)

# Ver a variância explicada
get_eig(res.pca)

# --- Gráfico: Biplot (PCA) ---
# Mostra tanto os indivíduos (Apiários) quanto as variáveis (Classes de uso)
plot_pca = fviz_pca_biplot(res.pca,
                repel = TRUE,                # Evita sobreposição de texto
                col.var = "#3242a8",         # Cor das variáveis (setas)
                col.ind = "#000000",         # Cor dos indivíduos (pontos)
                pointsize = 3,
                title = "PCA - Biplot dos Apiários",
                subtitle = paste0("Buffer de ", buffer_size,"m"),
                ggtheme = theme_minimal(),
                ylab = "PC2",
                xlab = "PC1"
                
) + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(plot_pca)

# Salvar gráfico PCA gerado
nome_pca <- paste0("PNG/",buffer_size,"m/PCA_LULC_Apiarios_", buffer_size,"m.png")
ggsave(nome_pca, plot_pca, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 4. Clusterização Hierárquica (Dendrograma)
# ==============================================================================

# --- Gráfico: Método do Cotovelo (Elbow Method) ---
# Ajuda a decidir o número ideal de clusters
fviz_nbclust(df_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Visualisação para: Método do Cotovelo (Elbow Method) para K ideal",
       x = "Número de Clusters (k)",
       y = "Soma Total de Quadrados Dentro dos Clusters"
  )

k_ideal <- 6; # Deve ser alterado segundo o gráfico Visualisação

# Geração do gráfico final com representação do k ideal
plot_elbow <- fviz_nbclust(df_scaled, kmeans, method = "wss", k.max = 10) +
  geom_vline(xintercept = k_ideal, linetype = 2, color = "red") +
  labs(title = "Método do Cotovelo (Elbow Method) para K ideal",
       x = "Número de Clusters (k)",
       y = "Soma Total de Quadrados Dentro dos Clusters"
  ) + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(plot_elbow)

# Salvar gráfico Elbow gerado
nome_elbow <- paste0("PNG/",buffer_size,"m/Elbow_LULC_Apiarios_", buffer_size,"m.png")
ggsave(nome_elbow, plot_elbow, width = 10, height = 6, dpi = 300)

# --- Gráfico: Dendrograma ---
# Calcular matriz de distância (Euclidiana)
dist_mat <- dist(df_scaled, method = "euclidean")

# Realizar o agrupamento (Método Ward, igual ao Python)
hc <- hclust(dist_mat, method = "ward.D2")

# Geração do gráfico dendograma com k ideal
plot_dend <- fviz_dend(hc, 
          k = k_ideal,           # Número de grupos desejado (cortar a árvore)
          cex = 0.6,             # Tamanho do texto
          k_colors = "jco",      # Paleta de cores
          ggtheme = theme_minimal(), #mudar tema
          rect = TRUE,           # Adicionar retângulo ao redor dos grupos
          horiz = FALSE,
          main = "Dendrograma de Agrupamento - Uso e Ocupação da Terra",
          subtitle = "Método de Ward, Euclidiano",
          ylab = "Distância", 
          xlab = "Apiários", 
          sub = ""
) + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

print(plot_dend)

# Salvar gráfico Dendograma gerado
nome_dendograma <- paste0("PNG/",buffer_size,"m/Dendrograma_LULC_Apiarios_", buffer_size,"m.png")
ggsave(nome_dendograma, plot_dend, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 5. K-Means (Clusterização Não-Hierárquica)
# ==============================================================================

# Aplicando o K-Means ideal (baseado na análise anterior)
set.seed(123) # Para reprodutibilidade (o resultado ser sempre o mesmo)
km.res <- kmeans(df_scaled, centers = k_ideal, nstart = 25)

# Ver a média de cada variável por cluster
aggregate(df_clean, by=list(cluster=km.res$cluster), mean)

# --- Gráfico: Clusters K-Means ---
plot_cluster = fviz_cluster(km.res, data = df_scaled,
             palette = "Set2", 
             ggtheme = theme_minimal(),
             main = paste0("Agrupamento K-Means (k=",k_ideal,")")
)+ theme(plot.title = element_text(hjust = 0.5))

print(plot_cluster)

# Salvar gráfico Cluster gerado
nome_cluster <- paste0("PNG/",buffer_size,"m/Cluster_K-means_LULC_Apiarios_", buffer_size,"m.png")
ggsave(nome_cluster, plot_cluster, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 6. Salvar os resultados em CSV
# ==============================================================================
# Adicionar a coluna de cluster aos dados originais
dados_finais <- cbind(df_clean, Cluster = km.res$cluster)

# Salvar em CSV
nome_arquivo_exp <- paste0("CSV/",buffer_size,"m/Apiarios_Cluster_", buffer_size,"m.csv")
write.csv2(dados_finais, nome_arquivo_exp,fileEncoding = 'UTF-8')
