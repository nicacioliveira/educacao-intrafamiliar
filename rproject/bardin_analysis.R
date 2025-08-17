# ====================================================================
# SCRIPT FINAL TCC - ANÁLISE HÍBRIDA COMPLETA (v21.0 - OFICIAL)
# BARDIN + LDA SINTONIZADO + BIGRAMAS + TABELAS DE EXEMPLOS (BARDIN E LDA) + GRÁFICOS COMPLETOS
# AUTORA: Heloisa Diniz Ferreira
# DATA: 17 de agosto de 2025
# ====================================================================
# DESCRIÇÃO: Este script realiza uma análise híbrida de dados textuais coletados
# do Instagram. Ele combina a Análise de Conteúdo de Bardin (manual e teórica)
# com a modelagem de tópicos Latent Dirichlet Allocation (LDA) para validar
# e explorar os temas emergentes. O script é autônomo e gera todos os
# artefatos necessários (dados e gráficos) para a seção de resultados do TCC.
# ====================================================================


# ====================================================================
# 1. CONFIGURAÇÕES GERAIS
# ====================================================================
# METODOLOGIA: Nesta seção, todos os parâmetros que controlam a análise
# são definidos. Isso garante a reprodutibilidade da pesquisa, permitindo
# que qualquer pessoa execute o script com as mesmas configurações.

CONFIG <- list(
  # --- Fontes de Dados ---
  arquivo_dados = "../resultados/comentarios_instagram.csv", # Caminho relativo para o arquivo de dados brutos.
  coluna_texto = "textoComentario",                          # Nome da coluna que contém os comentários.
  coluna_likes = "likes",                                    # Nome da coluna com o número de curtidas.
  encoding = "UTF-8",                                        # Codificação do arquivo para correta leitura de acentos.

  # --- Filtros para a Construção do Corpus ---
  min_caracteres = 15,                                       # Comentários com menos caracteres que isso serão ignorados.
  palavras_contexto = c("irmão", "irmao", "irmã", "irma", "pai", "mãe", "mae", "casa", "família",
                        "diferente", "diferença", "tratamento", "machismo",
                        "filho", "filha", "criação", "educação"), # Palavras-chave para garantir que o comentário é relevante ao tema.

  # --- Parâmetros das Análises ---
  k_lda_manual = 4,      # (Decisão Metodológica Chave) Número de tópicos para o LDA, definido como 4 para espelhar o número de categorias de Bardin.
  alpha_lda = 0.1,       # Parâmetro de sintonia do LDA. Um valor baixo incentiva o modelo a atribuir cada comentário a um único tópico, tornando-os mais distintos.
  num_top_bigramas = 8,  # Número de bigramas (termos com duas palavras) a serem exibidos por categoria.
  num_exemplos = 2,      # Número de comentários de exemplo a serem extraídos para cada categoria/tópico.

  # --- Configurações de Saída ---
  dir_resultados = "resultados",      # Pasta para salvar os arquivos de dados gerados (CSV).
  dir_graficos = "resultados/graficos", # Pasta para salvar as imagens dos gráficos (PNG).
  dpi = 300,                          # Resolução dos gráficos salvos, adequada para publicação.

  # --- Reprodutibilidade ---
  seed = 1234 # Semente aleatória para garantir que os resultados do LDA sejam sempre os mesmos a cada execução.
)


# ====================================================================
# 2. SISTEMA DE CATEGORIAS (BARDIN)
# ====================================================================
# METODOLOGIA: Esta estrutura de dados representa o quadro teórico da pesquisa.
# Cada nome de lista é uma categoria temática definida a priori, baseada na
# teoria e na análise de conteúdo de Bardin. O vetor de palavras associado
# a cada categoria serve como um dicionário para a classificação automática.

SISTEMA_CATEGORIAS <- list(
  `Divisão Desigual do Trabalho Doméstico` = c("lavar", "cozinhar", "limpar", "cuidar", "casa", "louça", "roupa", "faxina", "arrumar", "servir", "tarefa"),
  `Desigualdade Financeira e de Suporte` = c("dinheiro", "presente", "pagou", "curso", "faculdade", "carro", "moto", "bicicleta", "ajuda", "investiu"),
  `Contrastes na Liberdade e Autonomia` = c("sair", "namorar", "amigos", "festa", "voltar", "hora", "permissão", "liberdade", "proibida", "viajar"),
  `Percepção Crítica e Resistência` = c("injusto", "revolta", "questionar", "brigar", "machismo", "desigual", "rebelar", "bato o pé", "não aceitava")
)


# ====================================================================
# 3. CARREGAR BIBLIOTECAS
# ====================================================================
# DESCRIÇÃO: Carrega todos os pacotes R necessários para a análise.
# É importante garantir que todos estejam instalados antes de executar.
# install.packages(c("tidyverse", "tidytext", "quanteda", ...))

suppressPackageStartupMessages({
  library(tidyverse)      # Para manipulação de dados e gráficos (ggplot2)
  library(tidytext)       # Para análise de texto no formato tidy
  library(quanteda)       # Para criação de corpus e matrizes de documentos-termos (DFM)
  library(quanteda.textplots) # Para visualizações de redes de palavras
  library(topicmodels)    # Para a modelagem de tópicos (LDA)
  library(wordcloud2)     # Para criar nuvens de palavras interativas
  library(ggraph)         # Para gráficos de rede mais avançados
  library(igraph)         # Para manipulação de objetos de rede
  library(ggrepel)        # Para evitar sobreposição de texto nos gráficos
  library(lexiconPT)      # Para análise de sentimentos em português
})


# ====================================================================
# 4. FUNÇÕES AUXILIARES
# ====================================================================
# DESCRIÇÃO: Define funções personalizadas para manter o código principal
# limpo e organizado. Não é necessário um arquivo externo "funcoes.R",
# pois todas estão contidas aqui.

# Função para criar/limpar as pastas de saída.
setup_dirs <- function() {
  if (!dir.exists(CONFIG$dir_resultados)) dir.create(CONFIG$dir_resultados, recursive = TRUE)
  if (!dir.exists(CONFIG$dir_graficos)) dir.create(CONFIG$dir_graficos, recursive = TRUE)
  cat("    - Pastas de resultados e gráficos prontas.\n")
}

# Função para limpar e padronizar o texto dos comentários.
clean_text <- function(text) {
  text %>%
    str_to_lower() %>%
    str_replace_all("(?i)\\bmãe|mae|mamae\\b", "mãe") %>%
    str_replace_all("(?i)\\bpai|papai\\b", "pai") %>%
    str_replace_all("(?i)\\birmao|irmão\\b", "irmão") %>%
    str_replace_all("(?i)\\birma|irmã\\b", "irmã") %>%
    str_remove_all("http[s]?://[^\\s]+") %>%
    str_remove_all("@[A-Za-z0-9_]+") %>%
    str_remove_all("#[A-Za-z0-9_]+") %>%
    str_remove_all("[^\\p{L}\\s]") %>%
    str_squish()
}

# Função que aplica o sistema de categorias a um texto.
categorizar_documento <- function(texto, sistema_categorias) {
  categorias_encontradas <- names(sistema_categorias)[map_lgl(sistema_categorias, ~ any(str_detect(texto, .x)))]
  if (length(categorias_encontradas) == 0) return("Sem Categoria")
  return(categorias_encontradas)
}

# Função para padronizar o visual de todos os gráficos do TCC.
tema_tcc <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "gray20"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 1, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold", color = "gray30"),
      axis.text = element_text(size = 10, color = "gray50"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#F8F9FA", color = NA),
      panel.background = element_rect(fill = "#F8F9FA", color = NA)
    )
}


# ====================================================================
# 5. CARREGAMENTO E PRÉ-PROCESSAMENTO
# ====================================================================
cat("1. Carregando e pré-processando os dados...\n")
set.seed(CONFIG$seed)
setup_dirs()

dados_brutos <- read_csv(CONFIG$arquivo_dados, col_types = cols(.default = "c")) %>%
  select(texto = .data[[CONFIG$coluna_texto]], likes = .data[[CONFIG$coluna_likes]]) %>%
  mutate(likes = as.integer(likes), likes = ifelse(is.na(likes), 0, likes))

corpus_filtrado <- dados_brutos %>%
  mutate(id_documento = row_number(),
         texto_limpo = clean_text(texto),
         n_palavras = str_count(texto_limpo, "\\S+")) %>%
  filter(nchar(texto_limpo) >= CONFIG$min_caracteres,
         str_detect(texto_limpo, paste(CONFIG$palavras_contexto, collapse = "|")))


# ====================================================================
# 6. ANÁLISE DE BARDIN (MANUAL)
# ====================================================================
cat("2. Executando a Análise de Conteúdo de Bardin...\n")
# METODOLOGIA: Cada comentário é classificado em uma ou mais categorias
# definidas na Seção 2. Este processo simula a etapa de codificação
# da Análise de Conteúdo.

dados_categorizados <- corpus_filtrado %>%
  mutate(categorias = map(texto_limpo, ~categorizar_documento(.x, SISTEMA_CATEGORIAS))) %>%
  unnest(categorias)

# Calcula a frequência de cada categoria para entender a prevalência dos temas.
freq_categorias <- dados_categorizados %>%
  count(categorias, sort = TRUE, name = "n") %>%
  mutate(percentual = n / sum(n) * 100)
write_csv(freq_categorias, file.path(CONFIG$dir_resultados, "frequencia_categorias_bardin.csv"))


# ====================================================================
# 7. ANÁLISE DE TÓPICOS (LDA) SINTONIZADA
# ====================================================================
cat("3. Executando a Análise de Tópicos (LDA) sintonizada...\n")
# METODOLOGIA: O LDA é uma técnica de aprendizado não supervisionado que
# identifica "tópicos" (conjuntos de palavras que co-ocorrem frequentemente)
# em um conjunto de documentos. Aqui, ele é usado para validar se os temas
# encontrados manualmente também emergem de uma análise puramente estatística.

# Prepara o texto para o LDA: remove pontuação, números e stopwords (palavras comuns como "e", "ou", "a").
custom_stopwords <- c(stopwords("pt"), "q", "pra", "pro", "tá", "né", "aí", "coisa", "tudo",
                      "nada", "sempre", "nunca", "ainda", "hoje", "disse", "falou",
                      "era", "foi", "tinha", "porque", "depois", "enquanto", "então", "assim")

tokens_obj <- corpus(corpus_filtrado, text_field = "texto_limpo") %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = custom_stopwords) %>%
  tokens_select(min_nchar = 3)

# Cria uma matriz documento-termo (DFM), a estrutura de dados necessária para o LDA.
dfm_obj <- tokens_obj %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5) # Remove palavras muito raras.

dtm_lda <- convert(dfm_obj, to = "topicmodels")

# Define os parâmetros do modelo com base nas decisões da Seção 1.
k_definido <- CONFIG$k_lda_manual
alpha_definido <- CONFIG$alpha_lda
cat("    - Número de tópicos (k) definido como:", k_definido, "\n")
cat("    - Alpha definido como:", alpha_definido, "(para tópicos mais nítidos)\n")

# Executa o modelo LDA.
lda_model <- LDA(dtm_lda, k = k_definido, method = "VEM",
                 control = list(seed = CONFIG$seed, alpha = alpha_definido))

# Extrai os resultados: as palavras mais importantes por tópico (beta) e o tópico principal de cada documento (gamma).
termos_topicos_lda <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>% slice_max(beta, n = 10) %>% ungroup() %>% arrange(topic, -beta)
documentos_topicos_lda <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>% slice_max(gamma, n = 1) %>% ungroup()
freq_topicos_lda <- documentos_topicos_lda %>%
  count(topic, sort = TRUE, name = "n") %>%
  mutate(percentual = n / sum(n) * 100)


# ====================================================================
# 7.5 MAPEAMENTO E CÁLCULO DE SIMILARIDADE: LDA -> BARDIN
# ====================================================================
cat("3.5 Mapeando Tópicos e calculando similaridade...\n")
# METODOLOGIA: Para comparar os tópicos automáticos do LDA com as categorias
# manuais de Bardin, calculamos a similaridade de Jaccard entre os conjuntos
# de palavras de cada um. O tópico LDA é "nomeado" com a categoria de Bardin
# com a qual ele tem a maior similaridade.

jaccard_similarity <- function(set1, set2) {
  intersection <- length(intersect(set1, set2))
  union_set <- length(union(set1, set2))
  if (union_set == 0) return(0)
  return(intersection / union_set)
}

similaridade_completa <- map_df(1:k_definido, function(lda_topic) {
  palavras_lda <- termos_topicos_lda %>% filter(topic == lda_topic) %>% pull(term)
  map_df(names(SISTEMA_CATEGORIAS), function(cat_nome) {
    palavras_categoria <- SISTEMA_CATEGORIAS[[cat_nome]]
    tibble(
      topic = lda_topic,
      categoria_mapeada = cat_nome,
      similaridade = jaccard_similarity(palavras_lda, palavras_categoria),
      palavras_comuns = length(intersect(palavras_lda, palavras_categoria))
    )
  })
})

topic_mapping <- similaridade_completa %>%
  group_by(topic) %>%
  slice_max(similaridade, n = 1, with_ties = FALSE) %>%
  ungroup()

freq_topicos_lda_mapeado <- freq_topicos_lda %>%
  left_join(topic_mapping, by = "topic")

write_csv(freq_topicos_lda_mapeado, file.path(CONFIG$dir_resultados, "frequencia_topicos_lda_mapeado.csv"))


# ====================================================================
# 8. ANÁLISE DE BIGRAMAS E EXEMPLOS (BARDIN)
# ====================================================================
cat("4. Extraindo Bigramas e Exemplos para Categorias de Bardin...\n")
# METODOLOGIA: Para enriquecer a análise qualitativa, extraímos as expressões
# de duas palavras (bigramas) mais comuns e os comentários mais representativos
# (baseado em curtidas) para cada categoria de Bardin.

bigramas_por_categoria <- dados_categorizados %>%
  unnest_tokens(bigram, texto_limpo, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stopwords, !word2 %in% custom_stopwords) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(categorias != "Sem Categoria") %>%
  count(categorias, bigram, sort = TRUE) %>%
  group_by(categorias) %>%
  slice_max(n, n = CONFIG$num_top_bigramas) %>%
  ungroup()
write_csv(bigramas_por_categoria, file.path(CONFIG$dir_resultados, "frequencia_bigramas_por_categoria.csv"))

tabela_exemplos_bardin <- dados_categorizados %>%
  filter(categorias != "Sem Categoria") %>%
  group_by(categorias) %>%
  arrange(desc(likes), desc(nchar(texto))) %>%
  slice_head(n = CONFIG$num_exemplos) %>%
  ungroup() %>%
  select(Categoria = categorias, `Exemplo de Relato` = texto, `Likes` = likes)
write_csv(tabela_exemplos_bardin, file.path(CONFIG$dir_resultados, "tabela_exemplos_categorias_bardin.csv"))


# ====================================================================
# 8.5 EXTRAÇÃO DE EXEMPLOS PARA TÓPICOS LDA
# ====================================================================
cat("4.5 Extraindo Exemplos Representativos para Tópicos LDA...\n")
# METODOLOGIA: Similar à seção anterior, mas aqui selecionamos os comentários
# que o modelo LDA considerou os exemplos mais "puros" de cada tópico.

documentos_exemplos_lda <- tidy(lda_model, matrix = "gamma") %>%
  group_by(topic) %>%
  slice_max(gamma, n = CONFIG$num_exemplos) %>%
  ungroup()

tabela_exemplos_lda <- documentos_exemplos_lda %>%
  mutate(id_documento = as.integer(document)) %>%
  left_join(corpus_filtrado, by = "id_documento") %>%
  left_join(topic_mapping, by = "topic") %>%
  select(
    `Tópico Mapeado para` = categoria_mapeada,
    `Exemplo de Relato` = texto,
    `Likes` = likes,
    `Probabilidade (Gamma)` = gamma
  ) %>%
  filter(!is.na(`Tópico Mapeado para`))

write_csv(tabela_exemplos_lda, file.path(CONFIG$dir_resultados, "tabela_exemplos_topicos_lda.csv"))


# ====================================================================
# 9. GERAÇÃO DE GRÁFICOS E ARTEFATOS FINAIS
# ====================================================================
cat("5. Gerando todos os gráficos e artefatos finais...\n")

# ---- Gráfico 1: Frequência das Categorias (Bardin) ----
# PROPÓSITO: Visualizar a distribuição dos relatos entre as categorias manuais.
grafico_freq_bardin <- freq_categorias %>%
  filter(categorias != "Sem Categoria") %>%
  ggplot(aes(x = n, y = fct_reorder(str_wrap(categorias, 30), n))) +
  geom_col(fill = "#3498db", alpha = 0.8) +
  geom_text(aes(label = paste0(n, " (", round(percentual, 1), "%)")), hjust = -0.1, size = 3.5, color = "gray30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Análise Manual: Frequência das Categorias (Bardin)", subtitle = "Baseado nas categorias teóricas pré-definidas", x = "Número de Relatos", y = "Categoria Temática") +
  tema_tcc()
ggsave(file.path(CONFIG$dir_graficos, "01_frequencia_categorias_bardin.png"), plot = grafico_freq_bardin, width = 10, height = 7, dpi = CONFIG$dpi)

# ---- Gráfico 2: Tópicos Descobertos via LDA (Palavras-chave) ----
# PROPÓSITO: Mostrar as palavras que definem cada tópico encontrado pelo LDA.
grafico_palavras_lda_aprimorado <- termos_topicos_lda %>%
  left_join(topic_mapping, by = "topic") %>%
  mutate(categoria_mapeada = ifelse(is.na(categoria_mapeada), paste("Tópico", topic), categoria_mapeada)) %>%
  mutate(term = reorder_within(term, beta, categoria_mapeada)) %>%
  ggplot(aes(x = beta, y = term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ str_wrap(categoria_mapeada, 35), scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Análise Automática: Termos que Definem cada Tópico (LDA)",
    subtitle = "Tópicos nomeados pela categoria de Bardin mais similar (via Similaridade de Jaccard)",
    x = "Probabilidade do termo pertencer ao tópico (β)",
    y = "Termos mais representativos"
  ) +
  tema_tcc()
ggsave(file.path(CONFIG$dir_graficos, "02_topicos_palavras_lda_aprimorado.png"), plot = grafico_palavras_lda_aprimorado, width = 12, height = 9, dpi = CONFIG$dpi)

# ---- Gráfico 3: Frequência dos Tópicos (LDA com Nomes Mapeados) ----
# PROPÓSITO: Visualizar a distribuição dos relatos entre os tópicos automáticos.
grafico_freq_lda <- freq_topicos_lda_mapeado %>%
  group_by(categoria_mapeada) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  mutate(percentual = n / sum(n) * 100) %>%
  ggplot(aes(x = n, y = fct_reorder(str_wrap(categoria_mapeada, 30), n))) +
  geom_col(fill = "#e74c3c", alpha = 0.8) +
  geom_text(aes(label = paste0(n, " (", round(percentual, 1), "%)")), hjust = -0.1, size = 3.5, color = "gray30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Análise Automática: Frequência dos Tópicos (LDA Mapeado)", subtitle = "Número de comentários por tópico, nomeado pela categoria mais similar", x = "Número de Comentários", y = "Tópico Mapeado para Categoria") +
  tema_tcc()
ggsave(file.path(CONFIG$dir_graficos, "03_frequencia_topicos_lda_mapeado.png"), plot = grafico_freq_lda, width = 10, height = 7, dpi = CONFIG$dpi)

# ---- Gráfico 4: Bigramas Mais Frequentes por Categoria (Bardin) ----
# PROPÓSITO: Identificar as expressões de duas palavras mais comuns em cada tema.
grafico_bigramas <- bigramas_por_categoria %>%
  mutate(bigram = reorder_within(bigram, n, categorias)) %>%
  ggplot(aes(x = n, y = bigram, fill = categorias)) +
  geom_col(show.legend = FALSE) + scale_y_reordered() + facet_wrap(~ categorias, scales = "free") +
  labs(title = "Bigramas Mais Comuns por Categoria", subtitle = "Principais expressões de duas palavras em cada tema", x = "Frequência", y = "Bigrama") +
  tema_tcc()
ggsave(file.path(CONFIG$dir_graficos, "04_bigramas_por_categoria.png"), plot = grafico_bigramas, width = 12, height = 8, dpi = CONFIG$dpi)

# ---- Artefato 5: Wordcloud ----
# PROPÓSITO: Criar uma representação visual das palavras mais frequentes na categoria principal.
if (nrow(freq_categorias) > 0) {
  categoria_principal <- freq_categorias$categorias[1]
  palavras_cat_principal <- dados_categorizados %>%
    filter(categorias == categoria_principal) %>%
    unnest_tokens(word, texto_limpo) %>%
    anti_join(tibble(word = custom_stopwords), by = "word") %>%
    count(word, sort = TRUE) %>%
    head(70)
  
  if (nrow(palavras_cat_principal) > 0) {
    caminho_wordcloud <- file.path(CONFIG$dir_graficos, "05_wordcloud_bardin_principal.html")
    wc <- wordcloud2(data = palavras_cat_principal, size = 0.7, color = 'random-dark', backgroundColor = "#F8F9FA")
    htmlwidgets::saveWidget(wc, caminho_wordcloud, selfcontained = FALSE)
  }
}

# ---- Gráfico 6: Rede de Co-ocorrência ----
# PROPÓSITO: Visualizar as relações entre as palavras mais importantes do corpus.
top_terms <- names(topfeatures(dfm_obj, 30))
fcm_obj <- fcm(tokens_obj, context = "window", window = 5, tri = FALSE)
fcm_subset <- fcm_select(fcm_obj, pattern = top_terms)
set.seed(CONFIG$seed)
grafico_rede <- textplot_network(fcm_subset, min_freq = 0.8, edge_color = "#3498db", edge_alpha = 0.5, vertex_labelsize = 3) +
  labs(title = "Rede de Co-ocorrência de Palavras-Chave", subtitle = "Termos que aparecem frequentemente próximos nos relatos") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "gray20"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
        plot.background = element_rect(fill = "white", color = NA))
ggsave(file.path(CONFIG$dir_graficos, "06_rede_coocorrencia.png"), plot = grafico_rede, width = 10, height = 10, dpi = CONFIG$dpi, bg = "white")

# ---- Gráfico 7: Boxplot do Tamanho dos Relatos por Categoria ----
# PROPÓSITO: Comparar a distribuição do número de palavras dos comentários entre as categorias.
grafico_boxplot <- dados_categorizados %>%
  filter(categorias != "Sem Categoria") %>%
  ggplot(aes(x = fct_reorder(str_wrap(categorias, 20), n_palavras, .fun = median), y = n_palavras, fill = categorias)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.2, size = 1) +
  labs(title = "Distribuição do Tamanho dos Relatos por Categoria",
       subtitle = "Análise do número de palavras em cada comentário",
       x = "Categoria Temática",
       y = "Número de Palavras no Relato") +
  tema_tcc() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(CONFIG$dir_graficos, "07_boxplot_tamanho_relato.png"), plot = grafico_boxplot, width = 10, height = 8, dpi = CONFIG$dpi)

# ---- Gráfico 8: Gráfico de Dispersão (Similaridade vs Frequência) ----
# PROPÓSITO: Avaliar a força da correspondência entre os tópicos do LDA e as categorias de Bardin.
dados_dispersao <- topic_mapping %>%
  left_join(freq_categorias, by = c("categoria_mapeada" = "categorias")) %>%
  select(topic, categoria_mapeada, similaridade, palavras_comuns, n_freq_bardin = n)

grafico_dispersao <- dados_dispersao %>%
  filter(!is.na(n_freq_bardin)) %>%
  ggplot(aes(x = similaridade, y = n_freq_bardin, size = palavras_comuns)) +
  geom_point(alpha = 0.7, color = "#2980b9") +
  ggrepel::geom_text_repel(aes(label = str_wrap(categoria_mapeada, 20)), size = 3.5, box.padding = 0.5) +
  scale_size_continuous(name = "Palavras\nComuns") +
  labs(title = "Relação: Correspondência LDA-Bardin vs Frequência",
       subtitle = "Similaridade dos tópicos automáticos com as categorias manuais",
       x = "Similaridade (Jaccard)",
       y = "Frequência da Categoria (Bardin)") +
  tema_tcc()
ggsave(file.path(CONFIG$dir_graficos, "08_dispersao_similaridade_frequencia.png"), plot = grafico_dispersao, width = 10, height = 8, dpi = CONFIG$dpi)

# ---- Gráfico 9: Comparativo de Frequência - Bardin vs. LDA ----
# PROPÓSITO: O gráfico mais importante para a validação da metodologia híbrida. Compara lado a lado os resultados das duas abordagens.
freq_bardin_comp <- freq_categorias %>%
  filter(categorias != "Sem Categoria") %>%
  select(categoria = categorias, n_bardin = n)
freq_lda_comp <- freq_topicos_lda_mapeado %>%
  group_by(categoria_mapeada) %>%
  summarise(n_lda = sum(n), .groups = 'drop') %>%
  rename(categoria = categoria_mapeada)

dados_comparativos <- full_join(freq_bardin_comp, freq_lda_comp, by = "categoria") %>%
  pivot_longer(
    cols = c("n_bardin", "n_lda"),
    names_to = "metodologia",
    values_to = "frequencia"
  ) %>%
  mutate(
    metodologia = ifelse(metodologia == "n_bardin", "Bardin (Manual)", "LDA (Automático)")
  ) %>%
  filter(!is.na(frequencia))

grafico_comparativo <- ggplot(dados_comparativos, aes(x = fct_reorder(str_wrap(categoria, 30), frequencia, .fun = max), y = frequencia, fill = metodologia)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_text(aes(label = frequencia), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5, color = "gray20") +
  coord_flip() +
  scale_fill_manual(values = c("Bardin (Manual)" = "#2980b9", "LDA (Automático)" = "#c0392b")) +
  labs(
    title = "Validação Metodológica: Frequência de Categorias",
    subtitle = "Comparação entre Análise Manual (Bardin) e Análise Automática (LDA)",
    x = "Categoria Temática",
    y = "Número de Relatos",
    fill = "Método de Análise"
  ) +
  tema_tcc() +
  theme(legend.position = "top")
ggsave(file.path(CONFIG$dir_graficos, "09_comparativo_bardin_lda.png"), plot = grafico_comparativo, width = 12, height = 8, dpi = CONFIG$dpi)

# ---- Gráfico 10: Análise de Sentimentos por Categoria ----
# PROPÓSITO: Explorar a carga emocional (positiva, negativa, neutra) predominante em cada categoria.
sentimentos_df <- dados_categorizados %>%
  filter(categorias != "Sem Categoria") %>%
  unnest_tokens(palavra, texto_limpo) %>%
  inner_join(oplexicon_v3.0, by = c("palavra" = "term")) %>%
  count(categorias, polarity, sort = TRUE) %>%
  mutate(sentimento = case_when(
    polarity == 1  ~ "Positivo",
    polarity == -1 ~ "Negativo",
    polarity == 0  ~ "Neutro"
  ))

grafico_sentimentos <- sentimentos_df %>%
  ggplot(aes(x = n, y = fct_reorder(categorias, n, .fun = sum), fill = sentimento)) +
  geom_col(position = "fill", alpha = 0.8) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Positivo" = "#27ae60", "Negativo" = "#c0392b", "Neutro" = "#7f8c8d"),
                    name = "Polaridade do Sentimento") +
  labs(
    title = "Distribuição de Sentimentos nos Relatos por Categoria",
    subtitle = "Análise da carga emocional predominante em cada tema",
    x = "Proporção de Palavras",
    y = "Categoria Temática"
  ) +
  tema_tcc() +
  theme(legend.position = "top")
ggsave(file.path(CONFIG$dir_graficos, "10_sentimentos_por_categoria.png"), plot = grafico_sentimentos, width = 11, height = 8, dpi = CONFIG$dpi)

# ---- Gráfico 11: Termos Mais Relevantes por Categoria (TF-IDF) ----
# PROPÓSITO: Identificar palavras que são unicamente importantes para uma categoria em comparação com as outras.
palavras_tfidf <- dados_categorizados %>%
  filter(categorias != "Sem Categoria") %>%
  unnest_tokens(palavra, texto_limpo) %>%
  filter(!palavra %in% custom_stopwords, nchar(palavra) > 2) %>%
  count(categorias, palavra, sort = TRUE) %>%
  bind_tf_idf(palavra, categorias, n)

grafico_tfidf <- palavras_tfidf %>%
  group_by(categorias) %>%
  slice_max(tf_idf, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(palavra = reorder_within(palavra, tf_idf, categorias)) %>%
  ggplot(aes(x = tf_idf, y = palavra, fill = categorias)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~categorias, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Palavras-Chave Distintivas por Categoria (via TF-IDF)",
    subtitle = "Termos que são unicamente importantes para cada tema",
    x = "Importância do Termo (TF-IDF)",
    y = "Termo"
  ) +
  tema_tcc()
ggsave(file.path(CONFIG$dir_graficos, "11_tfidf_por_categoria.png"), plot = grafico_tfidf, width = 12, height = 9, dpi = CONFIG$dpi)


cat("Análise completa finalizada! Verifique a pasta 'resultados' e 'resultados/graficos'.\n")
cat("Foram geradas duas tabelas de exemplos no formato CSV:\n")
cat("- 'tabela_exemplos_categorias_bardin.csv' (para suas categorias manuais)\n")
cat("- 'tabela_exemplos_topicos_lda.csv' (para os tópicos automáticos)\n")
