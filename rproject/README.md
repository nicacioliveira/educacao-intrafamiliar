## 📚 **Bibliotecas utilizadas na Análise de Bardin**

Aqui está um resumo sucinto das bibliotecas para seu README:

### **🔍 Análise de Texto e Processamento**
- **`quanteda`** - Framework principal para análise quantitativa de texto, criação de corpus e matrizes documento-termo
- **`quanteda.textstats`** - Extensão do quanteda para estatísticas textuais (frequência de palavras, diversidade lexical)
- **`tidytext`** - Integração entre text mining e tidyverse, facilita análise de sentimentos e manipulação de tokens

### **🤖 Machine Learning e Topic Modeling**  
- **`topicmodels`** - Implementação de algoritmos LDA (Latent Dirichlet Allocation) para descoberta automática de tópicos

### **📊 Manipulação e Visualização de Dados**
- **`tidyverse`** - Conjunto de pacotes (dplyr, ggplot2, stringr, etc.) para manipulação e visualização de dados
- **`lubridate`** - Manipulação de datas e timestamps dos comentários
- **`jsonlite`** - Exportação de resultados em formato JSON

### **☁️ Visualizações Avançadas**
- **`wordcloud2`** - Criação de nuvens de palavras interativas e estáticas
- **`wordcloud`** - Nuvens de palavras tradicionais (fallback)
- **`igraph`** - Análise e visualização de redes de co-ocorrência de palavras
- **`ggraph`** - Visualização de grafos integrada ao ggplot2

### **🎨 Paletas e Cores**
- **`RColorBrewer`** - Paletas de cores científicas para wordclouds e visualizações

---

### **💡 Arquitetura da Solução:**

```
Dados Brutos (CSV) 
    ↓ tidyverse
Corpus Filtrado 
    ↓ quanteda
Tokens + DFM 
    ↓ quanteda.textstats
Frequências + Estatísticas
    ↓ topicmodels
Validação LDA
    ↓ tidytext + tidyverse  
Análise de Sentimentos
    ↓ ggplot2 + wordcloud + igraph
Visualizações Finais
```

### **🎯 Metodologia Híbrida:**
- **Bardin Clássica**: Categorização teórica baseada em literatura
- **Text Mining**: Processamento automatizado com quanteda
- **Machine Learning**: Validação cruzada com LDA
- **Data Science**: Pipeline reproduzível com tidyverse

**Total: ~10 bibliotecas principais integrando métodos qualitativos e quantitativos** 🚀
