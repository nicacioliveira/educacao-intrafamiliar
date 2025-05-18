const fs = require('fs');
const path = require('path');
const { JSDOM } = require('jsdom');

// Caminho para o arquivo HTML
const htmlFilePath = './html/html-comentarios.html';
const resultadosDir = './resultados/';

// Verifica e cria o diretório de resultados se não existir
if (!fs.existsSync(resultadosDir)) {
  console.log(`Criando diretório de resultados: ${resultadosDir}`);
  fs.mkdirSync(resultadosDir, { recursive: true });
}

// Função para limpar o texto e remover espaços extras
function limparTexto(texto) {
  if (!texto) return '';
  
  // Remove as quebras de linha e múltiplos espaços
  return texto
    .replace(/\s+/g, ' ')    // Substitui sequências de espaços por um único espaço
    .trim();                  // Remove espaços no início e fim
}

// Função para extrair comentários
async function extrairComentariosInstagram() {
  console.log(`Processando arquivo: ${htmlFilePath}`);
  
  // Verifica se o arquivo HTML existe
  if (!fs.existsSync(htmlFilePath)) {
    throw new Error(`Arquivo HTML não encontrado: ${htmlFilePath}`);
  }
  
  // Lê o arquivo HTML
  const htmlContent = fs.readFileSync(htmlFilePath, 'utf-8');
  console.log(`Arquivo lido: ${(htmlContent.length / 1024 / 1024).toFixed(2)}MB`);
  
  // Cria um DOM a partir do HTML
  const dom = new JSDOM(htmlContent);
  const document = dom.window.document;
  
  // Array para armazenar os comentários extraídos
  const comentarios = [];
  
  // Encontra os containers de comentários
  const containerComentarios = document.querySelectorAll('.xurb0ha');
  console.log(`Encontrados ${containerComentarios.length} possíveis comentários`);
  
  // Extrai dados dos comentários
  containerComentarios.forEach((container, index) => {
    try {
      // Usuário
      const elementoUsuario = container.querySelector('a[role="link"] span');
      const usuario = elementoUsuario ? elementoUsuario.textContent.trim() : 'desconhecido';
      
      // Link do perfil do usuário
      const elementoLink = container.querySelector('a[role="link"]');
      const perfilLink = elementoLink ? elementoLink.getAttribute('href').replace(/\//g, '') : '';
      
      // Texto do comentário
      const elementoComentario = container.querySelector('span[dir="auto"] > div > span[dir="auto"]');
      const textoComentarioRaw = elementoComentario ? elementoComentario.textContent : '';
      const textoComentario = limparTexto(textoComentarioRaw);
      
      // Timestamp
      const elementoTempo = container.querySelector('time');
      const timestamp = elementoTempo ? elementoTempo.getAttribute('datetime') : '';
      const tempoExibido = elementoTempo ? elementoTempo.textContent.trim() : '';
      
      // Título do elemento time (data completa)
      const dataFormatada = elementoTempo ? elementoTempo.getAttribute('title') : '';
      
      // Likes
      const elementoLikes = container.querySelector('a[href*="liked_by"]');
      // Limpa bem o texto de likes para pegar apenas o número
      let likes = '0';
      if (elementoLikes) {
        const likesText = elementoLikes.textContent.trim();
        // Extrai somente o número de likes
        const likesMatch = likesText.match(/(\d+)/);
        if (likesMatch && likesMatch[1]) {
          likes = likesMatch[1];
        }
      }
      
      // Adiciona ao array apenas se tiver usuário e comentário
      if (usuario && textoComentario && usuario !== 'desconhecido') {
        comentarios.push({
          usuario,
          perfilLink,
          textoComentario,
          timestamp,
          dataFormatada,
          tempoExibido,
          likes
        });
      }
    } catch (erro) {
      console.error(`Erro ao processar comentário #${index}:`, erro);
    }
  });
  
  // Se não encontrou comentários, tenta outro seletor
  if (comentarios.length < 5) {
    console.log('Tentando método alternativo...');
    
    // Procura por estruturas alternativas de comentários
    const blocosComentarios = document.querySelectorAll('div._ac6x._ac6y');
    console.log(`Encontrados ${blocosComentarios.length} blocos alternativos`);
    
    blocosComentarios.forEach((bloco, index) => {
      try {
        // Usuário
        const elementoUsuario = bloco.querySelector('a[href*="/"] span[dir="auto"]');
        if (!elementoUsuario) return;
        
        const usuario = elementoUsuario.textContent.trim();
        
        // Link do perfil do usuário
        const elementoLink = bloco.querySelector('a[href*="/"]');
        const perfilLink = elementoLink ? elementoLink.getAttribute('href').replace(/\//g, '') : '';
        
        // Texto do comentário
        const spansComentario = bloco.querySelectorAll('span[dir="auto"]');
        let textoComentarioRaw = '';
        
        // Geralmente o último span contém o texto do comentário
        if (spansComentario.length > 1) {
          textoComentarioRaw = spansComentario[spansComentario.length - 1].textContent;
        }
        
        const textoComentario = limparTexto(textoComentarioRaw);
        
        // Timestamp
        const elementoTempo = bloco.querySelector('time');
        const timestamp = elementoTempo ? elementoTempo.getAttribute('datetime') : '';
        const tempoExibido = elementoTempo ? elementoTempo.textContent.trim() : '';
        
        // Título do elemento time (data completa)
        const dataFormatada = elementoTempo ? elementoTempo.getAttribute('title') : '';
        
        // Likes - nova abordagem para extrair apenas o número
        let likes = '0';
        const elementoLikes = bloco.querySelector('a[href*="liked_by"]');
        if (elementoLikes) {
          const likesText = elementoLikes.textContent.trim();
          const likesMatch = likesText.match(/(\d+)/);
          if (likesMatch && likesMatch[1]) {
            likes = likesMatch[1];
          }
        }
        
        // Adiciona ao array
        if (usuario && textoComentario && usuario !== textoComentario) {
          const duplicado = comentarios.some(
            comentario => comentario.usuario === usuario && comentario.textoComentario === textoComentario
          );
          
          if (!duplicado) {
            comentarios.push({
              usuario,
              perfilLink,
              textoComentario,
              timestamp,
              dataFormatada,
              tempoExibido,
              likes
            });
          }
        }
      } catch (erro) {
        console.error(`Erro ao processar bloco #${index}:`, erro);
      }
    });
  }
  
  // Terceiro método - mais genérico
  if (comentarios.length < 5) {
    console.log('Tentando método mais genérico...');
    
    // Tenta selecionar qualquer elemento que pareça um comentário
    const todosElementos = document.querySelectorAll('div.x9f619.xjbqb8w.x78zum5.x168nmei.x13lgxp2.x5pf9jr.xo71vjh');
    console.log(`Encontrados ${todosElementos.length} possíveis elementos de comentário`);
    
    todosElementos.forEach((elemento, index) => {
      try {
        // Só processa se tiver pelo menos 2 spans com dir="auto"
        const spans = elemento.querySelectorAll('span[dir="auto"]');
        if (spans.length < 2) return;
        
        // Usuário
        const elementoUsuario = elemento.querySelector('a[role="link"] span');
        if (!elementoUsuario) return;
        
        const usuario = elementoUsuario.textContent.trim();
        
        // Link do perfil do usuário
        const elementoLink = elemento.querySelector('a[role="link"]');
        const perfilLink = elementoLink ? elementoLink.getAttribute('href').replace(/\//g, '') : '';
        
        // Texto do comentário
        const ultimoSpan = spans[spans.length - 1];
        const textoComentarioRaw = ultimoSpan.textContent;
        const textoComentario = limparTexto(textoComentarioRaw);
        
        // Timestamp
        const elementoTempo = elemento.querySelector('time');
        const timestamp = elementoTempo ? elementoTempo.getAttribute('datetime') : '';
        const tempoExibido = elementoTempo ? elementoTempo.textContent.trim() : '';
        
        // Título do elemento time (data completa)
        const dataFormatada = elementoTempo ? elementoTempo.getAttribute('title') : '';
        
        // Likes - nova abordagem
        let likes = '0';
        const elementoLikes = elemento.querySelector('a[href*="liked_by"]');
        if (elementoLikes) {
          const likesText = elementoLikes.textContent.trim();
          const likesMatch = likesText.match(/(\d+)/);
          if (likesMatch && likesMatch[1]) {
            likes = likesMatch[1];
          }
        }
        
        // Adiciona ao array
        if (usuario && textoComentario && usuario !== textoComentario) {
          const duplicado = comentarios.some(
            comentario => comentario.usuario === usuario && comentario.textoComentario === textoComentario
          );
          
          if (!duplicado) {
            comentarios.push({
              usuario,
              perfilLink,
              textoComentario,
              timestamp,
              dataFormatada,
              tempoExibido,
              likes
            });
          }
        }
      } catch (erro) {
        console.error(`Erro ao processar elemento #${index}:`, erro);
      }
    });
  }
  
  console.log(`Total de ${comentarios.length} comentários extraídos`);
  
  // Cria o diretório de resultados caso ainda não exista
  try {
    if (!fs.existsSync(resultadosDir)) {
      fs.mkdirSync(resultadosDir, { recursive: true });
    }
  } catch (erro) {
    console.error(`Erro ao criar diretório de resultados: ${erro.message}`);
    // Usa o diretório atual como fallback
    resultadosDir = './';
  }
  
  // Abordagem alternativa para gerar CSV - usar biblioteca externa para não ter problemas
  let conteudoCSV = '';
  
  // Função para escapar corretamente campos CSV
  function escaparCampoCSV(campo) {
    if (campo === null || campo === undefined) {
      return '';
    }
    
    // Converte para string, caso não seja
    const str = String(campo);
    
    // Se tiver aspas, vírgulas ou quebras de linha, coloca entre aspas e duplica as aspas
    if (str.includes('"') || str.includes(',') || str.includes('\n') || str.includes('\r')) {
      return '"' + str.replace(/"/g, '""') + '"';
    }
    
    return str;
  }
  
  // Adiciona cabeçalhos
  const cabecalhos = ['usuario', 'perfilLink', 'textoComentario', 'timestamp', 'dataFormatada', 'tempoExibido', 'likes'];
  conteudoCSV = cabecalhos.map(escaparCampoCSV).join(',') + '\n';
  
  // Adiciona linhas de dados
  comentarios.forEach(comentario => {
    const linha = [
      comentario.usuario,
      comentario.perfilLink,
      comentario.textoComentario,
      comentario.timestamp,
      comentario.dataFormatada,
      comentario.tempoExibido,
      comentario.likes
    ].map(escaparCampoCSV).join(',');
    
    conteudoCSV += linha + '\n';  // Adiciona quebra de linha explícita
  });
  
  try {
    // Salva o arquivo CSV
    const arquivoSaida = path.join(resultadosDir, 'comentarios_instagram.csv');
    fs.writeFileSync(arquivoSaida, conteudoCSV, 'utf-8');
    console.log(`Arquivo CSV salvo como: ${arquivoSaida}`);
  } catch (erro) {
    console.error(`Erro ao salvar arquivo CSV: ${erro.message}`);
  }
  
  // Mostra exemplos dos comentários extraídos
  if (comentarios.length > 0) {
    console.log('\nExemplos de comentários extraídos:');
    comentarios.slice(0, 3).forEach((comentario, index) => {
      console.log(`\nComentário #${index + 1}:`);
      console.log(`- Usuário: ${comentario.usuario}`);
      console.log(`- Perfil: ${comentario.perfilLink}`);
      console.log(`- Texto: ${comentario.textoComentario.substring(0, 50)}${comentario.textoComentario.length > 50 ? '...' : ''}`);
      console.log(`- Data ISO: ${comentario.timestamp}`);
      console.log(`- Data formatada: ${comentario.dataFormatada}`);
      console.log(`- Tempo relativo: ${comentario.tempoExibido}`);
      console.log(`- Likes: ${comentario.likes}`);
    });
  }
  
  try {
    // Também gera um arquivo TSV (valores separados por tabulação)
    // TSV geralmente funciona melhor para textos com possíveis quebras de linha
    let conteudoTSV = '';
    
    // Função para escapar campos TSV
    function escaparCampoTSV(campo) {
      if (campo === null || campo === undefined) {
        return '';
      }
      
      // Converte para string
      return String(campo)
        .replace(/\t/g, ' ')    // Substitui tabs por espaços
        .replace(/\n/g, ' ')    // Substitui quebras de linha por espaços
        .replace(/\r/g, ' ');   // Substitui retornos de carro por espaços
    }
    
    // Adiciona cabeçalhos TSV
    conteudoTSV = cabecalhos.map(escaparCampoTSV).join('\t') + '\n';
    
    // Adiciona linhas de dados TSV
    comentarios.forEach(comentario => {
      const linha = [
        comentario.usuario,
        comentario.perfilLink,
        comentario.textoComentario,
        comentario.timestamp,
        comentario.dataFormatada,
        comentario.tempoExibido,
        comentario.likes
      ].map(escaparCampoTSV).join('\t');
      
      conteudoTSV += linha + '\n';
    });
    
    // Salva o arquivo TSV
    const arquivoTSV = path.join(resultadosDir, 'comentarios_instagram.tsv');
    fs.writeFileSync(arquivoTSV, conteudoTSV, 'utf-8');
    console.log(`Arquivo TSV salvo como: ${arquivoTSV}`);
  } catch (erro) {
    console.error(`Erro ao salvar arquivo TSV: ${erro.message}`);
  }
  
  try {
    // Salva também em formato JSON para facilitar análises futuras
    const arquivoJson = path.join(resultadosDir, 'comentarios_instagram.json');
    fs.writeFileSync(arquivoJson, JSON.stringify(comentarios, null, 2), 'utf-8');
    console.log(`Arquivo JSON salvo como: ${arquivoJson}`);
  } catch (erro) {
    console.error(`Erro ao salvar arquivo JSON: ${erro.message}`);
  }
  
  return comentarios;
}

// Executa a função
extrairComentariosInstagram()
  .then(() => {
    console.log('Processamento concluído com sucesso!');
  })
  .catch(erro => {
    console.error('Erro durante a extração:', erro);
    process.exit(1); // Sai com código de erro
  });
