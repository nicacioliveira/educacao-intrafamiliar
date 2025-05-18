window.stopMoreClicking = false;

function clickMoreButtonsRecursively() {
  if (window.stopMoreClicking) {
    console.log("Script interrompido pelo usuário");
    return;
  }

  const allSpans = document.querySelectorAll('span');
  const moreButtons = Array.from(allSpans).filter(span => 
    span.textContent.trim() === "more" && 
    span.offsetParent !== null
  );
  
  console.log(`Encontrados ${moreButtons.length} botões "more"`);
  
  if (moreButtons.length === 0) {
    console.log("Nenhum botão 'more' encontrado. Verificando novamente em 2 segundos...");
    setTimeout(clickMoreButtonsRecursively, 2000);
    return;
  }
  
  // Clica em cada botão encontrado
  let clickCount = 0;
  moreButtons.forEach((button, index) => {
    setTimeout(() => {
      if (window.stopMoreClicking) return;
      
      button.click();
      clickCount++;
      console.log(`Clicado em ${clickCount}/${moreButtons.length} botões`);
      
      // Quando terminar, verifica novamente após um intervalo
      if (clickCount === moreButtons.length) {
        console.log("Verificando por novos botões em 2 segundos...");
        setTimeout(clickMoreButtonsRecursively, 2000);
      }
    }, index * 300);
  });
}

// Inicia o processo
clickMoreButtonsRecursively();
