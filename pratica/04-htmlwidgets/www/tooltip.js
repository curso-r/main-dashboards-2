var target_ano = document.getElementById("filtro_ano");

tippy('#pesquisar', {
  content: 'Clique para atualizar os resultados',
  triggerTarget: [target_ano],
  trigger: 'click',
  hideOnClick: false,
  placement: 'right'
});

var botao = document.getElementById("pesquisar");
botao.onclick = function() {
  tippy.hideAll()
}
