$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('toggle-dark-mode', function(enabled) {
    document.body.classList.toggle('dark-mode', enabled);
  });
});
