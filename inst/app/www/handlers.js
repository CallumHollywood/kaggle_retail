$( document ).ready(function() {

  Shiny.addCustomMessageHandler('doughnut_toast', function(msg) {
    Toastify({
      text: msg,
      gravity: "bottom",
      position: 'center'
    }).showToast();
  })

});
