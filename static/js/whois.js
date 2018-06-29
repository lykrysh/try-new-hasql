$(document).ready(function() {

  $.getJSON('http://jsonip.com', function(client) {
    var cip = client.ip;
    $.ajax({
      url: '/whois',
      data: { ip : cip, format: 'json'},
      error: function() {
        $('#whois').html('<p>An error has occurred</p>');
      },
      dataType: 'text',
      success: function(data) {
         $('#whois').html(cip);
      },
      type: 'GET'
    });
  });

});
