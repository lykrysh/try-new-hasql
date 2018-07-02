$(document).ready(function() {

  $('a[data-stateName]').click(function() {
    var state = $(this).attr('data-stateName');
    var on = "off";
    if ($(this).hasClass('buton')) on = "on";
    $.ajax({
      url: '/filters',
      data: {
        filter : state,
        toggled : on,
        format: 'json'
      },
      error: function() {
        $('#info').html('<p>An error has occurred</p>');
      },
      dataType: 'text',
      success: function(data) {
         $('#info').html(state);
         $('#info').css('background','#999999');
      },
      type: 'GET'
    });
  });
  
  $('.but').click(function() {
    $(this).toggleClass('buton');
  });

});
