$(document).ready(function() {

$('#nonfiction').click(function() {
   $.ajax({
     url: '/nonfiction',
      data: {
         format: 'json'
      },
      error: function() {
         $('#info').html('<p>An error has occurred</p>');
      },
      dataType: 'text',
      success: function(data) {
         $('#info').html("NONFICTION GET");
         $('#info').css('background','#999999');
      },
      type: 'GET'
   });
});
$('#fiction').click(function() {
   $.ajax({
      url: 'https://api.joind.in/v2.1/talks/10889',
      data: {
         format: 'json'
      },
      error: function() {
         $('#info').html('<p>An error has occurred</p>');
      },
      dataType: 'jsonp',
      success: function(data) {
         var $description = $('<p>').text(data.talks[0].talk_description);
         $('#info').html($description);
         $('#info').css('background','#ff9900');
      },
      type: 'GET'
   });
});
$('#surreal').click(function() {
   $.ajax({
      url: 'https://api.joind.in/v2.1/talks/10889',
      data: {
         format: 'json'
      },
      error: function() {
         $('#info').html('<p>An error has occurred</p>');
      },
      dataType: 'jsonp',
      success: function(data) {
         var $description = $('<p>').text(data.talks[0].talk_description);
         $('#info').html($description);
         $('#info').css('background','#cccc00');
      },
      type: 'GET'
   });
});
$('#abstraction').click(function() {
   $.ajax({
      url: 'https://api.joind.in/v2.1/talks/10889',
      data: {
         format: 'json'
      },
      error: function() {
         $('#info').html('<p>An error has occurred</p>');
      },
      dataType: 'jsonp',
      success: function(data) {
         var $description = $('<p>').text(data.talks[0].talk_description);
         $('#info').html($description);
         $('#info').css('background','#dfdfdf');
      },
      type: 'GET'
   });
});
$('#soundscape').click(function() {
   $.ajax({
      url: 'https://api.joind.in/v2.1/talks/10889',
      data: {
         format: 'json'
      },
      error: function() {
         $('#info').html('<p>An error has occurred</p>');
      },
      dataType: 'jsonp',
      success: function(data) {
         var $title = $('<h1>').text(data.talks[0].talk_title);		
         $('#info').html($title);
         $('#info').css('background','#00cc00');
      },
      type: 'GET'
   });
});

	$("a[data-stateName]").click(function() {
		var state = $(this).attr("data-stateName");
		$("#viewall").text(state);

	});

	$('.but').click(function() {
		$(this).toggleClass('buton'); 
	});
});
