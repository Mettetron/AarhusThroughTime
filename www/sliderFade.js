$(document).ready(function() {
	$('#contrast').on('input', function() {
    $('#photo img.top').css('opacity', $(this).val());
});
});	
