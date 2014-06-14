+(function ($) {
    var $submit = $('#submit')
      , $output = $('#output')
      , $search = $('#search')

    $(submit).click(function () {
	var query = $search.val()
	$.getJSON("/query/"+query, function (res) {
	    var json = JSON.parse(res)
	      , title = json.track.title
	      , line  = json.phraseLines[0].line
	    $output.html(line)
	})
    })


}) (Zepto)
