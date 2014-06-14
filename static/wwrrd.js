/* jshint asi:true, laxcomma:true, expr:true */
+(function ($) {
    "use strict";
    var $submit = $('#submit')
      , $output = $('#output')
      , $search = $('#search')

    $(submit).click(function () {
	$.getJSON("/query/"+$search.val(), function (res) {
	    var json = JSON.parse(res)
	      , title = json.track.title
	      , line  = json.phraseLines[0].line
	    $output.html(line)
	})
    })
}) (Zepto)
