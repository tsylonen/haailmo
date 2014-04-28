var DEFAULT_PEOPLE = 2;
var MAX_PEOPLE = 6;

$ = jQuery;
$(document).ready(function(){
    
    var cur_people = DEFAULT_PEOPLE;
    $(".addperson").click(function() {
        if(cur_people < MAX_PEOPLE) {
            cur_people += 1;
            $(".person" + cur_people).slideDown();
        } 
        if (cur_people === MAX_PEOPLE) {
            $(".addperson").hide();
        }
    });
});
