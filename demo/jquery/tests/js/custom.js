$(document).ready(function(){
  
  $("a").addClass("test");

  $("a").click(function(event){
    event.preventDefault();
    $(this).hide("slow");
  });

  $("a").click(function(event){
    event.preventDefault();
  });

  $('#faq').find('dd').hide().end()
           .find('dt').click(function(){$(this).next().slideToggle();});

});

