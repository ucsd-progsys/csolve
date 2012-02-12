var redOn     = function(){$(this).addClass("red");};
var redOff    = function(){$(this).removeClass("red");};
var yellowOn  = function(){$(this).addClass("yellow");};
var yellowOff = function(){$(this).removeClass("yellow");};

$(document).ready(function(){
 
  //$("a[href*='ucsd.edu']").click(function(event){
  //  event.preventDefault();
  //  $(this).hide("slow");
  //});

  $("a[href*='ucsd.edu']").hover(redOn, redOff);
 
  $("span[class='line']").hover(yellowOn, yellowOff);

//  $("span[class*='line']").hover(
//      function(){$(this).addClass("green");}
//     ,function(){$(this).removeClass("green");}
//  );
//
//  $("span[class*='line']").hover(
//      function(){$(this).addClass("green");}
//     ,function(){$(this).removeClass("green");}
//  );

});

