var redOn      = function(){$(this).addClass("red");};
var redOff     = function(){$(this).removeClass("red");};
var yellowOn   = function(){$(this).addClass("yellow");};
var yellowOff  = function(){$(this).removeClass("yellow");};

var getVarName = function(x){ return $(x).text();};
var getVarLine = function(x){ return $(x).closest("span[class='line']").attr("num"); }
var getVarInfo = function(x){ return (getVarName(x) + " at line: " + getVarLine(x)); }
var toggleText = { 'Hide': 'Show', 'Show': 'Hide' };

var hilitError = function(line){ 
  if ($(line).attr("num") in csolveData.errorLines) {
    $(line).addClass("errLine");
  }
}
  
$(document).ready(function(){

  $("#showlines").click(function(){
    $("span[class='linenum']").slideToggle(); 
    $(this).text(toggleText[$(this).text()]);
  });

  //Hover-Highlights Variables and Functions
  $("span[class='n']").hover(yellowOn, yellowOff);
  $("span[class='nf']").hover(yellowOn, yellowOff);

  //Nuke identifiers on click
  //$("span[class='n']").click(function(event){
  //  $(this).hide("slow");
  //});

  //Show Variable Info on click
  $("span[class='n']").click(function(event){
    $("#msg").text("Variable " + getVarInfo(this));
  });

  //Show Function Info on click
  $("span[class='nf']").click(function(event){
    $("#msg").text("Function " + getVarInfo(this));
  });

  //Color ErrorLines
  $("span[class='line']").each(function(){ 
    hilitError(this); 
  });

});

