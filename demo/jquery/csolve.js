/****************************************************************/
/************* Miscellaneous Globals ****************************/
/****************************************************************/

var toggleText = { 'Hide': 'Show', 'Show': 'Hide' };

/****************************************************************/
/************* CSS Helpers **************************************/
/****************************************************************/

var redOn      = function () { $(this).addClass("red"); };
var redOff     = function () { $(this).removeClass("red"); };
var yellowOn   = function () { $(this).addClass("yellow"); };
var yellowOff  = function () { $(this).removeClass("yellow"); };

/****************************************************************/
/************* Accessing Selected Span Information **************/
/****************************************************************/

var getVarName = function(x){ 
  return $(x).text();
};

var getVarLine = function(x){ 
  return $(x).closest("span[class='line']").attr("num"); 
};

var getVarInfo = function(x){ 
  return (getVarName(x) + " at line: " + getVarLine(x)); 
};

var hilitError = function(line){ 
  if ($(line).attr("num") in csolveData.errorLines) 
  {
    $(line).addClass("errLine");
  };
};

/****************************************************************/
/**************** Accessing csolveData Information **************/
/****************************************************************/

var annotVarLine = function(v, line) {
  try { return csolveData.annot[v][line] }
  catch(err) { return null };
};

var isErrorLine = function(i){
  return (i in csolveData.errorLines);
};

/****************************************************************/
/**************** Accessing csolveData Information **************/
/****************************************************************/


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

