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

/****************************************************************/
/**************** Accessing csolveData Information **************/
/****************************************************************/

var qualDef = function(n){ 
  return ("This is qualifier " + n); 
};

var annotVarLine = function(v, i) {
  var q1 = { name : "Pos"
           , args : ["VV", "x", "y", "z"] 
           , url  : "http://www.google.com" };

  var q2 = { name : "Neg"
           , args : ["VV", "x"]
           , url  : "http://nytimes.com" };
  
  return { name   : v
         , line   : i
         , oq     : q1
         , quals  : [q1, q2 ] /* ["cat", "dog", "mouse"] */ 
         }; 
  //REAL
  //try { return csolveData.annot[v][line] }
  //catch(err) { return null };
};

var isErrorLine = function(i){
  return (i in csolveData.errorLines);
};

/****************************************************************/
/**************** Accessing csolveData Information **************/
/****************************************************************/

var hilitError = function(line){ 
  var lineNum = $(line).attr("num"); 
  if (isErrorLine(lineNum)) {
    $(line).addClass("errLine");
    //if ($(line).attr("num") in csolveData.errorLines) {
  };
};

/****************************************************************/
/**************** Top Level Rendering ***************************/
/****************************************************************/


$(document).ready(function(){

  $("#showlines").click(function(){
    $("span[class='linenum']").slideToggle(); 
    $(this).text(toggleText[$(this).text()]);
  });

  //Hover-Highlights Variables and Functions
  $("span[class='n']").hover(yellowOn, yellowOff);
  $("span[class='nf']").hover(yellowOn, yellowOff);
 
  //Generate tooltips for each ident, place after ident 
  //http://flowplayer.org/tools/demos/tooltip/any-html.html
  $("span[class='n']").each(function(){
    var name    = getVarName(this);
    var lineNum = getVarLine(this);
    var annot   = annotVarLine(name, lineNum); 
    $("#varTooltipTemplate").tmpl(annot).insertAfter(this);
  });

  //Link tooltips for each identifier
  $("span[class='n']").tooltip({
      position : 'top right'
    , offset   : [10, -10]
    , delay    : 50
    , effect   : 'slide'
  });
  
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

