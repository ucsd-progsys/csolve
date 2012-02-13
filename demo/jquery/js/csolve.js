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
  if ($(line).attr("num") in csolveData.errorLines) {
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
/********** Generating Identifier Annotation Tooltips ***********/ 
/****************************************************************/

var varToolTipTplt = 
   "<div id=\"vartooltip\" class=\"tooltip\" ident=${name} num=${line}>" 
 + "${name} on line ${line}" 
 + "This is a <a href=\"http://www.google.com\">hyperlink.</a>" 
 + "</div>"
 ;

/* Compile markup string as a named template */
$.template("varToolTipTplt", varToolTipTplt);

/* Render the named template */
$("#showBtn" ).click(function() {
  $("#movieList").empty();
  $.tmpl("movieTemplate", movies).appendTo( "#movieList" );
});
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
 
  //Generate tooltips for each ident, place after ident 
  //http://flowplayer.org/tools/demos/tooltip/any-html.html
  $("span[class='n']").each(function(){
    //HEREHEREHERE: get name
    //HEREHEREHERE: get line
    //HEREHEREHERE: create TT from template
    //HEREHEREHERE: insert after this
  });

  //Link tooltips for each identifier
  $("span[class='n']").tooltip({
  /*  tip      : '#vartooltip', */ 
      position : 'top right'
    , offset   : [0, 15]
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

