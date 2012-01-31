/****************************************************************/
/************* Miscellaneous Globals ****************************/
/****************************************************************/

var lineSpans   = {};   //index of ALL line spans
var currentLine = null;
var toggleText  = { 'Hide': 'Show', 'Show': 'Hide' };

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

var helloYou = "Hello";

var getVarName = function(x){ 
  return $(x).text();
};

var getLine = function(x){ 
  return $(x).closest("span[class='line']").attr("num"); 
};

var getVarInfo = function(x){ 
  return (getVarName(x) + " at line: " + getLine(x)); 
};

/****************************************************************/
/**************** Accessing csolveData Information **************/
/****************************************************************/

var qualDef = function(n){ 
  return (csolveData.qualDef[n]); 
};


var genericAnnotVarLine = function(v, i){
  var a = csolveData.genAnnot;
  a.name = "No Information for " + v;
  return a;

} 

var annotVarLine = function(v, i) {
  var a = genericAnnotVarLine(v, i);
  if (v in csolveData.varAnnot) {
    if (i in csolveData.varAnnot[v]) {
      a = csolveData.varAnnot[v][i];
    }
  }
  a.line = i;
  return a;
};

var annotFun = function(fn) {
  if (fn in csolveData.funAnnot) {
    var a = csolveData.funAnnot[fn];
    if (a.args.length == 0) {
      a.argZero = true;
    } else if (a.args.length == 1) {
      a.argOne  = true;
    } else {
      a.argMany = true;
    }
    return a;
  }
  return null;
}

var listExists = function(f, xs){
  for (var i = 0; i < xs.length; i++) {
    if (f(xs[i])){ return true; };
  };
  return false;
}

var makeErrorLines = function(){
  csolveData.errorLines = {};
  for (j in csolveData.errors) {
    csolveData.errorLines[csolveData.errors[j].line] = true;
  };
}

var isErrorLine = function(i){
  return (i in csolveData.errorLines);
};

/****************************************************************/
/**************** Highlighting Lines ****************************/
/****************************************************************/

var hilitError = function(line){ 
  var lineNum = $(line).attr("num"); 
  if (isErrorLine(lineNum)) {
    $(line).addClass("errLine");
  };
};

var hilitCurrent = function(x){
  var line = $(x).closest("span[class='line']"); 
  $(line).addClass("curLine");
  if (currentLine) { $(currentLine).removeClass("curLine"); };
  currentLine = line;
};

/****************************************************************/
/**************** Top Level Rendering ***************************/
/****************************************************************/

$(document).ready(function(){
  $("#showlines").click(function(){
    $("a[class='linenum']").slideToggle(); 
    $(this).text(toggleText[$(this).text()]);
  });

  //Hover-Highlights Variables and Functions
  $("span[class='n']").hover(yellowOn, yellowOff);
  $("span[class='nf']").hover(yellowOn, yellowOff);
 
  //Generate tooltips for each ident, place after ident 
  //http://flowplayer.org/tools/demos/tooltip/any-html.html
  $("span[class='n']").each(function(){
    var name    = getVarName(this);
    var lineNum = getLine(this);
    var annot   = annotFun(name);
    if (annot) {
      $("#annotfTooltipTemplate").tmpl(annot).insertAfter(this);
    } else {
      annot = annotVarLine(name, lineNum); 
      $("#annotvTooltipTemplate").tmpl(annot).insertAfter(this);
    };
  });

  //Set tooltips for each identifier (shows: reftype)
  $("span[class='n']").tooltip({
      position : 'top right'
    , offset   : [10, -10]
    , delay    : 50
    , effect   : 'slide'
  });
 
  //Set tooltips for each qualname (shows: expanded qual-def) 
  $("span[class='qname']").tooltip({
      position : 'right'
    , offset   : [-190, -95]
    , delay    : 50
    , effect   : 'slide'
  });
 
  //Set tooltips for each qualargname (shows: var-def-info) 
  $("span[class='qarg']").tooltip({
      position : 'right'
    , offset   : [-190, -95]
    , delay    : 50
    , effect   : 'slide'
  });

  //Color ErrorLines
  makeErrorLines();
  $("span[class='line']").each(function(){ 
    hilitError(this); 
  });

  //Click "selects" a line
  $("a[class='linenum']").click(function(evt){
    hilitCurrent(this);
  });

  //Create LineSpans
  $("span[class='line'").each(function(){
    var lineName = $(this).attr("name");
    lineSpans[lineName] = this;
  });

  //Re-highlight Line (Hash) Change
  //HEREHEREHERE: need to download from
  //http://benalman.com/projects/jquery-hashchange-plugin/
  $(window).hashchange(function(){
    hash = location.hash;
    if (hash){ // hash is not # 
      var lineName = hash.substring(1);
      var line     = lineSpans[lineName];
      hilitCurrent(line); 
    }
  }

  //Nuke identifiers on click
  //$("span[class='n']").click(function(event){
  //  $(this).hide("slow");
  //});

  //Show Variable Info on click
  //$("span[class='n']").click(function(event){
  //  $("#msg").text("Variable " + getVarInfo(this));
  //});

  //Show Function Info on click
  //$("span[class='nf']").click(function(event){
  //  $("#msg").text("Function " + getVarInfo(this));
  //});

  //$( "#movieTemplate" ).tmpl( movies ).appendTo( "#movieList" );

});

