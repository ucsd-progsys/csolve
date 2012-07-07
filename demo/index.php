<?
$entered_program = 0;
$demo = "demo_test01p";

$category = array(
  "test01p" 	=> "demo_test01p", 
);

$category = str_replace(" ", " ", $category);

function getWarns($logfile){
  $warns = "";
  $wflag  = 0;
  $fh = fopen($logfile, 'r');
  while (!feof($fh)){
    $s = fgets($fh);
    if (strpos($s,"exec:") !== false){
      $wflag = 0;
    }
    if ($wflag == 1){
      $warns = $warns . $s;
    }
    if (strpos($s,"Errors") !== false){
      $wflag = 1;
    }

  }
  fclose($fh);
  if ($warns == ""){
    $warns = "<h3>Program Safe</h3>";
  } else {
    $warns = "<h3>Warnings</h3> <pre> ".$warns."</pre>";
  }
  return $warns; 
}

function getAnnots($htmlfile){
  $annothtml = "<h3> Annotated Program </h3>" ;
  $annothtml = $annothtml."Hover over variable to see inferred type." ;
  $annothtml = $annothtml.(file_get_contents($htmlfile));
  return $annothtml;
}

function getRawTextFromField($fld){
  return stripslashes($_POST[$fld]);
}

function writeTextFile($fname,$fld){
  $f = fopen($fname, "w");
  fwrite($f,getRawTextFromField($fld));
  fclose($f);
}

function getFieldOrFile ($entered_program, $field, $filename) {
  if($entered_program == 1) {
    return (htmlspecialchars (getRawTextFromField ($field)));
  }
  else {
    return (htmlspecialchars (file_get_contents ($filename)));
  }
}

  if($_POST['chooseform'] == "1") {
    $demo = $category[$_POST['choosedemo']]; 
  }

  if($_POST['programform'] == "1") {
    $tb    = tempnam ("/tmp/csolve-demo", "csolve-demo-");
    $tc    = $tb    . ".c";
    $tobj  = $tb    . ".o";
    $tann  = $tobj  . ".annot";
    $thq   = $tobj  . ".hquals";
    $thtml = $tobj  . ".html";
    $log   = $tobj  . ".log";
    writeTextFile($tc,  'program');
    writeTextFile($thq, 'qualifiers');
    $out = shell_exec("../src/csolve -c ".$tc." -o ".$tobj." 2>&1");
    $annothtml = getAnnots($thtml);
    // $loghtml   = "<a href=\"".$logfile."\"> <h3>Log</h3> </a>";
    $entered_program = 1;
    //shell_exec("rm -f ".$tc."*");
  }
?>

<html>
<head>
  <title>CSolve Demo</title>
  <link rel="stylesheet" type="text/css" media="screen" href="css/pyg_default.css" />
  <link rel="stylesheet" type="text/css" media="screen" href="css/csolve.css" />
  <script src="http://cdn.jquerytools.org/1.2.6/full/jquery.tools.min.js"></script>
  <script src="http://ajax.microsoft.com/ajax/jquery.templates/beta1/jquery.tmpl.min.js"></script>
  <script src="js/jquery.ba-hashchange.min.js"></script>
  <script src="js/csolve.js"></script>
  <style type="text/css">
    .hidden {
      visibility: hidden;
      height: 0;
      width: 0;
    }
  </style>
</head>
<body>
  <h1>CSolve Demo</h1>
  <hr />

<h3>Pick a demo</h3>

<form action='<? echo $_SERVER['PHP_SELF']; ?>' 
      method='post'><p>

<select name="choosedemo">
<? foreach ($category as $key => $value){
     if ($value == $demo) { 
       echo '<OPTION selected = "yes" value='.$key.'> '.$key.''; 
     } else{
       echo '<OPTION value='.$key.'> '.$key.''; 
     }
  } 
?>   
<input name='chooseform' type='hidden' value='1'>
<input type='submit' value='choose'>
</select>
</form>

<script>
  function setTextAreaValue (id, editor) {
    document.getElementById (id).value = editor.getSession ().getValue ();
  }
</script>

<form action='<? echo $_SERVER['PHP_SELF']; ?>' 
      method='post'
      onSubmit='setTextAreaValue ("program", editor); setTextAreaValue ("qualifiers", qualEditor)'><p>

<h3>Predicate Templates</h3>

<div id="qualifiers-editor" style="position: relative; width: 60em; height: 10em"><?
  echo (getFieldOrFile ($entered_program, 'qualifiers', $demo.".c.hquals"));
?></div>
<textarea id='qualifiers' name='qualifiers' class="hidden"></textarea>

<h3>C Program</h3>

<div id="program-editor" style="position: relative; width: 60em; height: 30em"><?
  echo (getFieldOrFile ($entered_program, 'program', $demo.".c"));
?></div>
<textarea id='program' name='program' class="hidden"></textarea>

<script src="ace/ace.js" type="text/javascript" charset="utf-8"></script>
<script src="ace/mode-c_cpp.js" type="text/javascript" charset="utf-8"></script>
<script>
  var qualEditor = ace.edit ("qualifiers-editor");
  var editor = ace.edit ("program-editor");
  var CMode = require ("ace/mode/c_cpp").Mode;
  editor.getSession ().setMode (new CMode ());
</script>

<br />

<input name='programform' type='hidden' value='1'>
<input type='submit' value='csolve'>
</p></form>

<hr />
<? echo $annothtml ?>
</body>
</html>
