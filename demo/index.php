<?
$entered_program = 0;

$category = array(
  "arraysum"    => "arraysum",
  "csv"         => "csv",
  "mergesort"   => "mergesort",
  "stringlist" 	=> "stringlist",
  "sumreduce"   => "sumreduce",
);

$demo = "stringlist";

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
    $status = 0;
    $out    = array();
    exec("../src/csolve --web-demo -c ".$tc." -o ".$tobj." 2>&1", $out, $status);
    $annothtml = file_get_contents ($thtml);
    $entered_program = 1;
  }

  if($_POST['chooseform'] == "1" || $entered_program) {
    $demo = $category[$_POST['demo']];
  }
?>

<html>
<head>
  <title>CSolve Demo</title>
  <link rel="stylesheet" type="text/css" media="screen" href="css/pyg_default.css" />
  <link rel="stylesheet" type="text/css" media="screen" href="css/csolve.css" />
  <link rel="stylesheet" type="text/css" media="screen" href="http://goto.ucsd.edu/csolve/style.css" />
  <script src="http://cdn.jquerytools.org/1.2.6/full/jquery.tools.min.js"></script>
  <script src="http://ajax.microsoft.com/ajax/jquery.templates/beta1/jquery.tmpl.min.js"></script>
  <script src="js/jquery.ba-hashchange.min.js"></script>
  <script src="js/csolve.js"></script>
  <script>
    function toggleVisible (id) {
      var elem = document.getElementById (id);
      if (elem.style.display == "block") {
        elem.style.display = "none";
      } else {
        elem.style.display = "block";
      }
    }

    function toggleValue (id, val1, val2) {
      var elem = document.getElementById (id);
      if (elem.value == val1) {
        elem.value = val2;
      } else {
        elem.value = val1;
      }
    }
  </script>
</head>
<body>
  <div id="title">CSolve Demo</div>
  <div id="subtitle">Liquid Types-Based C Program Verifier</div>

<? if ($entered_program) {
  if ($status == 0) {
?>
    <div id="result">
      <img src="safe.png" class="resultImg" />
      Safe
    </div>
<?
  } else {
?>
    <div id="result">
      <img src="unsafe.png" class="resultImg" />
      Unsafe
    </div>
<?
  }

  echo ($annothtml);
?>

  <input id="logButton" type='button' value='Show Log' onClick='toggleVisible ("log"); toggleValue ("logButton", "Show Log", "Hide Log")' />
  <div id="log" style="font-family:monospace; width: 60em; display: none;">
    <? echo (nl2br ($out)); ?>
  </div>
<? } ?>

<h1>Pick a demo</h1>

<form action='<? echo $_SERVER['PHP_SELF']; ?>' 
      method='post'>
<select name="demo">
<? foreach ($category as $key => $value) {
     if ($value == $demo) { 
       echo '<option selected = "yes" value='.$key.'>'.$key.'</option>';
     } else{
       echo '<option value='.$key.'> '.$key.'</option>';
     }
  } 
?>   
</select>
<input name='chooseform' type='hidden' value='1'>
<input type='submit' value='Choose'>
</form>

<p>Each demo consists of a C program and the predicate templates
required to verify its safety.</p>

<p>You may also try your own programs, in which case you may want to refer
to the <a href="readme.html">CSolve README</a>.</p>

<script>
  function setTextAreaValue (id, editor) {
    document.getElementById (id).value = editor.getSession ().getValue ();
  }
</script>

<form action='<? echo $_SERVER['PHP_SELF']; ?>' 
      method='post'
      onSubmit='setTextAreaValue ("program", editor); setTextAreaValue ("qualifiers", qualEditor)'><p>

<h1>C Program</h1>

<div id="program-editor" style="position: relative; width: 52em; height: 30em"><?
  echo (getFieldOrFile ($entered_program, 'program', $demo.".c"));
?></div>
<textarea id='program' name='program' class="hidden"></textarea>

<h1>Predicate Templates</h1>

<div id="qualifiers-editor" style="position: relative; width: 52em; height: 10em"><?
  echo (getFieldOrFile ($entered_program, 'qualifiers', $demo.".c.hquals"));
?></div>
<textarea id='qualifiers' name='qualifiers' class="hidden"></textarea>

<script src="ace/ace.js" type="text/javascript" charset="utf-8"></script>
<script src="ace/mode-c_cpp.js" type="text/javascript" charset="utf-8"></script>
<script>
  var qualEditor = ace.edit ("qualifiers-editor");
  var editor = ace.edit ("program-editor");
  var CMode = require ("ace/mode/c_cpp").Mode;
  editor.getSession ().setMode (new CMode ());
</script>

<br />

<input name='demo' type='hidden' value='<? echo ($demo); ?>' />
<input name='programform' type='hidden' value='1' />
<input type='submit' value='CSolve' />
</p></form>

</body>
</html>
