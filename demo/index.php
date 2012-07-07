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
  $annothtml = $annothtml."Click on variable to see inferred types." ;
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

  if($_POST['chooseform'] == "1") {
    $demo = $category[$_POST['choosedemo']]; 
  }

  if($_POST['programform'] == "1") {
    $tc    = tempnam ("/tmp/csolve-demo", "csolve-demo") . ".c";
    $tann  = $tc    . ".annot";
    $thq   = $tc    . ".hquals";
    $thtml = $tc    . ".html"; 
    $log   = $tc    . ".log";
    writeTextFile($tc,  'program');
    writeTextFile($thq, 'qualifiers');
    $out = shell_exec("../src/csolve ".$tc." > ".$log." 2>&1");
    $annothtml = getAnnots($thtml);
    // $loghtml   = "<a href=\"".$logfile."\"> <h3>Log</h3> </a>";
    $entered_program = 1;
    //shell_exec("rm -f ".$tc."*");
  }
?>

<html>
<head>
  <title>CSolve Demo</title>

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

<form action='<? echo $_SERVER['PHP_SELF']; ?>' 
      method='post'><p>

<h3>Logical Qualifiers</h3>
<textarea name='qualifiers' rows='5' cols='80'>
<?
  if($entered_program == 1) {
    echo (getRawTextFromField('qualifiers'));
  }
  else {
    echo (file_get_contents($demo.".c.hquals"));
  }
?>
</textarea>

<h3>C Program</h3>

<textarea name='program' rows='20' cols='80'>
<?
  if($entered_program == 1) {
    echo (getRawTextFromField('program'));
  }
  else {
    echo (file_get_contents($demo.".c"));
  }
?>
</textarea>

<br />
<input name='programform' type='hidden' value='1'>
<input type='submit' value='csolve'>
</p></form>

<hr />
<? echo $annothtml ?>
</body>
</html>
