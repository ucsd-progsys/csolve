#!/usr/bin/python
# Copyright (c) 2009-12 The Regents of the University of California. All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
# IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
# OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
# TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

import os, sys, distutils.dir_util #shutil
from pygments import highlight
from pygments.lexers import CLexer
from pygments.formatters import HtmlFormatter
from string import Template

srcFile  = os.path.abspath(sys.argv[1])
tgtFile  = srcFile + ".html"
srcDir   = os.path.dirname(srcFile)
baseDir  = sys.path[0]
jsonFile = srcFile + ".json"
tgtTplt  = baseDir + "/templates/csolve.html" 
lineTplt = Template("<span class='line' num=$linenum><a class='linenum' href= \"#$linenum\" num=$linenum name=\"$linenum\">$padlinenum:&nbsp;</a>$line</span>")


##################### Generic IO Helpers #########################

def readFrom(file):
  f  = open(file, "r")
  rv = f.read()
  f.close()
  return rv

def writeTo(file, s):
  f  = open(file, "w")
  f.write(s)
  f.close()

################ Add Line Numbers To Pygment Output ######################

def isBlank(s):
  return (len(s.split()) == 0)

def numBlanks(s):
  k = 0
  for l in s.split("\n"):
    if isBlank(l):
      k += 1
    else:
      break
  return k

def padLineNum(size, i):
  isize = len(str(i))
  pad   = "&nbsp;" * (size - isize)
  return (pad + str(i))

def addLineNumbers(src, html):
  s    = html[28:-13] 
  ins  = ["" for i in range(numBlanks(src))] + s.split("\n")
  n    = len(ins)
  size = len(str(n))
  args = [(i, padLineNum(size, i), l) for (i, l) in zip(range(1, n+1), ins)]
  outs = [lineTplt.substitute(linenum = i, padlinenum = si, line = l) for (i, si, l) in args]
  return (html[:28] + "\n".join(outs) + html[-13:])

##################### Plugging Into Templates #########################

def copyDir(off):
  inDir  = baseDir + "/" + off
  outDir = srcDir  + "/" + off
  distutils.dir_util.copy_tree(inDir, outDir)
  print "c2Html: copyDir from ", inDir, " To ", outDir
  ##if os.path.exists(dstDir):
  ##  pass
  ##else:
  ##  shutil.copytree(srcDir, dstDir)

def main(srcFile, jsonFile):
  src     = readFrom(srcFile)
  srcHtml = highlight(src, CLexer(stripall=False), HtmlFormatter())
  srcHtml = addLineNumbers(src, srcHtml)
  srcJson = readFrom(jsonFile)
  tplt    = Template(readFrom(tgtTplt))
  tgt     = tplt.substitute(srcHtml = srcHtml, srcJson = srcJson)
  writeTo(tgtFile, tgt)

#############################################################################

try: 
  print "c2html src=", srcFile, " srcDir=", srcDir
  main(srcFile, jsonFile)
  copyDir("css")
  copyDir("js")
except: 
  print "Unexpected error:", sys.exc_info()[0]
  print "Error in c2html. Usage: ./c2html.py inFile.c" 
  raise


