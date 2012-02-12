#!/usr/local/bin/python
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

import sys
from pygments import highlight
from pygments.lexers import CLexer
from pygments.formatters import HtmlFormatter
from string import Template

srcTplt = "templates/source.html"
tgtTplt = "templates/csolve.html" 
tgtFile = "csolve.html"

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

def addLineNumber(i, line):
  return ("<span class=\"line\" num=\"%d\">%s</span>" % (i, line))

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

def addLineNumbers(src, html):
  s    = html[28:-13] 
  ins  = ["" for i in range(numBlanks(src))] + s.split("\n")
  n    = len(ins)
  outs = [addLineNumber(i, l) for (i, l) in zip(range(1, n+1), ins)]
  return (html[:28] + "\n".join(outs) + html[-13:])

##################### Plugging Into Templates #########################

def addHeaderFooter(html):
  css = "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"pyg_default.css\" />\n"
  return (css + html)

def main(srcFile):
  src     = readFrom(srcFile)
  srcHtml = highlight(src, CLexer(stripall=False), HtmlFormatter())
  srcHtml = addLineNumbers(src, srcHtml)
 
  #Write Source File (HTML)
  htmlFile = srcFile + ".html"
  writeTo(htmlFile, Template(readFrom(srcTplt)).substitute(srcHtml = srcHtml)) 

  #Write Target File (HTML)
  writeTo(tgtFile, Template(readFrom(tgtTplt)).substitute(srcHtmlFile = htmlFile)) 


#try:
main(sys.argv[1])
#except:
#  print "Usage: ./c2html.py <infile.c>"
