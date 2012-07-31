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
from optparse import OptionParser

baseDir          = sys.path[0]
#lineTplt         = Template("<span class='line' num=$linenum><a class='linenum' href= \"#$linenum\" num=$linenum name=\"$linenum\">$padlinenum:&nbsp;</a>$line</span>")
lineSpan         = "<span class='line' file=$filenum num=$linenum>" + \
                   "<a class='linenum' href= \"$lineanch\" file=$filenum num=$linenum name=\"$lineanch\">$padlinenum:&nbsp;</a>" + \
                   "$line</span>"
lineTplt         = Template(lineSpan)
anchTplt         = Template("#$filenum-$linenum")

#headTplt         = "/***********************************************************************/" + \
#                   "/***** Source $filenum: $filename *********/"


def headSrc(fileNum, fileName):
  str   = "/*** Source %d: %s " % (fileNum, fileName)
  if len(str) > 79: 
    str += "*/"
  else:
    str += "*" * (79 - len(str)) + "/"
  stars = "/" + "*" * (len(str) - 2) + "/"
  return "\n".join([stars, str, stars])



########################### Generic IO Helpers ###########################

def getOptions():
  p = OptionParser()
  p.add_option("-j", "--json"
              , dest="json", help="name of json file"
              , default="undefined", metavar="FILE")
  p.add_option("-o", "--out"
              , dest="out", help="name of output html"
              , default="out.html", metavar="FILE")
  p.add_option("-w", "--web-demo"
              , dest="isWebDemo"
              , action="store_true"
              , default=False)
  (opts, args)  = p.parse_args()
  opts.srcFiles = args
  return opts

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

def lineToHtml(fi, li, pad, line):
  anch = anchTplt.substitute(filenum = fi, linenum = li)
  return lineTplt.substitute(filenum = fi, linenum = li, padlinenum = pad, lineanch = anch, line = line)

def addLineNumbers(srcIndex, src, html):
  s    = html[28:-13] 
  ins  = ["" for i in range(numBlanks(src))] + s.split("\n")
  n    = len(ins)
  size = len(str(n))
  args = [(i, padLineNum(size, i), l) for (i, l) in zip(range(1, n+1), ins)]
  outs = [lineToHtml(srcIndex, i, si, l) for (i, si, l) in args]
  return (html[:28] + "\n".join(outs) + html[-13:])

##################### Plugging Into Templates #########################

def copyDir(destDir, off):
  inDir  = baseDir + "/" + off
  outDir = destDir + "/" + off
  distutils.dir_util.copy_tree(inDir, outDir)
  print "cs2Html: copyDir from ", inDir, " To ", outDir

def rawSrcToHtml(src):
  return highlight(src, CLexer(stripall=False), HtmlFormatter())

def srcToHtml(isWebDemo, srcIndex, srcFile):
  src      = readFrom(srcFile)
  html     = rawSrcToHtml(src)
  srcHtml  = addLineNumbers(srcIndex, src, html)
  if (isWebDemo):
    return (srcHtml)
  else:
    head     = headSrc(srcIndex, srcFile)
    headHtml = rawSrcToHtml(head)
    return (headHtml + srcHtml)

def srcsToHtml(isWebDemo, srcFiles):
  n     = len(srcFiles)
  htmls = [srcToHtml(isWebDemo, i, f) for (i, f) in zip(range(len(srcFiles)), srcFiles)]
  return "\n\n\n".join(htmls)

def main():
  opts    = getOptions()
  print "cs2html", "Json:", opts.json, "Sources:", opts.srcFiles, "Html:", opts.out 
  tgtFile = os.path.abspath(opts.out)
  srcDir  = os.path.dirname(tgtFile)
  srcHtml = srcsToHtml(opts.isWebDemo, opts.srcFiles) 
  srcJson = readFrom(opts.json)
  if (opts.isWebDemo):
    tpltFile = baseDir + "/templates/web-csolve.html"
  else:
    tpltFile = baseDir + "/templates/csolve.html"
  tplt = Template(readFrom(tpltFile))
  tgt     = tplt.substitute(srcHtml = srcHtml, srcJson = srcJson)
  writeTo(tgtFile, tgt)
  copyDir(srcDir, "css")
  copyDir(srcDir, "js")

#############################################################################

try: 
  main()
except: 
  print "Unexpected error:", sys.exc_info()[0]
  print "Error in c2html. Usage: ./cs2html.py -j inFile.json -o outFile.html file1.c file2.c fileN.c" 
  raise
