#!/usr/bin/python
# Copyright (c) 2009 The Regents of the University of California. All rights reserved.
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

import sys, string, shutil, json, common

template_html = "template.html" 
tmp           = "c2html.tmp"
prefix	      = ".1"
suffix	      = ".2"

#########################################################################
########### Pad HTML file with SPAN indicating Line Number ##############
#########################################################################

def pad(i,s):
  return ("<span id=\"line:%d\"> %s </span>\n" % (i, s[:-1]))

def divpad(base):
  src = base + ".html"
  f   = open(src, "r")
  t   = open(base + suffix, "w")
  i   = 0
  t.write("<div id=\"code\" onmouseup = \"showTypeOfElement(event.srcElement)\">\n\n")
  for l in f:
    i +=1
    if (i == 5):
      t.write(l[0:9] + "\n")
      t.write(pad(1, l[9:]))
    if (i > 5):
      t.write(pad(i-4, l))
  t.write("</div>")
  f.close()
  t.close()

#########################################################################
########### Generate Tables from .annot and .vmap file ##################
#########################################################################

def make_vtab(src):
  vt = {} 
  f  = open(src, "r")
  for l in f:
    vs    = [t for t in l.split(" ") if t and not t.isspace()]
    k     = vs[0] + " at " + vs[2]
    d     = vs[3]
    vt[k] = d
  f.close()
  return vt

def make_ttab(src):
  tt  = {}
  f   = open(src, "r")
  st  = 0
  key = ""
  val = ""
  for l in f:
    x = l.split(" ")
    if l.isspace():
      pass
    elif l[0:3] == "=+=":
      tt[key] = val
      st = 0
    elif st == 2:
      pass
    elif st == 1:
      val += l
    elif x[0] == "funstore":
      st = 2
      key = ""
      val = ""
    else:
      st  = 1
      key = x[1]
      val = ""
  tt[key] = val
  f.close()
  return tt

#########################################################################
######################### Generate JavaScript File ######################
#########################################################################

def gen_jscript(base):
  vt = make_vtab(base + ".vmap")
  tt = make_ttab(base + ".annot")
  js = "<script>\n  vt = eval(%s) \n  tt = eval(%s) \n</script>\n\n" 
  js = js % (json.dumps(vt), json.dumps(tt))
  f  = open(base + prefix, "w")
  f.write
  f.write(js)
  f.close()

#########################################################################
######################### Generate JavaScript File ######################
#########################################################################

# input:  base, base.vmap, base.annot, base.html
# output: base.html
def main(base):
  gen_jscript(base)
  divpad(base)
  common.cat_files([template_html, base + prefix, base + suffix], base + ".out.html")

main(sys.argv[1])
