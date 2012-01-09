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

import sys, string, shutil, json, re, common

template_html = "template.html" 
tmp           = "c2html.tmp"
prefix	      = ".1"
suffix	      = ".2"

#########################################################################
######### Format HTML file with SPANs indicating Line Number ############
#########################################################################

def format_line(i,et,s):
  span_class = ""
  if et.has_key(i):
    span_class = "class=\"errorline\""

  return ("<span id=\"line:%d\" %s> %s </span>\n" % (i, span_class, s[:-1]))

def make_error_link(text):
  return "<a href=\"javascript:showErrors()\">%s</a>" % (text)

def format_code(base, et):
  src = base + ".html"
  f   = open(src, "r")
  t   = open(base + suffix, "w")
  i   = 0

  t.write("<div id=\"code\" onmouseup = \"showAnnotOfElement(event.srcElement)\">\n\n")

  num_errors = len(et.keys())
  if num_errors == 0:
    t.write("Safe!")
  elif num_errors == 1:
    t.write(make_error_link("There was 1 type error."))
  else:
    t.write(make_error_link("There were %d type errors." % (num_errors)))

  for l in f:
    i +=1
    if (i == 5):
      t.write(l[0:9] + "\n")
      t.write(format_line(1, et, l[9:]))
    if (i > 5):
      t.write(format_line(i-4, et, l))
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

error_re = re.compile (r"^\w+\.c:(\d+):$")

def insert_append(d, i, v):
  try:
    d[i].append(v)
  except KeyError:
    d[i] = [v]

def make_etab(src):
  et    = {}
  f     = open(src, "r")
  st    = 0
  line  = -1
  error = []
  for l in f:
    if l.isspace ():
      if st == 1:
        st = 2
      elif st == 2:
        insert_append(et, line, "".join (error))
        line  = -1
        error = []
        st    = 0

    if st == 2:
      error.append(l)

    m = re.match(error_re, l)
    if m != None:
      line = int(m.group(1))
      st = 1

  insert_append(et, line, "".join (error))
  f.close()
  et.pop (-1)
  return et

def gen_annot_tables(base):
  vt = make_vtab(base + ".vmap")
  tt = make_ttab(base + ".annot")
  et = make_etab(base + ".csolve.out")
  return (vt, tt, et)

#########################################################################
######################### Generate JavaScript File ######################
#########################################################################

def gen_jscript(base, vt, tt, et):
  js = "<script>\n  vt = eval(%s) \n  tt = eval(%s) \n  et = eval(%s) \n</script>\n\n" 
  js = js % (json.dumps(vt), json.dumps(tt), json.dumps(et))
  f  = open(base + prefix, "w")
  f.write
  f.write(js)
  f.close()

#########################################################################
######################### Generate JavaScript File ######################
#########################################################################

# input:  base, base.vmap, base.annot, base.csolve.out, base.html
# output: base.html
def main(base):
  (vt, tt, et) = gen_annot_tables(base)
  gen_jscript(base, vt, tt, et)
  format_code(base, et)
  common.cat_files([template_html, base + prefix, base + suffix], base + ".out.html")

main(sys.argv[1])
