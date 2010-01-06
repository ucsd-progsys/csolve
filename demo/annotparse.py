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

import sys, string, shutil

def make_vtab(src):
  vt = {} 
  f  = open(src, "r")
  for l in f:
    vs    = [t for t in l.split(" ") if t and not t.isspace()]
    k     = (vs[0], vs[2])
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
  f.close()
  return tt


