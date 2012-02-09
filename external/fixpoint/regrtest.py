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

import sys, time, os, os.path, subprocess, string
import itertools as it

# App Specific Constants
solve      = "_build/main.native".split()
testdirs   = [("./tests/dsolve", 0),("./tests/csolve-postests", 0),("./tests/csolve-negtests", 1)]
null       = open("/dev/null", "w")
logfile    = "./regrtest.results"
argcomment = "//! run with "

########################################################################

def logged_sys_call(args, out=None, err=None):
  print "exec: " + " ".join(args)
  return subprocess.call(args, stdout=out, stderr=err)

def logstring(outs):
  print outs
  f = open(logfile, "a")
  f.write(outs)
  f.close

def getfileargs(file):
  f = open(file)
  l = f.readline()
  f.close()
  if l.startswith(argcomment):
    return l[len(argcomment):].strip().split(" ")
  else:
    return []

def logtest(file, ok, runtime):
  if ok:
    oks = "\033[1;32mSUCCESS!\033[1;0m\n"
  else:
    oks = "\033[1;31mFAILURE :(\033[1;0m\n"
  outs = "test: %s \ntime: %f seconds \nresult: %s \n \n" % (file, runtime, oks) 
  logstring(outs)

def run_app(file,bare,time,quiet,flags):
  if quiet: out = null
  else: out = None
  if time: time = ["time"]
  else: time = []
  return logged_sys_call(time + solve + flags + [file], out)

def runtest(file, expected_status, dargs):
  fargs   = getfileargs(file)
  start   = time.time()
  status  = run_app(file, True, False, True, fargs + dargs)
  runtime = time.time() - start
  ok      = (status == expected_status)
  logtest(file, ok, runtime)
  return (file, ok)

#API
def runtests(dir, expected_status, dargs):
  print "Running tests from %s/" % dir
  files = it.chain(*[[os.path.join(dir, file) for file in files] for dir, dirs, files in os.walk(dir)])
  return [runtest(file, expected_status, dargs) for file in files if file.endswith(".fq") ]

#API
def logresults(results):
  failed    = [result[0] for result in it.chain(*results) if result[1] == False]
  failcount = len(failed)
  if failcount == 0:
    outs = "\n\033[1;32mPassed all tests! :D\033[1;0m"
  else:
    outs = "\n\033[1;31mFailed %d tests:\033[1;0m %s" % (failcount, "\n".join(failed))
  logstring(outs)
  sys.exit(failcount != 0)

#####################################################################################

os.environ['LD_LIBRARY_PATH'] = "../z3/lib"

results   = [runtests(dir, expected_status, sys.argv[1:]) for (dir, expected_status) in testdirs]
logresults(results)

