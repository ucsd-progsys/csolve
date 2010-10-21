#!/usr/bin/env python

import sys, tempfile, subprocess, os


def logged_sys_call(args, out=None, err=None):
  print "exec: " + " ".join(args)
  return subprocess.call(args, stdout=out, stderr=err)

def run(quiet, flags):
  tfile = tempfile.NamedTemporaryFile()
  
  cpp     = "cpp"
  cpp_out = ("-o %s" % tfile.name)
  cpp_i   = "-I../coreutils/src -I../coreutils/lib -I../usr/include"
  if len(flags) > 1:
    cpp_i  += " ".join(flags[1:])
  cpp_in  = flags[0]
  cpp_cmd = ("%s %s %s %s" % (cpp, cpp_i, cpp_out, cpp_in)) 
  
  fpr     = "funptr/main.native"
  fpr_in  = tfile.name
  fpr_cmd = ("%s %s" % (fpr, fpr_in))
  
  logged_sys_call(cpp_cmd.split(" "), quiet)
  rv = logged_sys_call(fpr_cmd.split(" "), quiet)

  tfile.close()

  if rv > 0:
    print ("Found %s: %d" % (flags[0], rv-1))

  return rv

if len(sys.argv) < 2:
  print ("Usage: %s fname [-I...]" % sys.argv[0])
  sys.exit(1)
print ("Checking file: %s" % sys.argv[1])
sys.exit(run(False, sys.argv[1:]))

