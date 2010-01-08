import os, subprocess

def cat_files(files, outfile):
  os.system("rm -f %s" % outfile)
  for f in files: os.system("cat %s 1>> %s 2> /dev/null" % (f,outfile))

def read_lines(name):
  f = open(name)
  lines = f.readlines()
  f.close()
  return lines

def write_line(name,line):
  f = open(name,"w")
  f.write(line)
  f.close()

def logged_sys_call(args, out=None, err=None):
  print "exec: " + " ".join(args)
  return subprocess.call(args, stdout=out, stderr=err)

def str_to_bool(str):
  if str == "False":
    return False
  return True
