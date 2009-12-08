#In your .vimrc, add the following:
#map <F4> :python printLiquidType("normal")<CR>
#vmap <F4> :python printLiquidType("visual")<CR>
#pyfile path/to/csolve.py
#to "reparse", do
#:python annot.parseLiquidType()


#python << ENDPYTHON

import vim
import time
import re
import os

class AnnExc(Exception):
  def __init__(self, reason):
    self.reason = reason

class Cannotations:
  def __init__(self):
    self.__vmap_filename = None # last .vmap parsed file
    self.__filename = None 	# last .c parsed file 
    self.__timestamp = None     # last parse action timestamp
    self.__annot = {}
 
  def parse(self, filename):
    self.__filename      = filename 
    self.__vmap_filename = filename + ".vmap" 
    self.__timestamp     = int(time.time())
    try:
      f = open(self.__vmap_filename)
      for l in f:
	es  = [x.strip() for x in l.split("\t")]
	key = (str(es[0]), int(es[2]))
	val = es[3]
	self.__annot[key] = val
      f.close()
    except:
      raise 
      #raise AnnExc("Csolve: Error during __parse") 
 
  def print_keys(self):
    for k in self.__annot:
      print "key = ", k, ", data = ", self.__annot[k]
    return
 
  def get_ssaname(self, varname, line):
    if vim.current.buffer.name == None:
      raise AnnExc("Empty buffer") 
    if vim.current.buffer.name != self.__filename or  \
      os.stat(self.__filename).st_mtime > self.__timestamp:
	filename = self.__filename #vim.current.buffer.name
	self.parse(filename)
	vim.command("set tags=" + filename + ".tags")
    ssaname = varname
    try:
      ssaname = self.__annot[(str(varname), int(line))]
    except:
      pass
    return ssaname
  
  def get_type(self, varname, line):
    ssaname = self.get_ssaname(varname, line)
    return vim.command("tag " + ssaname) 

annot    = Cannotations()	#global annotation object
alphanum = re.compile("^[\w.]$")

def getVarnameVisual(buf):
  (line, col1) = buf.mark("<")
  (_   , col2) = buf.mark(">")
  return buf[line-1][col1 : (col2 + 1)]

def getVarnameNormal(buf, (line, col)):
  left, right = col, col
  while left >= 0 and alphanum.search(buf[line-1][left]):
    left  -= 1
  while right < len(buf[line-1]) and alphanum.search(buf[line-1][right]):
    right += 1
  return buf[line-1][left + 1: right]

def printLiquidType(mode):
  if mode == "visual":
    varname = getVarnameVisual(vim.current.buffer)
  else:
    varname = getVarnameNormal(vim.current.buffer, vim.current.window.cursor)
  try:
    annot.get_type(varname, vim.current.window.cursor[0])
  except:
    print "No annotation for ", varname
    raise

def parseLiquidType():
  annot.parse(vim.current.buffer.name)

def reparseLiquidType(x):
  annot.parse(x)
#ENDPYTHON
