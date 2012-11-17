#!/usr/bin/python

import sys,os
import os.path

def main():
  cmd="clang++"
  for a in sys.argv[1:]:
    if a[-3:]=='lzz':
      b = a[:-3]+"cpp"
      if os.path.isfile(b):
        a = b
      else:
        ind = b.rfind("/")
        ind+=1;
        if os.path.isdir(b[:ind]+"generated"):
          a = b[:ind]+"generated/"+b[ind:]
    cmd+=" "+a
  os.system(cmd)

if __name__ == "__main__":
      main()
