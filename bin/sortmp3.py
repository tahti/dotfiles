#!/usr/bin/python
import sys
import os
import re
def sort(source, dest):
  if not dest.endswith('/'):
    dest = dest + "/"
  if not source.endswith('/'):
    source = source+ "/"
  for path, subdirs, files in os.walk(source):
    if not path.endswith('/'):
      path = path + "/"
    for fil in files:
      print "path:" + path
      print "file:" + fil
      if (not os.path.isdir(fil)) and (fil.lower().endswith(".mp3") or fil.lower().endswith(".ogg")):
        start = re.search("[A-z]", fil).start()
        end = re.search("[-]", fil).start()
        artist = fil[start:end].strip()
        artist = re.sub('[.\'\"!]', ' ', artist.lower().replace('&', 'and'))
        artist = re.sub(' +', ' ', artist).strip().replace(' ', '_')
        if artist.startswith('the_'):
          artist = artist[4:]
        if not os.path.isdir(dest + artist):
          os.mkdir(dest + artist)
        f = fil
        if f.lower().endswith(".mp3"):
          f = f[start:-4]+".mp3"
        elif f.lower().endswith(".ogg"):
          f = f[start:-4]+".ogg"
        f = f.strip()
        os.rename(path + fil, dest + artist + "/" + f)

if __name__ == '__main__':
  if len(sys.argv) != 3:
    print 'Usage: %s <source_dir> <dest_dir>' % sys.argv[0]
    sys.exit(2)
  else:
    sort(sys.argv[1], sys.argv[2])
  sys.exit(0)
