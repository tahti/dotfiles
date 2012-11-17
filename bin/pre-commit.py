#!/usr/bin/python
import sys
import re
import os

# These should point to the respective commands
SVNLOOK = '/usr/bin/svnlook'
MAX_FILENAME_LENGTH = 144
ALLOWED_CHARACTERS = "A-Za-z0-9_ \." #note that . would match any character so we have \.

def reg(expression,value):
  return bool(re.search(expression,value))

# Gets a command's output
def commandOutput(command):
  import subprocess
  process = subprocess.Popen(command.split(), stdout = subprocess.PIPE)
  return process.communicate()[0] # 0 is stdout

# Returns an array of the changed files' names
def getChangedFiles(svnPath, transaction):
  # Run svnlook to find the files that were changed
  output = commandOutput('{} changed {} --transaction {}'.format(SVNLOOK, svnPath, transaction))

  # The output from svnlook looks like the following:
  # U   folder/file1.cpp
  # A   folder/file2.cpp
  # where U means updated and A means added
  def changed(fileName):
    return len(line) > 0 and line[0] in ('A', 'U')
  changedFiles = [line[4:] for line in output.split('\n') if changed(line)]

  # svnlook inserts an empty line, so output.split() will have an extra
  # line with nothing in it - ignore the last lines if they're empty
  while len(changedFiles)>0 and 0 == len(changedFiles[-1]):
    changedFiles = changedFiles[:-1]

  return changedFiles

def validateFileName(fileName):
  # We only check the basename of the path.  You can't create /1/2
  # without first creating /1, so this should work.
  basename = os.path.basename (fileName)
  diff = len(basename) - MAX_FILENAME_LENGTH
  if diff > 0:
    print >> sys.stderr, "Filename \'{}\' too long by {} characters.".format(fileName,diff)
    return 1 # Error found
  if not reg("^["+ALLOWED_CHARACTERS+"]+\Z",fileName):
    print >> sys.stderr, "Filename \'{}\' contains illegal characters.".format(fileName)
    return 1 # Error found
  if reg("^[ ].+\Z|^.+[ ]\Z",fileName):
    print >> sys.stderr, "Filename \'{}\' starts or ends with a space.".format(fileName)
    return 1 # Error found
  if reg("(PRN|CON|AUX|CLOCK|NUL|COM\d|LPT\d)",fileName):
    print >> sys.stderr, "Filename \'{}\' is a reserved filename.".format(fileName)
    return 1 # Error found
  return 0 # No errors


svnPath = sys.argv[1]
transaction = sys.argv[2]
files = getChangedFiles(svnPath, transaction)
#files = ["eaea","ee #e", "  e"]

errorCount = 0

for fileN in files:
  errorCount = errorCount + validateFileName(fileN)

sys.exit(errorCount)
