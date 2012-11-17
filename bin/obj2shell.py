#!/usr/bin/python
from subprocess import Popen, PIPE
import sys
 
def shellcode_from_objdump(obj):
    res = ''
    p = Popen(['objdump', '-d', obj], stdout=PIPE, stderr=PIPE)
    (stdoutdata, stderrdata) = p.communicate()
    if p.returncode == 0:
        for line in stdoutdata.splitlines():
            cols = line.split('\t')
            if len(cols) > 2:
                for b in [b for b in cols[1].split(' ') if b != '']:
                    res = res + ('\\x%s' % b)
    else:
        raise ValueError(stderrdata)
 
    return res
 
 
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print 'Usage: %s <obj_file>' % sys.argv[0]
        sys.exit(2)
    else:
        print 'Shellcode for %s:' % sys.argv[1]
        print shellcode_from_objdump(sys.argv[1])
    sys.exit(0)
