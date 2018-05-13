from __future__ import print_function
"""
Mount  USB drives automatically.
"""
# ---- conf begin
usb_root = '~/usb'
# ---- conf end

import os
import os.path
import errno
import logging

from udevedu.utils import mkdir_p, invoke
mounted = set()

def check(action, device):
    return False 

def react(action, device):
    print('---------------------------')
    for key, value in device.items():
      print('{key}={value}'.format(key=key, value=value))
