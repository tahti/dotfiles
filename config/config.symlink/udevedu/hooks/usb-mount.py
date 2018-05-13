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
    major = os.major(device.device_number)
    return device.get('DEVTYPE') == 'partition' and device.get('SUBSYSTEM') == 'block' and (major == 3 or major == 8)


def react(action, device):
    #print('USB device %s,++++++++++++++++++++++++++++++' % action, end="")

    #for key, value in device.items():
    #  print('{key}={value}'.format(key=key, value=value))
    devpath = device.get('DEVNAME') 
    if action == 'add':
          if devpath in str(invoke('pmount')):
              invoke('pumount', devpath)
          invoke('pmount', devpath)
          mounted.add(devpath)
    elif action == 'remove':
      if devpath in str(invoke('pmount')) or devpath in mounted:
          logging.info('unmounting')
          mounted.remove(devpath)
          invoke('pumount', devpath)
