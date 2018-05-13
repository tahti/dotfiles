"""
Mount and unmount MTP devices automatically.
"""
# ---- conf begin
mp_root = '~/usb'
# ---- conf end

from pathlib import Path
import os
import os.path
import errno
import logging

from udevedu.utils import mkdir_p, invoke

mounted = dict()
mp_root = os.path.expanduser(mp_root)


def check(action, device):
  if action == 'add':
    return device.get('ID_MTP_DEVICE')
  if action == 'remove':
    try:
      bus = device['BUSNUM']
      dev = device['DEVNUM']
      if ((bus,dev) in mounted) or device.get('ID_MTP_DEVICE'):
        return True
    except:
        return False
  return False

def umount(path, bus, dev):
    if (bus, dev) in mounted:
        del mounted[(bus, dev)]
    invoke('fusermount', '-u', path)
    invoke('rmdir', path)

def react(action, device):
    logging.info('MTP device %s,' % action)

    try:
        bus = device['BUSNUM']
        dev = device['DEVNUM']
        if (action == 'add'):
          model = device['ID_MODEL']
    except KeyError as e:
        loggging.info('but %s key missing' % e.args[0])
        return

    if action == 'add':
        mp = os.path.join(mp_root, model)
        try:
            if Path(mp).exists():
              umount(mp, bus, dev) 
            mkdir_p(mp)
        except OSError as e:
            if e.errno == errno.EEXIST:
                logging.info('but failed to create mountpoint %s' % mp)
                logging.info('Error %s' % str(e))
                return
        else:
            logging.info('created mountpoint %s, mouting' % mp)

        e = invoke('jmtpfs', '-device=%s,%s' % (bus, dev), mp)
        if isinstance(e, bytes) or (e is None):
            mounted[(bus, dev)] = mp
        else:
            umount(mp, bus, dev) 

    elif action == 'remove':
        if (bus, dev) in mounted:
            logging.info('unmounting')
            umount(mounted[bus,dev], bus, dev) 
