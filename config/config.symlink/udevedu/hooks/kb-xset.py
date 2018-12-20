"""
Call xset on keyboard changes.
"""
import time

# ---- conf begin
xset_args = ['r', 'rate', '250', '80']
# ---- conf end

from udevedu.utils import invoke


def check(action, device):
    return device.get('ID_INPUT_KEYBOARD') and device.get('LED')


def react(action, device):
    print('new keyboard attached')
    time.sleep(1)
    invoke('xset', *xset_args)
