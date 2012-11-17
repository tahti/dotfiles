#! /usr/bin/env python
import sys
import time
import subprocess
from Xlib import display, X

class miniEWMH:
  def __init__(self, _display=None, root = None):
    self.display = _display or display.Display()
    self.root = root or self.display.screen().root

  def getWmName(self, win):
    """Get the property _NET_WM_NAME for the given window as a string.
    
    :param win: the window objet
    :return: str"""
    return self._getProperty('_NET_WM_NAME', win)
  
  def getWmWindowCommand(self, win):
    """Get the list of window types of the given window (property _NET_WM_WINDOW_TYPE).
    
    :param win: the window objet
    :param str: True to get a list of string types instead of int
    :return: list of (int|str)"""
    l = self._getProperty('WM_CLASS', win).split('\x00')
    l.pop() 
    return l[0]

  def getWmWindowClass(self, win):
    """Get the list of window types of the given window (property _NET_WM_WINDOW_TYPE).
    
    :param win: the window objet
    :param str: True to get a list of string types instead of int
    :return: list of (int|str)"""
    l = self._getProperty('WM_CLASS', win).split('\x00')
    l.pop() 
    return l[1]

  def getActiveWindow(self):
    """Get the current active (toplevel) window or None (property _NET_ACTIVE_WINDOW)
    
    :return: Window object or None"""
    return self._createWindow(self._getProperty('_NET_ACTIVE_WINDOW')[0])

  def _getProperty(self, _type, win=None):
    if not win: win = self.root
    atom = win.get_full_property(self.display.get_atom(_type), X.AnyPropertyType)
    if atom: return atom.value

  def _createWindow(self, wId):
    if not wId: return None
    return self.display.create_resource_object('window', wId)

def sendKeys(keys):
  subprocess.call("/home/piotr/bin/crikey -S 80 -t \'"+keys+"\'",shell=True)

if __name__ == "__main__":
    time.sleep(1)
    ewmh = miniEWMH()
    win=ewmh.getActiveWindow()
    name = ewmh.getWmWindowClass(win)    
    print name
