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
  #subprocess.call("/home/piotr/bin/crikey -S 80 -x \'"+keys+"\'",shell=True)
  time.sleep(0.15)
  subprocess.call("xte "+keys+"",shell=True)
def sendKeysX(keys):
  subprocess.call("/home/piotr/bin/crikey -S 80 -x \'"+keys+"\'",shell=True)

if __name__ == "__main__":
  if (len(sys.argv) < 2):
    print "Usage: python doCommand commandName"
  else:
    ewmh = miniEWMH()
    win=ewmh.getActiveWindow()
    name = ewmh.getWmWindowClass(win)    
    if( sys.argv[1] == 'quit' ):
      print "quit"
      if name == "Iceweasel" or name=="Firefox":
        #print "Fire Ice"
        sendKeys("\"str :wqall\" \"key Return\"")	
      #elif name=="Konqueror":
	##print "Konqueror"
        ##time.sleep(0.01)
        #sendKeys("\"keydown Control_L\" \"key q\" \"keyup Control_L\"")
      #elif name == "Okular" or name == "Main.py":
	##print "Okular"
        #sendKeys("\\Cq")	
      elif name == "XTerm" or name == "Gnome-terminal":
	#print "Xterm"
        sendKeys("\"keydown Control_L\" \"keydown Shift_L\" \"key q\" \"keyup Control_L\" \"keyup Shift_L\"")
      elif name == "URxvt":
	#print "URxvt"
        sendKeys("\"keydown Control_L\" \"key d\" \"keyup Control_L\"")
      elif name == "Pidgin":
	#print "Pidgin"
        #sendKeys("\"keydown Control_L\" \"key q\" \"keyup Control_L\"")
        sendKeys("\"keydown Alt_L\" \"key b\" \"keyup Alt_L\" \"key q\"")
        #sendKeys("\Axq")	
      elif name == "feh" or name == "Xpdf":
	#print "feh"
        sendKeys("\"str q\"")	
      else:
        sendKeys("\"keydown Control_L\" \"key q\" \"keyup Control_L\"")
        print "other quit"
        #sendKeysX("\\Cq")	
    elif( sys.argv[1] == 'winLeft' ):
        pass
        #sendKeys("\\An")	
