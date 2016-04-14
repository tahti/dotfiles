"""cpmerge

Usage:
  cpmerge [--nogui] [--distance=<dt>] [-v | -vv]

Options:
  --nogui         Start without GUI.
  --distance=<dt> Distance the mouse must have moved before copying the primary selection [default: 20].
  -h --help       Show this screen.
  -v              Verbose (print whenever the clipboard changes).
  -vv             Very verbose (also print the mouse distance after a primary selection).
  --version       Show version.

"""
from __future__ import print_function
import wx
import math
import functools
import os.path
from docopt import docopt

__version__ = '1.0.5'

TRAY_TOOLTIP = 'cpmerge - Clipboard Manager'
TRAY_ICON = os.path.join(os.path.dirname(__file__), 'icon.png')
TIMER_ID = 100
MAX_HISTORY = 10

# Yes, I know global variables are evil but since this is a < 200 loc program I think they are justafiable
current_content = None
history = []
primary_cache = None
primary_mouse_location = None
arguments = None


def get_label(content):
    # Use the first visible characters as label
    # If there is more than one line or if the line is too long, truncate it
    lines = content.strip().splitlines()

    if len(lines[0]) > 23:
        return lines[0][:20] + u'...'
    elif len(lines) > 1:
        return lines[0] + u'...'
    else:
        return lines[0]


def create_menu_item(menu, label, func):
    item = wx.MenuItem(menu, -1, label)
    menu.Bind(wx.EVT_MENU, func, id=item.GetId())
    menu.AppendItem(item)
    return item


class TaskBarIcon(wx.TaskBarIcon):
    def __init__(self, frame):
        self.frame = frame
        super(TaskBarIcon, self).__init__()
        self.set_icon(TRAY_ICON)
        self.Bind(wx.EVT_TASKBAR_LEFT_DOWN, self.on_left_down)

    def CreatePopupMenu(self):
        menu = wx.Menu()
        for content in history:
            func = functools.partial(self.on_history_chosen, content=content)  # Trap the content into a function
            create_menu_item(menu, get_label(content), func)

        if history:
            menu.AppendSeparator()

        create_menu_item(menu, 'Exit', self.on_exit)
        return menu

    def set_icon(self, path):
        icon = wx.IconFromBitmap(wx.Bitmap(path))
        self.SetIcon(icon, TRAY_TOOLTIP)

    def on_left_down(self, event):
        self.PopupMenu(self.CreatePopupMenu())

    def on_history_chosen(self, event, content):
        if arguments['-v']:
            print(u"Populate clipboard and primary from history: '{}'".format(get_label(content)))
        set_clipboard(content, use_primary=False)
        set_clipboard(content, use_primary=True)

    def on_exit(self, event):
        wx.CallAfter(self.Destroy)
        self.frame.Close()


def get_clipboard(use_primary=False):
    if not wx.TheClipboard.IsOpened():
        do = wx.TextDataObject()
        wx.TheClipboard.UsePrimarySelection(use_primary)
        wx.TheClipboard.Open()
        success = wx.TheClipboard.GetData(do)
        wx.TheClipboard.Close()
        return do.GetText()
    return None


def set_clipboard(content, use_primary=False):
    global current_content
    global primary_cache

    if not wx.TheClipboard.IsOpened():
        do = wx.TextDataObject()
        do.SetText(content)
        wx.TheClipboard.UsePrimarySelection(use_primary)
        wx.TheClipboard.Open()
        wx.TheClipboard.SetData(do)
        wx.TheClipboard.Close()

        current_content = content
        if use_primary:
            primary_cache = None


def add_history(content):
    global history

    if content:
        if content in history:
            # if it's already in there, remove it and add it so that it is at the top again
            history.remove(content)

        history.insert(0, content)
        history = history[0:MAX_HISTORY]  # Make sure we only keep 10


def distance(p1, p2):
    if p1 is None or p2 is None:
        return 0
    else:
        return math.sqrt(math.pow(p1[0] - p2[0], 2) + math.pow(p1[1] - p2[1], 2))


def on_timer(event):
    global current_content
    global primary_cache
    global primary_mouse_location

    def has_changed(content):
        return content is not None and content != current_content and len(content) > 0

    # copy clipboard -> primary
    content = get_clipboard(use_primary=False)
    if has_changed(content):
        set_clipboard(content, use_primary=True)
        add_history(content)
        if arguments['-v']:
            print(u"Copy from clipboard to primary: '{}'".format(get_label(content)))

    # copy primary -> cache
    content = get_clipboard(use_primary=True)
    if has_changed(content) and content != primary_cache:
        primary_cache = content
        primary_mouse_location = wx.GetMousePosition()

    # copy cache -> clipboard
    dist = int(arguments['--distance']) if arguments['--distance'] else 20
    if has_changed(primary_cache) and distance(wx.GetMousePosition(), primary_mouse_location) > dist:
        set_clipboard(primary_cache, use_primary=False)
        add_history(primary_cache)
        if arguments['-v']:
            print(u"Copy from primary to clipboard: '{}'".format(get_label(content)))

    if arguments['-v'] > 1 and has_changed(primary_cache):
        print(u"Distance: {}".format(distance(wx.GetMousePosition(), primary_mouse_location)))


def main():
    global arguments

    arguments = docopt(__doc__, version=__version__)

    app = wx.App(False)
    frame = wx.Frame(None)
    app.SetTopWindow(frame)

    if not arguments['--nogui']:
        TaskBarIcon(frame)

    timer = wx.Timer(frame, TIMER_ID)
    frame.Bind(wx.EVT_TIMER, on_timer, timer)
    timer.Start(200)
    app.MainLoop()


if __name__ == '__main__':
    main()
