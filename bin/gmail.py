#!/usr/bin/python
# Create keyring using seahorse, with name defined in KEYRING_NAME. If set to 'login' chances are it is unlocked at login.
# GMAIL_LOGIN and OWA_LOGIN are names of passwords that we look for and login names at the same time.


import os
import imaplib 
import re
import gnomekeyring as gk
import glib
import gtk

KEYRING_NAME = 'login'
APP_NAME = 'email-checker'
GMAIL_LOGIN = 'pkordy'
OWA_LOGIN = 'piotr.kordy'

def search_secret(keyring_name,name):
    items_ids = gk.list_item_ids_sync(keyring_name)
    for item_id in items_ids:
        item_info = gk.item_get_info_sync(keyring_name, item_id)
        if name == item_info.get_display_name():
            secret = item_info.get_secret()
            return secret
    else:
        return None

def get_password(message):
    dialog = gtk.Dialog("Enter login", None, 0,
                        (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                         gtk.STOCK_OK, gtk.RESPONSE_OK))
    dialog.props.has_separator = False
    dialog.set_default_response(gtk.RESPONSE_OK)

    hbox = gtk.HBox(False, 8)
    hbox.set_border_width(8)
    dialog.vbox.pack_start(hbox, False, False, 0)

    stock = gtk.image_new_from_stock(gtk.STOCK_DIALOG_AUTHENTICATION,
                                     gtk.ICON_SIZE_DIALOG)
    hbox.pack_start(stock, False, False, 0)

    table = gtk.Table(2, 2)
    table.set_row_spacings(4)
    table.set_col_spacings(4)
    hbox.pack_start(table, True, True, 0)

    label = gtk.Label("_Password")
    label.set_use_underline(True)
    table.attach(label, 0, 1, 0, 1)
    local_entry1 = gtk.Entry()
    local_entry1.set_activates_default(True)
    local_entry1.set_visibility(False)
    table.attach(local_entry1, 1, 2, 0, 1)
    label.set_mnemonic_widget(local_entry1)
    label = gtk.Label(message)
    table.attach(label, 0, 1, 1, 2)
    dialog.show_all()
    response = dialog.run()
    if response == gtk.RESPONSE_OK:
        return local_entry1.get_text()
    else:
	return None
def getUnreadImap(server,port,inbox,login,password):
    serv = imaplib.IMAP4_SSL(server,port)
    # if your imap mail server doesn't use ssl comment the above
    # line and uncomment this one.
    # serv = imaplib.IMAP4(server,port)
    serv.login(login,password)
    serv.select(inbox)
    data = str(serv.status(inbox, '(MESSAGES UNSEEN)'))
    serv.close()
    serv.logout()
    #prepare regular expression
    p=re.compile("(.*UNSEEN |\) \'\]\))")
    return int(p.sub('',data))

glib.set_application_name(APP_NAME)
keyrings = gk.list_keyring_names_sync()
if KEYRING_NAME not in keyrings:
    print 'Keyring',KEYRING_NAME,'not found'
    exit()
if gk.get_info_sync(KEYRING_NAME).get_is_locked():
    
    message = "Give password for keyring"
    while True:
       password = get_password(message)
       if password is not None:
          try:
	     gk.unlock_sync(KEYRING_NAME,password)
	     break
	  except gk.IOError:
	     message = "Password is wrong. Try again."
       else:
	  exit()
GMAIL_PASS = search_secret(KEYRING_NAME, GMAIL_LOGIN)
#OWA_PASS = search_secret(KEYRING_NAME, OWA_LOGIN)
mailserver = "imap4-3.uni.lu"
mailbox = "INBOX"
port = 993
#check gmail
com="wget -q -O - https://"+GMAIL_LOGIN+":"+GMAIL_PASS+"@mail.google.com/mail/feed/atom --no-check-certificate"
temp=os.popen(com)
msg=temp.read()
index=msg.find("<fullcount>")
index2=msg.find("</fullcount>")
try:
  fc=int(msg[index+11:index2])
except ValueError:
  fc=-1

#check owa server (uses SSL, port 993)
#fc+=getUnreadImap("imap4-3.some.com",993,"INBOX",OWA_LOGIN,OWA_PASS)

#generate message for conky
if fc==0:
#   print "^ca(1,i3-msg '[title=\"gmail\"] focus')^i(/home/piotr/.i3/icons/email.xpm) 0^ca()"
   print "^i(/home/piotr/.i3/icons/email.xpm) 0"
elif fc==-1:
   print "?"
else:
   print "^i(/home/piotr/.i3/icons/email_open_image.xpm)"+str(fc)+"^fg(red)!^fg()"
