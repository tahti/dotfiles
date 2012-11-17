# -*- coding: utf-8 -*-
import codecs
 
try:
    # Python 2.x. Jesieli używamy Pythona > 2.x, bedzie exception.
    from tkFileDialog import askopenfilename
except ImportError:
    # Python 3.x 
    from tkinter.filedialog import askopenfilename
 
# Wersja bez reload(sys), którego nie ma w Pythonie 3.x.
#Ten skript dzieje i w Pythonie 3.x i  w Pythonie 2.x, kodowania jest tylko w "codecs"
 
imiePlikOrigynalny = askopenfilename()
 
stareKodowaniePliku = 'windows-1250' #regionalna; czasami może być 'iso-8859-2' (i są inne kodowanie dla innych regionów jęzikowych)
plikOrigynalny = codecs.open(imiePlikOrigynalny, 'r', stareKodowaniePliku)
 
imieNowegoPliku = imiePlikOrigynalny[0:len(imiePlikOrigynalny)-4] + "_NOWY"+imiePlikOrigynalny[len(imiePlikOrigynalny)-4:]
 
nowyPlik = codecs.open(imieNowegoPliku, 'w', 'utf-8')
 
for kreska in plikOrigynalny.readlines():
    nowyPlik.write(kreska) # kreska "windows-1250" --> do pliku UTF-8  (= ąćęńłóśżźĄĆĘŃŁÓŚŻŹ)
 
plikOrigynalny.close()
nowyPlik.close()
