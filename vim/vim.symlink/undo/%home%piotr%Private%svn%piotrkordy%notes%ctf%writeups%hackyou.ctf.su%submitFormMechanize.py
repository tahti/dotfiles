Vim�UnDo�  �&��|���UAߢ;�GFx��%.C\����   $                                   Pw��    _�                        -    ����                                                                                                                                                                                                                                                                                                                                                             Pw�[     �               -from mechanize import urlopen, ParseResponse 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                          Pw�d    �                Qopener = mechanize.OpenerFactory(mechanize.SeekableResponseOpener).build_opener()   response = opener.open(url)   #response=urlopen(url)   "lines=response.read().splitlines()   response.seek(0)   %tokens = lines[8][:-4].partition("+")   a = int(tokens[0].strip())   b = int(tokens[2].strip())   print a,"+",b,"=",a+b   ans = str(a+b)   9form = ParseResponse(response, backwards_compat=False)[0]   form["answer"]=ans   request = form.click()    response2 = opener.open(request)   print response2.read()�                "url = "http://misteryou.ru/ppc100"5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             Pw�w     �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             Pw�y     �         "      if __name__ == "__main__":5�_�                            ����                                                                                                                                                                                                                                                                                                                                                          Pw�}    �         #      S  opener = mechanize.OpenerFactory(mechanize.SeekableResponseOpener).build_opener()     response = opener.open(url)     #response=urlopen(url)   $  lines=response.read().splitlines()     response.seek(0)   '  tokens = lines[8][:-4].partition("+")     a = int(tokens[0].strip())     b = int(tokens[2].strip())     print a,"+",b,"=",a+b     ans = str(a+b)   ;  form = ParseResponse(response, backwards_compat=False)[0]     form["answer"]=ans     request = form.click()   "  response2 = opener.open(request)     print response2.read()�         #      $  url = "http://misteryou.ru/ppc100"5�_�                             ����                                                                                                                                                                                                                                                                                                                                                          Pw��    �         #      def main():5��