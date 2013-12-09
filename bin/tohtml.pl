#!/usr/bin/perl
#Requires libhtml-fromtext-perl package under Debian
use HTML::FromText;
use strict;
 
my $text = join '',<>;
my $html = text2html($text, urls => 1, email => 1, bold=>1, paras => 1, allcaps => 1, tables => 1, bullets => 1, underline=>1);
 
print $html;
