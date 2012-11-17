#!/usr/bin/perl -w
#
# Copywrite 2005-2008 - Steven Rostedt
# Copyright 2008 Phil Endecott
# Licensed under the terms of the GNU GPL License version 2
#
# This script modifies your .config so that modules that are currently
# in use (i.e. as indicated in the output of lsmod) will be built in.
# Modules that are not in use will still be built as modules.
#
# This is based on streamline_config.pl by Steven Rostedt, which
# modifies your .config to not build unused modules at all.  The changes
# from Steven's code are indicated below.

my $config = ".config";
my $linuxpath = ".";

open(CIN,$config) || die "Can't open current config file: $config";
my @makefiles = `find $linuxpath -name Makefile`;

my %objects;
my $var;
my $cont = 0;

foreach my $makefile (@makefiles) {
	chomp $makefile;

	open(MIN,$makefile) || die "Can't open $makefile";
	while (<MIN>) {
		my $catch = 0;

		if ($cont && /(\S.*)$/) {
			$objs = $1;
			$catch = 1;
		}
		$cont = 0;

		if (/obj-\$\((CONFIG_[^)]*)\)\s*[+:]?=\s*(.*)/) {
			$var = $1;
			$objs = $2;
			$catch = 1;
		}
		if ($catch) {
			if ($objs =~ m,(.*)\\$,) {
				$objs = $1;
				$cont = 1;
			}

			foreach my $obj (split /\s+/,$objs) {
				$obj =~ s/-/_/g;
				if ($obj =~ /(.*)\.o$/) {
					$objects{$1} = $var;
				}
			}
		}
	}
	close(MIN);
}

my %modules;

open(LIN,"/sbin/lsmod|") || die "Cant lsmod";
while (<LIN>) {
	next if (/^Module/);  # Skip the first line.
	if (/^(\S+)/) {
		$modules{$1} = 1;
	}
}
close (LIN);

my %configs;
foreach my $module (keys(%modules)) {
	if (defined($objects{$module})) {
		$configs{$objects{$module}} = $module;
	} else {
		print STDERR "$module config not found!!\n";
	}
}

while(<CIN>) {
	if (/^(CONFIG.*)=m/) {
		if (defined($configs{$1})) {
			# print; (streamline_config)
			print "$1=y\n";
		} else {
			# print "# $1 is not set\n"; (streamline_config)
			print;
		}
	} else {
		print;
	}
}
close(CIN);
