#!/usr/bin/perl

####### README ##################################################################{{{
#
# dmenuLauncher is a program launcher that saves number of calls and opional the
# arguments for each program.
# 
# It uses dmenu to display the possible programs and select one.
# 
# dmenuLauncher is designed to be small and fast so that even on old systems
# programs can be started very comfortable and quick.
# 
#
# Configuration 
# =============
#
# - Look at the configuration section below and edit it to your needs 
# - Give execution permissions to dmenuLauncher.pl: chmod +x dmenuLauncher.pl 
# - Run dmenuLauncher in update mode: ./dmenuLauncher.pl -u 
# - Now all programs in your $PATH are added to your pathfile 
# - Configure a shortcut for dmenuLauncher.pl in your window manager
# 
#
# Usage 
# =====
# 
# Program execution 
# -----------------
#
# - Call dmenuLauncher.pl with your shortcut.  
# - A list of programs (in alphabetical order) will be shown at the top of 
#   your screen.  
# - Type some characters of the program you want to start until the desired 
#   program is selected.  
# - You can also use the arrow keys to navigate to the program 
# - Press ENTER to start the program.
# 
# 
# Program execution with arguments 
# --------------------------------
#
# - Call dmenuLauncher.pl and type in some characters until the desired command
#   is selected.  
# - Press TAB to automato complete the command.  
# - Type in the arguments the Program should be started with.  
# - Press ENTER to start the # program with the arguments.
#
#
# Execution in Terminal 
# --------------------- 
# To start a program in the terminal just append a ; to your program name.
#
# Example: free -m;
#
# This option will be saved and the program will be started in terminal
# automatically next time even if you call it without or with oter arguments.
#
# You can again append a ; to toggle this option.  
#
# If you want to start a command that ends with ; you can escape it with \
# 
#
# Saving of arguments 
# -------------------
# 
# If the configuration variable $SAVE_ARGUMENTS is set to 1 all arguments are
# saved automatically.  The number of calls is saved for each argument.
# 
# If $SAVE_ARGUMENTS is set to 0 arguments are not saved
#
# You can toggle the option $SAVE_ARGUMENTS by appending a . to the command. 
#
# Example: free -m.  
# If $SAVE_ARGUMENTS is set to 0 the argument -m is saved If
# $SAVE_ARGUMENTS is set to 1 the argument -m is NOT saved
# 
# If you want to start a command that ends with . you can escape it with \
# 
# ATTENTION: It is not possible to use ; and . at the same time!
# 
#
# Optimization 
# ------------ 
# If the pathfile gets too large you can optimize it by removing all programs 
# that were never started (or started less than a defined number of times).  
#
# Syntax: dmenuLauncher.pl -o [count] 
# count is the minimum number of program starts for a program to stay in the 
# pathfile.  
# When no count is given it will be set to 1.
# 
# Example: dmenuLauncher.pl -o 3 
# Removes all programs that have been started less than 3 times 
# with dmenuLauncher
# 
# dmenuLauncher.pl -o 
# Removes all programs that have never been started
# 
# Programs with saved arguments will never be removed
#
#
# Update 
# ------ 
# If you have installed or removed programs the pathfile is
# outdated.  New programs are not listed and removed one are still listed in the
# pathfile
# 
# To update the pathfile run dmenuLauncher in update mode: dmenuLauncher.pl -u
#
###################################################################################}}}

my $VERSION = "0.1.1";

### BEGIN CONFIGURATION ####{{{

# configure pathfile
# in this file all commands are stored with number of calls and (opional) arguments
my $PATHFILE=$ENV{'HOME'}."/.dmenuLauncherPath";

# configure mode: horizontal or vertical
# should dmenu be called in vertical mode? (not supported by all versions)
my $MODE = "vertical";

# configure number of lines to display in vertical mode
# how many lines should be displayed by dmenu when called in vertical mode
my $NUMBER_OF_LINES = 20;

# configure terminal to execute command with when called with ;
# it is possible to start programs in a terminal. Here you can confihure the terminal command the programs should be started at
# %COMMAND$ will be replaced by the command that should be started
my $TERMINAL = "xterm -T %COMMAND% -n %COMMAND% -hold -e %COMMAND%";

# configure string that will be inserted before the command
my $BEFORE_COMMAND = "cd ~ && ";

# configure saving of arguments: if set to 1 all arguments are saved with the command
# ATTENTION: with this option set to 1 the pathfile can get very large which may result in longer loading time
my $SAVE_ARGUMENTS = 0;

# configure dmenu colors and prompt
my $NB="#0f0f0f"; # normal background color
my $NF="#4e7093"; # normal foreground color
my $SB="#003d7c"; # selected background color
my $SF="#3a9bff"; # selected foreground color

### END CONFIGURATION ####}}}


### BEGIN PROCESS ARGUMENTS ####{{{

if($ARGV[0] eq "--help"){
    printHelp();
    exit;
}

if($ARGV[0] eq "--version"){
    print "\ndmenuLauncher.pl Version: $VERSION\n\n";
    exit;
}

if($ARGV[0] eq "-u"){
    update();
    exit;
}

if($ARGV[0] eq "-o"){
    my $minCount = 1;
    if(defined $ARGV[1]){
        $minCount = $ARGV[1];
    }
    optimize($minCount);
    exit;
}

my $opts = "";
if($ARGV[0] eq "-v" or $MODE eq "vertical"){
    $opts = "-l ".$NUMBER_OF_LINES;
}

if($ARGV[0] eq "-h"){
    $opts = "";
}

### END PROCESS ARGUMENTS ####}}}


### BEGIN PROGRAM START DEFAULT ####{{{

my @file = @{loadFile()};
my $commandInput = "";
getAllCommands(\$commandInput,\@file);

my $commandString = "echo \$(echo '$commandInput' | dmenu -nb \"$NB\" -nf \"$NF\" -sb \"$SB\" -sf \"$SF\" $opts)";
my $commandLine=`$commandString`;
$commandLine = trim($commandLine);

if(defined $commandLine and $commandLine ne ""){
    # toggle open in terminal if called with ;
    my $toggleTerminal = 0;
    if($commandLine =~ /[^\\];$/){
        $commandLine =~ s/;$//;
        $toggleTerminal = 1;
    }

    # toggle SAVE_ARGUMENTS with if called with .
    if($commandLine =~ /[^\\]\.$/){
        $commandLine =~ s/\.$//;
        $SAVE_ARGUMENTS = 1 - $SAVE_ARGUMENTS;
    }
    my @newFile;
    my $knownCommand = 0;
    my $openInTerminal = $toggleTerminal;
    my @commandTokens = split(/ /, $commandLine);
    my $cmd = shift(@commandTokens);
    my $cmdArguments = join(' ',@commandTokens);
    foreach my $line (@file){
        my ($command,$count,$savedOpenInTerminal,$argumentLine) = splitLine($line);
        $line = trim($line);
        $savedOpenInTerminal = 0 if not defined $savedOpenInTerminal;

        if($command eq $cmd){
            $savedOpenInTerminal = 1-$savedOpenInTerminal if $toggleTerminal;
            $knownCommand = 1;
            $openInTerminal = $savedOpenInTerminal;
            if($cmdArguments eq ""){
                push(@newFile, $command." ".++$count." ".$savedOpenInTerminal." ".$argumentLine);
            } else {
                push(@newFile, $command." ".$count." ".$savedOpenInTerminal." ".processArguments($cmdArguments,$argumentLine));
            }
        } else {
            push(@newFile, $line);
        }
    }
    if($knownCommand == 0){
            push(@newFile, $cmd." 1 ".$openInTerminal);
            @newFile = sort @newFile;
    }
    saveFile(\@newFile);

    if(defined $BEFORE_COMMAND and $BEFORE_COMMAND ne ""){
        $commandLine = $BEFORE_COMMAND.$commandLine;
    }

    my $execCommand = $commandLine;

    if($openInTerminal){
        $execCommand = $TERMINAL;
        $execCommand =~ s/%COMMAND%/'$commandLine'/g;
    }
    exec($execCommand);
}

sub processArguments(){
    my $cmdArguments = shift;
    my $argumentLine = shift;
    if($SAVE_ARGUMENTS){
        my @arguments = split(/\|\|\|/,$argumentLine);
        my $knownArguments = 0;
        my $newLine = "";
        foreach my $argumentWithCount (@arguments) {
            next if $argumentWithCount eq "";
            my ($argCount, $arg) = split(/\|#\|/, $argumentWithCount);
            if($arg eq $cmdArguments){
                $newLine .= "|||".++$argCount."|#|$cmdArguments";
                $knownArguments = 1;
            } else {
                $newLine .= "|||".$argumentWithCount;
            }
        }
        if(not $knownArguments){
            $newLine .= "|||1|#|$cmdArguments";
        }
        return $newLine;
    } else {
        return $argumentLine;
    }
}

### BEGIN PROGRAM START DEFAULT ####}}}


### BEGIN FUNCTIONS FOR SPECIAL OPERATIONS ####{{{

sub getAllCommands(){
    my $commandInputRef = shift;
    my $filePointer = shift;
    my @commandLines;
    foreach my $line (@{$filePointer}){
        my ($command,$count,$savedOpenInTerminal,$argumentLine) = splitLine($line);
        my @arguments = split(/\|\|\|/,$argumentLine);
        push(@commandLines,[$count, $command]);
        foreach my $argumentWithCount (@arguments) {
            next if $argumentWithCount eq "";
            my ($argCount, $arg) = split(/\|#\|/, $argumentWithCount);
            push(@commandLines, [$argCount, $command." ".$arg])
        }
    }
    @commandLines = reverse @commandLines;
    my @sorted = reverse sort {$a->[0] <=> $b->[0]} @commandLines;
    
    my $result = "";
    foreach my $commandPointer (@sorted) {
        my @cmd = @{$commandPointer};
        $result .= $cmd[1]."\n";
    }
    $$commandInputRef = $result;
}

sub update(){
    print "updating ...\n";
    my @file = @{loadFile()};
    if(not @file){
        print "file '".$PATHFILE."' not found: creating ...\n";
        initPathfile();
        print "... creating file '".$PATHFILE."' done\n";
        print "updating done ...\n";
        exit;
    }
    my @commands = `dmenu_path`;
    my @newFile;
    my $oldPointer = 0;
    my $newPointer = 0;
    while($oldPointer <= $#file or $newPointer <= $#commands){
        my ($oldCommand,$count,$savedOpenInTerminal,$argumentLine) = splitLine($file[$oldPointer]);
        $savedOpenInTerminal = 0 if not defined $savedOpenInTerminal;

        my $newCommand = $commands[$newPointer];
        $oldCommand = trim($oldCommand);
        $newCommand = trim($newCommand);
        $count = trim($count);
        $savedOpenInTerminal = trim($savedOpenInTerminal);
        if(not defined $oldCommand){
            push(@newFile, $newCommand." 0 0");
            $newPointer++;
            next;
        }
        if(not defined $newCommand){
            push(@newFile, $oldCommand." ".$count." ".$savedOpenInTerminal." ".$argumentLine);
            $oldPointer++;
            next;
        }
        if($oldCommand lt $newCommand){
            $oldPointer++;
        } elsif($oldCommand gt $newCommand){
            push(@newFile, $newCommand." 0 0");
            $newPointer++;
        } else {
            push(@newFile, $oldCommand." ".$count." ".$savedOpenInTerminal." ".$argumentLine);
            $oldPointer++;
            $newPointer++;
        }
    }
    saveFile(\@newFile);
    print "updating done ...\n";
}

sub optimize(){
    my $minCount = shift;
    my @commandsLines = @{loadFile()};
    my @newFile;
    print "optimizing with mincount: $minCount\n";
    foreach my $line (@commandsLines){
        my ($command,$count,$savedOpenInTerminal,$argumentLine) = splitLine($line);
        
        if($count >= $minCount){
            push(@newFile, $command." ".$count. " ".$savedOpenInTerminal." ".$argumentLine);
        } elsif (defined $argumentLine and $argumentLine ne ""){
            push(@newFile, $command." ".$count. " ".$savedOpenInTerminal." ".$argumentLine);
        }
    }
    saveFile(\@newFile);
    print "optimizing done\n";
}

sub initPathfile(){
    my @commands = `dmenu_path`;
    my @newFile;
    foreach my $command (@commands){
        $command = trim($command);
        push(@newFile, $command." 0 0");
    }
    saveFile(\@newFile);
}

sub printHelp(){
    print "\nVersion: dmenuLauncher $VERSION\n\n";
    print "Syntax: dmenuLauncher [option]\n\n";
    print "Call without commands to select programs for starting.\n";
    print "If you type a ';' after the command it is opened in the terminal and dmenuLauncher will save the option for this command.\n";
    print "If you don't want to open the command in terminal any longer just type a ';' after the command again.\n";
    print "To toggle the SAVE_ARGUMENTS option temporarily for a specific command just type '.' after the command.\n";
    print "You can't use '.' and ';' at the same time.\n";
    print "You can escape '.' and ';' with a '\\'.\n\n";
    print "Options:\n";
    print "    --help               print this help\n";
    print "    --version            print version information\n";
    print "    -u                   update pathfile: update after installing of new programs\n";
    print "                         When pathfile does not exist it will be generated\n";
    print "    -o                   optimize pathfile: remove commands from pathfile, that were never used\n";
    print "    -o [minCount]        optimize pathfile: remove commands from pathfile, that were used less than minCount times\n";
    print "    -v                   vertical mode: print commands in dmenu vertical mode\n";
    print "    -h                   horizontal mode: print commands in dmenu horizontal mode\n\n";
}

### END FUNCTIONS FOR SPECIAL OPERATIONS ####}}}


### BEGIN UTIL FUNCTIONS ####{{{

sub loadFile(){
    my @content;
    open(FILE,"<$PATHFILE");
    @content = <FILE>;
    close(FILE);
    return \@content;
}

sub saveFile(){
    my $contentRef = shift;
    open(FILE, ">$PATHFILE");
    foreach my $line (@{$contentRef}){
        $line = trim($line);
        print FILE $line."\n";
    }
}

sub splitLine(){
    my $line = shift;
    $line = trim($line);
    my @lineTokens = split(/ /, $line);
    my $command = shift @lineTokens;
    my $count = shift @lineTokens;
    my $savedOpenInTerminal = shift @lineTokens;
    my $argumentLine = trim(join(' ',@lineTokens));
    return $command,$count,$savedOpenInTerminal,$argumentLine;
}

sub trim(){
    my $string = shift;
    $string =~ s/^\s+|\s+$//g;
    return $string;
}


### END UTIL FUNCTIONS ####}}}

# tell vim to use foldings for this file:
# vim: foldmethod=marker foldmarker={{{,}}} foldlevel=0 
