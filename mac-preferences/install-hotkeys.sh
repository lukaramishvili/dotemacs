#!/bin/bash
defaults write com.apple.finder NSUserKeyEquivalents ' {

        "Go to Folder..." = "@$f";
        "Hide Tab Bar" = "@~^$t";
        "New Finder Window" = "@~^n";
        "New Folder" = "@~^$n";
        Recents = "@~^$r";
        "Show Tab Bar" = "@~^$t";
    
}'
defaults write com.apple.iTunes NSUserKeyEquivalents ' {

        Equalizer = "@e";
    
}'
defaults write Apple Global Domain NSUserKeyEquivalents ' {

        Minimise = "@~^m";
        "Minimise All" = "@~^$m";
        Minimize = "@~^m";
        "Minimize All" = "@~^$m";
    
}'
defaults write com.apple.Preview NSUserKeyEquivalents ' {

        "Export..." = "@e";
        "Find..." = "@s";
        Save = "^s";
    
}'
defaults write org.videolan.vlc NSUserKeyEquivalents ' {

        "Float on Top" = "@t";
    
}'
defaults write com.microsoft.Word NSUserKeyEquivalents ' {

        "Hide Word" = "@~^$h";
        "New Document" = "@~^$n";
        "Page Setup..." = "@~p";
        Save = "^s";
        "Select All" = "@$a";
    
}'
defaults write com.googlecode.iterm2 NSUserKeyEquivalents ' {

        "Clear Buffer" = "@$k";
        Copy = "~w";
        "Find..." = "@s";
        "New Window" = "@~^$u";
    
}'
defaults write org.mozilla.firefox NSUserKeyEquivalents ' {

        "Bookmark All Tabs..." = "@~^$a";
        "Bookmark This Page" = "@~^$k";
        "Find in Page..." = "@~^$f";
        "New Private Window" = "@~^$i";
        "New Window" = "@~^n";
        "Print..." = "@~^$p";
        "Save Page As..." = "@~^$s";
    
}'
defaults write com.google.Chrome NSUserKeyEquivalents ' {

        "Bookmark All Tabs..." = "@~$d";
        "Bookmark This Page..." = "@$d";
        Extensions = "@^x";
        "Find Next" = "@s";
        "Find Previous" = "@~^$r";
        "New Incognito Window" = "@~^$i";
        "New Window" = "~n";
        "Print..." = "@~^$p";
        "Save Page As..." = "@$s";
        "Show Full History" = "@~^$h";
        "Task Manager" = "@^t";
    
}'
defaults write com.macromates.TextMate NSUserKeyEquivalents ' {

        New = "@~^$n";
    
}'
defaults write org.chromium.Chromium NSUserKeyEquivalents ' {

        "Bookmark All Tabs..." = "@~$d";
        "Bookmark This Page..." = "@$d";
        Extensions = "@^x";
        "Find Next" = "@s";
        "Find Previous" = "@~^$r";
        "New Incognito Window" = "@~^$i";
        "New Window" = "~n";
        "Save Page As..." = "@$s";
        "Show Full History" = "@~^$h";
        "Task Manager" = "@^t";
    
}'
defaults write com.microsoft.Powerpoint NSUserKeyEquivalents ' {

        Save = "^s";
    
}'
defaults write com.microsoft.Outlook NSUserKeyEquivalents ' {

        Email = "~n";
        "Select All" = "@$a";
    
}'
defaults write com.apple.Safari NSUserKeyEquivalents ' {

        "Add Bookmark..." = "@~d";
        "Find..." = "@s";
        "New Private Window" = "@~^$[";
        "New Window" = "@~^[";
        "Print..." = "@~^$p";
        Quit = "@~q";
        "Save As..." = "@$s";
    
}'
defaults write com.apple.Notes NSUserKeyEquivalents ' {

        "New Note" = "@~^n";
    
}'
defaults write com.apple.Terminal NSUserKeyEquivalents ' {

        "Jump to Next Bookmark" = "@~^$0";
        "Jump to Previous Bookmark" = "@~^$9";
    
}'
defaults write com.fluidapp.FluidApp2.Asana NSUserKeyEquivalents ' {

        "Find..." = "@s";
    
}'
defaults write com.microsoft.Excel NSUserKeyEquivalents ' {

        "Hide Excel" = "@~^$h";
        New = "~n";
        "Select All" = "@$a";
    
}'
killall cfprefsd
