//*****************************
// Modules
//*****************************

// Tabs
require("new-tabs.js");
tab_bar_button_close=1; //"The mouse button that closes tabs." + "0 = left, 1 = middle, 2 = right, null = disabled.");
tab_bar_show_icon=true;// "Whether or not to show buffer icons in tabs.");

require("favicon.js");

//Sessions
require("session.js");
session_auto_save_auto_load = true
// auto completion in the minibuffer
minibuffer_auto_complete_default = true;
//Prevent focus stealing
require("block-content-focus-change.js");
// Open Middle-Clicked Links in New Buffers
require("clicks-in-new-buffer.js");

//*****************************
// Hooks
//*****************************
//Display Favicons
http://conkeror.org/Favicons
// favicons hook
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);

//*****************************
// Variables
//*****************************
url_completion_use_history = true; // should work since bf05c87405
url_completion_use_bookmarks = true;
//Favicons in the read_buffer completions list
read_buffer_show_icons = true;
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; // Now buffers open in background.
hint_digits="ABCDEFGHIJKLMNOPQRSTUVWXYZ";
minibuffer_read_url_select_initial=false //Specifies whether a URL presented in the minibuffer for editing should be selected. This affects find-alternate-url.

// cwd (set default download directory)
cwd=get_home_directory(); 
cwd.append("downloads");
// turn off all modes
page_mode_deactivate(cebook)
page_mode_deactivate(gmane)
page_mode_deactivate(google-maps)
page_mode_deactivate(google-voice)
page_mode_deactivate(reddit)
page_mode_deactivate(wikipedia)
page_mode_deactivate(eedly)
page_mode_deactivate(google-calendar)
page_mode_deactivate(google-reader)
page_mode_deactivate(grooveshark)
page_mode_deactivate(smbc)
page_mode_deactivate(xkcd)
page_mode_deactivate(dailymotion)
page_mode_deactivate(github)
page_mode_deactivate(google-gqueues)
page_mode_deactivate(google-search-results)
page_mode_deactivate(key-kill)
page_mode_deactivate(stackexchange)
page_mode_deactivate(youtube-player)
page_mode_deactivate(duckduckgo)
page_mode_deactivate(gmail)
page_mode_deactivate(google-images)
page_mode_deactivate(google-video)
page_mode_deactivate(newsblur)
page_mode_deactivate(twitter)
page_mode_deactivate(youtube)

