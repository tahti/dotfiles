//*****************************
// Modules
//*****************************
// Tabs
require("new-tabs.js");
//Sessions
require("session.js");
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

