//*****************************
// Leadermap
//*****************************
define_keymap("leader_keymap");
define_key(default_base_keymap, ",", leader_keymap)

//Define leader map
define_key(default_global_keymap, ", b", "switch-to-buffer")

//Unbind keys
undefine_key(content_buffer_normal_keymap,"l");
undefine_key(content_buffer_normal_keymap,"c");
undefine_key(content_buffer_normal_keymap,"t");
undefine_key(content_buffer_normal_keymap,"u");
undefine_key(content_buffer_normal_keymap,"d");
undefine_key(content_buffer_normal_keymap,"G");
//*****************************
// Globalmap
//*****************************
define_key(default_global_keymap, "M-h", "buffer-previous");
define_key(default_global_keymap, "M-n", "buffer-next");
define_key(default_global_keymap, "l", "find-url-new-buffer");
define_key(default_global_keymap, "t", "cmd_scrollLineDown");
define_key(default_global_keymap, "c", "cmd_scrollLineUp");
define_key(default_global_keymap, "G", "cmd_moveBottom");
//The following code is similar to the above, except buffer history is also restored. The command it defines is called revive-buffer. 
define_key(default_global_keymap, "u", "my-revive-buffer");
define_key(default_global_keymap, "d", "my-kill-current-buffer");
define_key(default_global_keymap, "o", "find-url");
define_key(default_global_keymap, "y", "copy");

//*****************************
// Content_buffer_map
//*****************************
define_key(content_buffer_normal_keymap, "O", "find-alternate-url");
define_key(content_buffer_normal_keymap, "n", "forward");
define_key(content_buffer_normal_keymap, "h", "back");
define_key(content_buffer_normal_keymap, "A", "bookmark");
define_key(content_buffer_normal_keymap, "f3", "open-calendar");
define_key(content_buffer_normal_keymap, 'v', 'view-source');
define_key(content_buffer_normal_keymap, '/', 'isearch-forward');
//:This binds the key 'y' in content buffers to the 'copy' command, and will cause it to prompt for any DOM node, instead of just links: 
define_key(content_buffer_normal_keymap, "Y", "copy", $browser_object = browser_object_dom_node);
//define_key(content_buffer_normal_keymap, "j", "follow");
//define_key(content_buffer_normal_keymap, "J", "follow-new-buffer-background");
//define_key(content_buffer_normal_keymap, "C-x l", "find-url");
//define_key(content_buffer_normal_keymap, "B", "list-bookmarks");
//define_key(content_buffer_normal_keymap, "f1", "open-googleAU");
//define_key(content_buffer_normal_keymap, "f2", "open-googleDE");
//define_key(content_buffer_normal_keymap, "C-x h", "open-googleAU");

// Content handlers
external_content_handlers.set("application/pdf", "okular");
