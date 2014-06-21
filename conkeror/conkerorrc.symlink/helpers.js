var killed_buffer_urls = new Array();
var killed_buffer_histories = new Array();
interactive("my-kill-current-buffer",
    "close and save the current buffer for later restore",
    function(I) {
        if(killed_buffer_urls.length==30){
           killed_buffer_urls.shift(); // remove older item to save
           killed_buffer_histories.shift();
           // memory, just save maximum 10 buffers
       }
       killed_buffer_urls.push(I.buffer.document.URL);
       var hist = I.buffer.web_navigation.sessionHistory;
       killed_buffer_histories.push(hist);
       kill_buffer(I.buffer); //kill the current buffer
    });
interactive("my-revive-buffer",
    "Loads url from a previously killed buffer",
    function (I) {
        if (killed_buffer_urls.length > 0) {
            var url=killed_buffer_urls[killed_buffer_urls.length - 1]
           // var url = yield I.minibuffer.read(
           //     $prompt = "Restore killed url:",
           //     $completer = all_word_completer($completions = killed_buffer_urls),
           //     $default_completion = killed_buffer_urls[killed_buffer_urls.length - 1],
           //     $auto_complete = "url",
           //     $auto_complete_initial = true,
           //     $auto_complete_delay = 0,
           //     $match_required);
            var window = I.window;
            var creator = buffer_creator(content_buffer);
            var idx = killed_buffer_urls.indexOf(url);

            // Create the buffer
            var buf = creator(window, null);

            // Recover the history
            buf.web_navigation.sessionHistory = killed_buffer_histories[idx];

            // This line may seem redundant, but it's necessary.
            var original_index = buf.web_navigation.sessionHistory.index;
            buf.web_navigation.gotoIndex(original_index);

            // Focus the new tab
            window.buffers.current = buf;

            // Remove revived from cemitery
            killed_buffer_urls.splice(idx, 1);
            killed_buffer_histories.splice(idx, 1);
        } else { I.window.minibuffer.message("No killed buffer urls");
        }
    });

