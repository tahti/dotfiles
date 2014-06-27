function possibly_valid_url (str) {
    return /^\s*[^\/\s]*(\/|\s*$)/.test(str)
        && /[:\/\.]/.test(str);
}

function read_url_google_translate_handler (input) {
    var m = /^(\S+\|\S+)\s+(.*)/.exec(input);
    if (m)
        return "http://translate.google.com/#"+m[1]+"|"+m[2];
    return null;
}

read_url_handler_list = [read_url_google_translate_handler,
                         read_url_make_default_webjump_handler("d")];
interactive("open-calendar", "Go to calendar.google.com", "follow", $browser_object = "http://calendar.google.com/");
// Some webjumps
define_opensearch_webjump("d", make_file("~/.conkerorrc/search_engines/ddg.xml"));
define_webjump("dict", "http://dict.leo.org/ende?lp=ende&lang=en&searchLoc=0&cmpType=relaxed&sectHdr=on&spellToler=&search=%s");
define_webjump("imdb", "http://imdb.com/find?q=%s");

