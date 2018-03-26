## About Inkwell
This is a client library for the Splatoon 2 Splatnet API. It offers access to player and battle statistics, basically exposing all the information available on the official Nintendo Switch mobile phone app.

## How To
This library does not currently handle the log in flow, as it is not publicly documented. You will have to provide a valid session token in order to make requests against the API. In order to obtain this token, search for [iksm_session](https://google.com/search?q=iksm_session) and use one of the methods described to sniff out the token from the phone app. Personally I've found the [ink-proxy](https://github.com/eliboa/ink-proxy) method to be the easiest and most reliable.

Once you have obtained the `iksm_session` token, you should plug it into the library:

    (ql:quickload :inkwell)
    (setf inkwell:*session* "adwdwadwaddadwadadwadawd")

From there you can retrieve information about current battles (`list-battles`, `battle`), single-player statistics (`single-player`), upcoming battle modes (`schedules`), specific users (`user`), active and past splafests (`active-festivals`, `list-festivals`, `votes`, `rankings`), and even the online shop merchandise (`shop-info`, `order`).

    (inkwell:list-battles)
    ; => (#<INKWELL:BATTLE CLAM-BLITZ VICTORY {1111}>
    ;     #<INKWELL:BATTLE CLAM-BLITZ LOSS {1110}>
    ;     #<INKWELL:BATTLE CLAM-BLITZ VICTORY {1109}>
    ;     #<INKWELL:BATTLE CLAM-BLITZ LOSS {1107}>
    ;     #<INKWELL:BATTLE CLAM-BLITZ LOSS {1106}>
    ;     ...)

The library takes care to transform the results from the API into sensible objects for you to inspect and extract information from with ease. Have a look at the symbol index to see what is offered.
