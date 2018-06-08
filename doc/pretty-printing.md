# Pretty printing

Use the `replique/pprint` command (`C-c C-c`) to pretty print the expression at point. Pretty printing can be used in any clj/cljs/cljc or [comint](https://www.emacswiki.org/emacs/ComintMode) buffer.

Internally, the Replique pretty printer does not make use of [clojure.pprint](https://clojure.github.io/clojure/clojure.pprint-api.html) and thus is not affected by its options.

The Replique pretty printer is only intended at pretty printing data.
It removes any comment found and makes no attempt to beautifully format code.