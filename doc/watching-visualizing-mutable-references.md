# Watching / visualizing mutable references

Replique supports using dedicated buffers to 'watch' the values of Clojure and Clojurescript mutable references over time.

## Demo

[Here] is a demo of the Replique watch feature. 

## Watching mutable references

In order to watch the values of a mutable reference such as an atom, use the `replique/watch` command (`C-c C-w`).
You will be prompted for a namespace and then a var among the vars of the chosen namespace. 

Once a var has been selected, Replique opens a dedicated buffer that shows its pretty printed value.
This buffer is a read-only buffer. It cannot be used to edit a value.
Every time the mutable reference value is changed, the buffer is marked as modified.
To display the new value, the buffer must be manually refreshed by using the `replique-watch/refresh` command (`g`).

## Watching REPL evaluation results / printing output

Just like mutable references, the REPL evaluation results / printing output can be watched by using the the `replique/watch` command (`C-c C-w`) and by selecting the `printed` or `results` entry.

## Customizing the way values are printed

The way values are printed in a watch buffer can be customized by using the `replique/params` command in a watch buffer.
The following vars are available for customization:

- \*print-level\*
- \*print-length\*
- \*print-meta\*

The value of these vars is local to the watch buffer and does not affect the global REPL behavior.

## Keeping track of multiple values

By default, Replique keeps track of the last value of the watched mutable references and the last three REPL evaluation results / printing outputs.

The number of values beeing kept track of can be modified by using the `replique/record` command (or `r` keybinding).
The `+` and `-` keybindings can be used to increment and decrement the recording size value once the recording edit menu is open.

The value currently beeing displayed in the watch buffer can be changed by using the `p` (previous) and `n` (next) keybindings.

## Browsing values

A watch buffer can display only a subtree of a nested data structure.
The `replique-watch/browse` command (`b` keybinding) opens a menu showing all the top level keys (for maps) or indexes (for sequentials) of the currently displayed data structure.
Selecting one of the keys shrinks the printed data structure to the value associated with this key.
`C-j` can be used to build a path into the printed data structure by appending the currently highlighted key to that path.
The first entry of the menu can be selected to shrink the currently printed data structure to the built path.
`C-g` leaves the currently printed data structure unmodified.

## Accessing the browsed value in the REPL

The currently browsed value is added to the metadata of the watched mutable reference under the `:replique.watch/value` key.