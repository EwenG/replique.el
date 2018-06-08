# Watching / visualizing mutable references

Replique supports using dedicated buffers to 'watch' the values of mutable references over time.

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

Once modified, their value is local to the watch buffer and does not affect the global REPL behavior.

## Keeping track of multiple values

By default, Replique keeps track of a single value of the watched mutable references and three values of the REPL evaluation results / printing output.

The value currently beeing displayed in the watch buffer can be changed by using the `p` (previous) and `n` (next) keybindings.

The number of values beeing kept track of can be modified by using the `replique/record` command (or `r` keybinding).
The `+` and `-` keybindings can be used to increment and decrement the recording size value once the recording edit menu is open.

## Browsing values

A watch buffer can display only a subtree of a nested datastructure.
The `replique-watch/browse` command (`b` keybinding) opens a menu showing all the top level keys (for maps) or indexes (for sequentials) of the currently displayed datastructure.
Selecting one of the keys shrinks the printed datastructure to the value associated with this key.
`C-j` can be used to build a path into the printed datastructure by appending the currently highlighted key to that path.
The first entry of the menu can be selected to shrink the currently printed datastructure to the built path.
`C-g` leaves the currently printed datastructure unmodified.

## Accessing the browsed value in the REPL

The currently browsed value is added to the metadata of the watched mutable reference under the `:replique.watch/value` key.