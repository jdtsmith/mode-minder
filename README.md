# mode-minder
Show all major and minor modes available in Emacs.

## Usage
`M-x mode-minder`

## Information Displayed

- Nested heirarchy, useful for targetting mode-hooks, etc.
- One-line mode description
- A clickable help link for the mode
- An origin tag:
    - `(P)` for modes found in packages installed by the default package manager
    - `(O)` for other non-builtin packages

Note: on first invocation, all known mode libraries are loaded with `require`.
