# mode-minder
Show all major and minor modes available in Emacs.

![mm](https://user-images.githubusercontent.com/93749/190288087-bf5c3541-4676-485c-9447-933b24b75a63.png)


## Usage
`M-x mode-minder`

## Information Displayed

- Nested major mode heirarchy, useful for targeting mode-hooks, etc.
- Standalone major modes
- Minor modes
- One-line mode description
- A clickable help link for the mode
- An origin tag:
    - `(P)` for modes found in packages installed by the default package manager
    - `(O)` for other non-builtin packages

Note: on first invocation, all known major-mode libraries are loaded with `require`.
