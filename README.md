# mode-minder
Show all major and minor modes available in Emacs.

![image](https://user-images.githubusercontent.com/93749/198852544-d6c2f996-0a1d-44a2-bc8b-cc1c9080b710.png)

## Usage
`M-x mode-minder`

## Information Displayed

- Nested major mode heirarchy, useful for targeting mode-hooks, etc.
- Standalone major modes
- Minor modes
- One-line mode description
- A clickable help link for the mode
- An origin tag:
    - `[P]` for modes found in packages installed by the default package manager
    - `[O]` for other non-builtin packages
- _Aliases_ to the mode in parentheses, if any

Note: on first invocation, all known major-mode libraries are loaded with `require`.
