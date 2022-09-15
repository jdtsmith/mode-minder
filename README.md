# mode-minder
Show all major and minor modes available in Emacs.

![image](https://user-images.githubusercontent.com/93749/190445473-8031e25c-88da-426f-a8f8-85c7350f2f81.png)

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
