# Just a task runner
# <https://github.com/casey/just>

# shows this help message
help:
    @just -l

# converts an org-based article into markdown
article file:
    emacs -q --script elisp/export-org-article.el {{file}}

[private]
alias a := article

# converts an org-based book into markdown
book file:
    emacs -q --script elisp/export-org-book.el {{file}}

[private]
alias b := book

# composes a Zenn book directory of .md files into one .org file
md-to-org book-dir out:
    emacs -q --script elisp/md-to-org-book.el {{book-dir}} {{out}}

# previews the zenn files
preview:
    zenn preview --port 8002

[private]
alias p := preview

