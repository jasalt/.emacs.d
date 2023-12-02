Work in progress Emacs 29+ config for text and code editing based on [sr.ht/~ashton314/emacs-bedrock](https://sr.ht/~ashton314/emacs-bedrock/), with best parts of [Emacs Prelude](https://github.com/bbatsov/prelude) included ([crux.el](https://github.com/bbatsov/crux) etc) and some own modifications on top.

Mostly aimed for using org-mode and coding Python and PHP.

## Details

On original Bedrock config side, there are two files of interest: `early-init.el` and `init.el`. Follow [original repo readme](https://sr.ht/~ashton314/emacs-bedrock/) for more info.

Own modifications and stuff imported from Prelude lie at `custom.el`.

Keeping code somewhat documented and using standard `use-package` (with `straight`) for managing and configuring packages.

## Trying this out without committing too hard

Emacs 29.1 added the handy `--init-directory` flag. This means that you can run `emacs --init-directory path/to/emacs-bedrock/` and all the customizations and package installations will be isolated to the project directory. 

