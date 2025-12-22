# gb-rgbds

A small Emacs package providing a simple Game Boy RGBDS workflow.

## Features

- Build Game Boy ROMs using RGBDS
- Run ROMs in Emulicious
- Build-and-run convenience command
- Editor-agnostic (works with Doom or vanilla Emacs)

## Requirements

- rgbasm, rgblink, rgbfix
- Java (for Emulicious)
- Emulicious emulator

## Installation (Doom Emacs)

```elisp
(package! gb-rgbds
  :recipe (:host github :repo "piratalpha/gb-rgbds"))
(use-package! gb-rgbds
  :config
  (setq gb-rgbds-emulicious-jar
        "~/tools/emulicious/emulicious.jar"))
```
## Usage

| Command                  | Description    |
|--------------------------|----------------|
| `gb-rgbds-build`         | Build ROM      |
| `gb-rgbds-run`           | Run ROM        |
| `gb-rgbds-build-and-run` | Build then run |

## Project Structure
The project root is detected by the presence of `main.asm`.
