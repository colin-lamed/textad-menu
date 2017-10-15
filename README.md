# textad-menu

[![Build Status](https://travis-ci.org/colin-passiv/textad-menu.svg?branch=master)](https://travis-ci.org/colin-passiv/textad-menu)

## A menu-driven text adventure motor

Will build a command line story, and a browser app (GHCJS).

Includes a demo story - Captain Fate


## TODO
- Document (in code & here)

### DSL

- Object must have some fields set  e.g. _oTitle, _oDescr, - this can be enforced in ObjectBuilderSyntax - i.e. must have SetOTitleF, SetODescrF ?
  _oTitle      = error "oTitle not defined"
  Applies to Room and Story too.
- setOTitle should only be set once. Similarly multiple "setOUse $ with benny" will just replace the previous value

- when nothing to say to benny, either printLn "You have nothing to say" or hide Talk button

- enable fallback for use with - e.g.

  ```haskell
  setOUse $ do
    with o1 $ do ...
    with anyother $ do
  ```

- Add sInit as an initial Action?

- Avoid defining Rid,Oid for all objects up front. Maybe Template Haskell?

### CommandLine View
- Back option for command-line view
- support history saving/restoring (in a file)
