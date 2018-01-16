# Lisp-like language interpreter

Just a weekend project in Ruby to explore parsing S-expression syntax
and language runtime.

- REPL using libedit/readline for history end editing
- single and double quoted ``string`` with some basic escape characters
- ``integer`` and ``float`` from Ruby
- variety of basic functions inspired by Common Lisp specification
  - ``+``, ``-``, ``*``, ``/``, ``mod``
  - ``not``, ``=``
  - ``list``, ``length``, ``car``, ``cdr``, ``cons``, ``last``
  - ``defvar``, ``setq``, ``let``, ``boundp``
  - ``lambda``
  - ``defun``, ``function`` - global scope
  - ``map``, ``reduce``
  - ``funcall``, ``apply``,
  - ``if``, ``progn``
  - ``puts``, ``print``, ``exit``
  - ``backtrace``
  - ``ruby`` eval just to have some fun
  - and maybe more
- separate function and variable namespace (Lisp-2)

## Quirks

- ``nil`` is not empty list which has interesting consequences
- numbers are interpreted as symbols and can be reassigned
- ``lambda`` only binds argument list variables, does not create closure
