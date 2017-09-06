# Introduction to OCaml PADS

## What is OCaml PADS?

OCaml PADS is a data-description language embedded in OCaml using PPX extension
points. It can be used to easily generate safe parsing and storing functions for
ad hoc data formats.  OCaml PADS is an OCaml implementation of [PADS][padsproj].

<a name="install"/>
## Installation

OCaml PADS requires OCaml 4.05.0 or greater and a few OPAM packages.

You can get all the prerequisites by following these steps:

1. [Install OPAM][opamInstall]
2. Run the following commands in your shell:

   > opam update

   > opam upgrade

   > opam switch 4.05.0

   > eval $(opam config env)

   > opam install ocamlfind oasis utop menhir core

4. Clone this repository
5. In the root of the clone, run:

   To build:

   > make && make install

   To rebuild:

   > make && make reinstall

   If you get errors, try

   > ocamlfind remove pads

<a name="usage"/>
## Usage

See the `sample` example for a quick overview of most of the currently available
PADS constructs. This example (along with any other future examples) can be run
by entering the examples directory and running:

> `make [exName]` 

> `[exName].native`

where `exName` should be `sample` to compile and run the `sample` example.

In the examples directory, there is a script for
desugaring a PADS source file into vanilla OCaml used by running:

> desugar.sh [file.ml]

There are two recommended ways to build a PADS source file using
either ocamlfind or ocamlbuild (which uses ocamlfind):

> ocamlfind ocamlc -thread -package pads,pads.pads_ppx -linkpkg [file.ml]

> ocamlbuild -use-ocamlfind -pkgs pads.pads_ppx -tags thread,'warn(-30)' -I
  [project folder containing fileName.ml] [fileName].native
  
Where pads.pads_ppx imports the extension interpreter for PADS and the
thread tag is required due to some other libraries.
  
[padsproj]: http://pads.cs.tufts.edu/ "PADS Project"
[opamInstall]: https://opam.ocaml.org/doc/Install.html
"How to install OPAM"