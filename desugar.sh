#!/bin/bash

ppxdir=$(ocamlfind query pads)/../../bin
ocamlfind ppx_tools/rewriter $ppxdir/ppx_pads $1
