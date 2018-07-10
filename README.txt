ProdProc Modeler - Copyright (C) 2011 Dario Campagna
Released under the terms of GNU GENERAL PUBLIC LICENSE Version 3



To execute ProdProc Modeler
---------------------------
1. Install SWI Prolog (http://www.swi-prolog.org/index.html).
   ProdProc Modeler needs the clpfd library (http://www.swi-prolog.org/man/clpfd.html)
   and the xpce package (http://www.swi-prolog.org/packages/xpce/) to run, you may need
   to manually install them.
   To check if the clpfd library is available, run the following command

		swipl -g 'use_module(library(clpfd)).'
   
   If SWI Prolog does not report any error, then the clpfd library is installed.
   To check if the xpce package is installed, run the following command
		
		swipl -g 'use_module(library(pce)).'
   
   If SWI Prolog does not report any error, then the pce library is available.
   At http://www.swi-prolog.org/build/ you can find instructions for building SWI Prolog 
   from source.
    

2. To run ProdProc Modeler, move to the directory "source" and type the following command
		
		swipl -s prodproc_modeler.pl -g 'prodproc_modeler.'



Example of ProdProc model
---------------------------
The directory "example" contains a ProdProc model for a building and its
construction process.