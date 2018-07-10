# ProdProc Modeler

*ProdProc* is a declarative constraint based framework for product and production process modeling. Its purposes are.

1. To allow a user to model a configurable product in terms of components, relations between components, and compatibility relations between component configurable characteristics.
1. To allow a user to model aspects of the production process for a product that may affect product configuration.
1. To allow a user to couple a product and a process description, in order to avoid or reduce planning impossibilities due to product configuration, and configuration impossibilities due to production planning.

*ProdProc Modeler* is a modeling system that allows a user to define a model of a configurable product and its production process using the *ProdProc* framework, to check model syntactic correctness, and to automatically generate product instances to check model validity.

![Screenshot of ProdProc Modeler](https://github.com/dario-campagna/ProdProc-Modeler/blob/master/images/modeling_tool_screenshot.jpg)

A detailed description of *ProdProc* and *ProdProc Modeler* can be found in: "[D. CAMPAGNA. Product and Production Process Modeling and Configuration. PhD thesis, Università degli Studi di Perugia, 2012](https://github.com/dario-campagna/ProdProc-Modeler/blob/master/PhD%20Thesis%20-Dario%20Campagna.pdf)".

## Architecture and implementation details

*ProdProc Modeler* consists of three main parts: a graphical user interface, a syntax checker, and a validity checker.

![ProdProc Modeler architecture](https://github.com/dario-campagna/ProdProc-Modeler/blob/master/images/modeling_tool_arch.jpg)

The graphical user interface has been implemented using the `PCE` library of SWI Prolog. The interface allows a user to create and save *ProdProc* models. Each model is saved into two files, one for the graphical elements drawn by a user (a file with extension `prp`), and one for the Prolog predicates defining its Prolog representation (a file with extension `pl`). Basically, the Prolog representation associates to each *ProdProc* modeling feature a predicate having has arguments the constituting elements of the feature. This representation is automatically generated once a graphical model has been created. For each (graphical) element of the model the interface creates the corresponding Prolog predicate.

Once a model has been saved, the user can starts the syntax and the validity checking of the model. Both the syntax checker and the validity checker have been implemented in Prolog. The validity checker uses the `CLPFD` library of SWI Prolog in order to generate *ProdProc* instances. The syntax checker takes as input only the Prolog representation of a model, while the validity checker requires also the user to set some parameters. Both the syntax checker and the validity checker return the results of their computation to the user interface upon termination.

## Usage

1. Install [SWI Prolog](http://www.swi-prolog.org/index.html).

1. *ProdProc Modeler* needs the [`clpfd` library](http://www.swi-prolog.org/man/clpfd.html) to run. To check if the it is available, run the following command.

    ```swipl -g 'use_module(library(clpfd)).'```

    If SWI Prolog does not report any error, then the `clpfd` library is installed. Otherwise you need to manually install it.
    
1. *ProdProc Modeler* needs the [`xpce` package](http://www.swi-prolog.org/packages/xpce/) to display the graphical interface. To check if it is installed, run the following command.

    ```swipl -g 'use_module(library(pce)).'```

    If SWI Prolog does not report any error, then the `pce` package is available. Otherwise you need to manually install it.

1. To run *ProdProc Modeler*, move to the `source` directory and type the following command

    ```swipl -s prodproc_modeler.pl -g 'prodproc_modeler.'```

## Examples

The `example` directory contains two *ProdProc* models. `simple.prp` is a basic model as the name suggest. `building.prp` is the model presented in the [PhD Thesis](https://github.com/dario-campagna/ProdProc-Modeler/blob/master/PhD%20Thesis%20-Dario%20Campagna.pdf) for a building and its construction process.