****************************************************************************

README file for FeynRules-2.1


The FeynRules package main file is FeynRules.m which should be loaded in mathematica 7 or above. It calls FeynRulesPackage.m which in turn loads the ToolBox.m as well as the various files contained in the Core and Interfaces directories.
The Core directory contains all the files for the running of FeynRules but those related to the output of other tools. Those files handle the Lagrangian, perform the renormalization, compute the Feynman rules and the two-body decays. The Interfaces directory contains all the interfaces to other tools.
The TemplateInterface.m in the interface directory can be used to start a new interface to an extra tool.

The package also contains NLOCT.m which is not loaded by FeynRules. This package should be loaded in a new mathematica kernel after loading FeynArts. It computes the one-loop UV counterterms and R2 rational terms. The inputs of this package are produced with the FeynArts interface using the renormalized Lagrangian computed in FeynRules using the OnShellRenormalization function. The result is written in a .nlo file. 
This file should be loaded in mathematica after loading FeynRules and the model file (e.g. SM.fr) and before generating the UFO output in a different mathematica kernel.
The Model directory contains the FeynRules input files for a first example and for the Standard Model as well as an example of a notebook to use each of those models.

To run the SM.nb example, open it in Mathematica 7 or higher.
The first sections up to and including the one called "Renormalization and output to FA" can be run linearly, then the mathematica kernel has to be killed (by the Quit[] at the beginning of section "Computation of the counterterms"). The section "Computation of the counterterms" can then also be run linearly after which the kernel must be killed again. Finally the sections "Load FeynRules", "Load the SM" and  "UFO at NLO" should be run. 

The process is explained in section 3 of the paper (before 3.1 and in 3.3)



More information can be found in the FeynRules manual (arXiv:1310.1921) and the NLOCT manual (arXiv:1406.3030).

File List

Core                   contains all the files for the running of FeynRules but those related to the output of other tools
FeynRules.m            main program file
FeynRulesPackage.m     next to main program file
FRPalette.nb           FeynRules tools palette
Interfaces             contains all the interfaces to other tools.
Models                 contains the FeynRules input files for two examples
NLOCT.m                Package for the computation of the NLO counterterms
Toolbox.m              tool functions of FeynRules
UpdateNotes.txt        Version history

********************************************************************************