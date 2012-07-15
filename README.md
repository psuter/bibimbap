Bibimbap
========

`bibimbap` is a tool to import BibTeX entries from various sources. It runs in
the console and is designed to be simple and fast. Be advised that it still
won't read the papers for you, though.

When properly configured, here is how a session can look like:


             __    _ __    _           __                     
       ———  / /_  (_) /_  (_)___ ___  / /_  ____ _____  ——————
      ———  / __ \/ / __ \/ / __ `__ \/ __ \/ __ `/ __ \  ———— 
     ———  / /_/ / / /_/ / / / / / / / /_/ / /_/ / /_/ /  ———  
    ———  /_.___/_/_.___/_/_/ /_/ /_/_.___/\__,_/ .___/  ———   
                                              /_/             

    bibimbap> search bryant boolean
    
    [0] R.E.Bryant, "Boolean Analysis of MOS Circuits", IEEE Trans. on CAD of Integrated Circuits and Systems, 1987
    [1] R.E.Bryant, M.N.Velev, "Boolean satisfiability with transitivity constraints", ACM Trans. Comput. Log., 2002
    [2] R.E.Bryant, "Graph-Based Algorithms for Boolean Function Manipulation", IEEE Trans. Computers, 1986
    [3] R.E.Bryant, "Symbolic manipulation of Boolean functions using a graphical representation", DAC, 1985
    [4] R.E.Bryant, M.N.Velev, "Boolean Satisfiability with Transitivity Constraints", CAV, 2000
    [5] R.E.Bryant, "Reasoning about Infinite State Systems Using Boolean Methods", FSTTCS, 2003
    
    bibimbap> show 3
    
    @inproceedings{Bryant85SymbolicManipulationBooleanFunctionsUsingGraphical,
            author = {Randal E. Bryant},
             title = {Symbolic manipulation of Boolean functions using a graphical representation},
         booktitle = {DAC},
             pages = {688--694},
              year = {1985}
    }
    
    bibimbap> import 3

...and that famous BDD paper is now imported in `managed.bib`. Moreoever, the
corresponding key is copied to the system clipboard.
