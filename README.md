What is an OIL Machine
======================

 - Contains several registers with arbitrary-integers, and programs consisting of sequences of one instruction
 - Instruction used is "subtract and branch if zero" (SBZ). `sbz x y l` is equivalent to

        x -= y;
        if(x == 0)
            goto l;

 - See http://en.wikipedia.org/wiki/One_instruction_set_computer

Guide
==========

 - Main documentation in oil.clj
 - macros.clj defines various helpers that expand into primitive sequences of sbz instructions
 - Examples in oil_programs.clj

Notes
=====

Written as part of a project for Computational Discrete Mathematics
at CMU in December 2010. Development led to the discovery of a bug in Clojure's basic arithmetic. ( https://groups.google.com/g/clojure/c/1IF11i-B0oE/m/EIU31liH6H0J )
