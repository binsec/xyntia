## Xyntia Tool (CCS'21 Artefact)
#### PAPER: Search-based Local Blackbox Deobfuscation: Understand, Improve and Mitigate 
#### AUTHORS: Grégoire Menguy, Sébastien Bardin, Richard Bonichon, Cauim de Souza Lima 
#### The 28th ACM Conference on Computer and Communications Security

Details about the paper: https://binsec.github.io/new/publication/1970/01/01/nutshell-ccs-21.html

---

This repository contains major datasets used to evaluate Xyntia:
* b1 is the dataset from Blazytko et al. to evaluate Syntia; 
* b2 is our custom dataset used to extend the evaluation of Syntia and compare it to our new AI-based blackbox deobfuscator Xyntia;
* bp1, bp2 and bp3 and the three datasets used to evaluate Xyntia over complex handlers (see section 8.2);
* merged1, merged2, merged3, merged4, merged5 are the datasets used in section 8.3 to evalaute Xyntia against merged handlers (see Section 8.3). 

A virtual machine (~2.3G) is available at https://doi.org/10.5281/zenodo.5094898. It contains all these datasets, the Xyntia binary and scripts to exercise Xyntia and replay major experiments. The user password is "user" (and the root password is "root").  

