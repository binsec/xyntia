
# Table of Contents

1.  [Requirements](#requirements)
2.  [Installation](#installation)
3.  [Usage](#usage)
    1.  [Synthesizing functions from fun.ml predefined functions](#synthesizing-functions-from-funml-predefined-functions)
    2.  [Synthesizing functions from sampling files](#synthesizing-functions-from-sampling-files)
    3.  [Grammar abbreviations](#grammar-abbreviations)
    4.  [Heuristic abbreviations](#heuristic-abbreviations)
4.  [Experiments](#experiments)
5.  [Synthesize all blocks from a execution trace](#synthesize-all-blocks-from-a-execution-trace)
6.  [Synthesize a specific output](#synthesize-a-specific-output)
7.  [References](#references)


# Requirements

## System requirements

On debian like systems, run the following command: 
```
sudo apt install libgmp3-dev gcc-multilib gdb python3 python3-pip python3-tqdm z3
```

## OCaml requirements

-   [OCaml](https://ocaml.org/docs/install.fr.html) (>= 4.09)
-   [Dune](https://github.com/ocaml/dune) (>= 1.11.4)
-   [Zarith](https://github.com/ocaml/Zarith)
-   [Yojson](https://github.com/ocaml-community/yojson) (>= 1.7.0)
-   [Qcheck](https://github.com/c-cube/qcheck/) (>= 0.20)

You can install all the packages above with [opam](https://opam.ocaml.org/).

## Symbolic execution engine

Xyntia is a standalone tool which takes I/O example as input and synthesize a corresponding expression. 
Still, in practice, you do not want to give these I/O examples by hand. 
Thus we give scripts to automatically sample them from a given binary. 
It relies on the Binsec symbolic execution engine:
-   [Binsec](https://binsec.github.io/) (version >= 0.6.3)

# Installation

First, if you do not have a opam switch for an ocaml version >= 4.09 do:
```
$ opam switch create . 4.09 # or any version >= 4.09
$ eval $(opam env)
```

Then you can install ocaml dependencies presented previously using `opam install <package-name>`.

Finally, to install Xyntia, execute:
```
$ make
$ make install
```

# Usage

The help of Xyntia is available through `xyntia -help`. In the following we will explain the two ways for using Xyntia. 

## Synthesizing functions from fun.ml predefined functions

The module Fun from src/fun.ml contains a list of sample functions that can be used to test Xyntia without the need of processing a sampling file.
To run Xyntia on one of the functions in Fun, run the following command:

```
$ xyntia [-ops <grammar>] [-time <time>] [-heur <heur>] -nosamp <id> <nargs>
```

where *grammar* is the abbreviation of the grammar used to define the search space (see below), *time* is the time budget of the synthesis in seconds, *heur* is the abbreviation for the search heuristic to be used (see below), *id* is the index of the function in the list, and *nargs* is the number of arguments that the function takes as input.


## Synthesizing functions from sampling files

To synthesize a function from a sampling file, execute the following command:

```
$ xyntia [-ops <grammar>] [-time <time>] [-heur <heur>] <file.json>
```
where *grammar* is the abbreviation of the grammar used to define the search space (see below), *heur* is the abbreviation for the search heuristic to be used (see below), *time* is the time budget of the synthesis in seconds, and *file* is the path of the sampling file.

The sampling file should be in the format produced by Syntia's random sampling module. The file `examples/samples/example.json` is an example of a sampling file.


## Grammar abbreviations

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">grammar</th>
<th scope="col" class="org-left">abbreviation</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Mixed Boolean Arithmetic (MBA)</td>
<td class="org-left">mba</td>
</tr>


<tr>
<td class="org-left">MBA+Division</td>
<td class="org-left">expr</td>
</tr>


<tr>
<td class="org-left">MBA+Division+Mod+Shift</td>
<td class="org-left">full</td>
</tr>


<tr>
<td class="org-left">MBA+Shift</td>
<td class="org-left">mba_shift</td>
</tr>


<tr>
<td class="org-left">MBA+If then else</td>
<td class="org-left">mba_ite</td>
</tr>
</tbody>
</table>

## Heuristic abbreviations

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">heuristic</th>
<th scope="col" class="org-left">abbreviation</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Iterated Local Search</td>
<td class="org-left">ils</td>
</tr>


<tr>
<td class="org-left">Hill Climbing</td>
<td class="org-left">hc</td>
</tr>


<tr>
<td class="org-left">Random Walk</td>
<td class="org-left">rw</td>
</tr>


<tr>
<td class="org-left">Simulated Annealing</td>
<td class="org-left">sa</td>
</tr>


<tr>
<td class="org-left">Metropolis-Hastings</td>
<td class="org-left">mh</td>
</tr>
</tbody>
</table>

# Experiments

All datasets and scripts are given to reproduce expriments presented in [1]. Especially, it contains the B1 dataset from the [Syntia](https://github.com/RUB-SysSec/syntia) paper [2] 
(Thank you [Tim Blazytko](https://synthesis.to/) for sharing it with us), our B2 dataset and the datasets used to evaluate anti-black-box deobfuscation.

## Python dependencies

To facilitate installation, we also give the `requirements.txt` to easily install the python dependencies.

To create and activate a python environment, execute the following commands (optional):
```
$ python3 -m venv <name-virtualenv> # create a virtual environment for python3
$ source <name-virtualenv>/bin/activate # active the virtual environment
```

Then, install dependencies:
```
$ pip install -r requirements.txt
```

## Launch expriments

Datasets used in [1] can be found in the `./datasets` directory.
To launch Xyntia over a dataset (e.g., B2) with a given timeout (e.g., 1s) execute the following commands:

```
$ python3 ./scripts/bench/sample.py --bench ./datasets/b2 --out <resdir>
$ python3 ./scripts/bench/bench.py --bench ./datasets/b2 --timeout 1 --out <resdir>
```

The option and their meanings can be found through the `--help` option.


# Synthesize all blocks from a execution trace

We also give `./scripts/utils/all_from_trace.sh` which traces code execution with GDB, extracts each code block executed, samples and synthesizes them.
The manual is available through `./scripts/utils/all_from_trace.sh --help` and it can be run as follows: 

```
$ ./scripts/utils/all_from_trace.sh --outdir <resdir> --all -- binary arg1 arg2 ...
```

Here is an example:

```
$ cd examples/bin && make && cd -
$ ./scripts/utils/all_from_trace.sh --outdir <resdir> --all -- ./examples/bin/add
```

# Synthesize a specific output

The previous section explained how to synthesize each output of each basic block. However, you may be interested in only one output of one basic block.
We explain how to do it for the `add` function of `./examples/bin/add` and the `eax` output.

First, you need to trace the code:
```
$ ./scripts/utils/all_from_trace.sh --outdir <resdir> --gdb -- ./examples/bin/add
```

Then you need to find the basic block of interest in `<resdir>/add/cfgs`. In our case, this is the file `<resdir>/add/cfgs/0x56556191.bin`. 
Now you can sample the `eax` output and run Xyntia over it:

```
$ python3 ./scripts/utils/binsec/sample.py --bin <resdir>/add/cfgs/0x56556191.bin --reg_out eax --out 0x56556191_eax
$ xyntia 0x56556191_eax/out_0.json
```

# References

[1] Menguy, G., Bardin, S., Bonichon, R., & Lima, C. D. S. (2021, November). Search-Based Local Black-Box Deobfuscation: Understand, Improve and Mitigate. In Proceedings of the 2021 ACM SIGSAC Conference on Computer and Communications Security (pp. 2513-2525).

[2] Blazytko, T., Contag, M., Aschermann, C., & Holz, T. (2017). Syntia: Synthesizing the semantics of obfuscated code. In 26th USENIX Security Symposium (USENIX Security 17) (pp. 643-659).
