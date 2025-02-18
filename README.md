
# Table of Contents

1.  [Requirements](#requirements)
2.  [Installation](#installation)
3.  [Usage](#usage)
    1.  [Synthesizing functions from sampling files](#synthesizing-functions-from-sampling-files)
    2.  [Synthesizing functions from a binary](#synthesizing-functions-from-a-binary)
    3.  [Grammar abbreviations](#grammar-abbreviations)
    4.  [Heuristic abbreviations](#heuristic-abbreviations)
4.  [Experiments](#experiments)
5.  [Synthesize all blocks from a execution trace](#synthesize-all-blocks-from-a-execution-trace)
7.  [References](#references)


# Requirements

## System requirements

On debian like systems, run the following command: 
```
sudo apt install libgmp3-dev gcc-multilib gdb python3 python3-pip python3-venv openjdk-17-jdk libgmp-dev pkg-config opam
```

You must also install [Ghidra](https://ghidra-sre.org/) and add the GHIDRA environment variable with the installation directory of ghidra 
```bash
export GHIDRA=<ghidra-directory>
```


# Installation

The easiest way to install xyntia is to create an opam switch. It will automatically install xyntia and its dependencies:
```
$ cd <xyntia-directory>
$ opam switch create . 4.14.1 -y # or any version >= 4.14.1
$ eval $(opam env)
```

# Usage

The help of xyntia is available through `xyntia -help`. In the following we will explain the two ways for using xyntia. 

## Synthesizing functions from sampling files

To synthesize a function from a sampling file, execute the following command:

```
$ xyntia [-ops <grammar>] [-time <time>] [-heur <heur>] <file.json>
```
where *grammar* is the abbreviation of the grammar used to define the search space (see below), *heur* is the abbreviation for the search heuristic to be used (see below), *time* is the time budget of the synthesis in seconds, and *file* is the path of the sampling file.

The sampling file should be in the format produced by Syntia's random sampling module. The file `examples/samples/example.json` is an example of a sampling file.

## Synthesizing functions from a binary

You can let xyntia sample the output from a binary and synthesize them with the 
following command:
```
$ xyntia [-ops <grammar>] [-time <time>] [-heur <heur>] -bin <binary-file> -config <config-file> 
```
where *binary-file* is the path to the binary to analyze and *config-file* is a configuration file to state which output to sample 
and how to sample (cf. [config documentation](sampler/README.md)).


For example to synthesize the `eax` output of the add function, run
```
$ cd examples/bin && make && cd -
$ cat examples/bin/add.ini
starting from <add>

set sample output stdout

explore all

hook <add:last> with 
    sample 100 eax
    halt
end

$ xyntia -bin examples/bin/add -config examples/bin/add.ini
```

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
$ python3 ./scripts/bench/bench.py --dataset datasets/b2 --out results --parallel -- xyntia -check -time 1
```

The option and their meanings can be found through the `--help` option.


# Synthesize all blocks from a execution trace

We also give `./scripts/utils/all_from_trace.sh` which traces code execution with Ghidra or GDB, extracts each code block executed, samples and synthesizes them.
The manual is available through `./scripts/utils/all_from_trace.sh --help` and it can be run as follows: 

```
$ XYNTIA="xyntia <options>" # the xyntia command to use
$ ./scripts/utils/all_from_trace.sh --outdir <resdir> --all -- binary arg1 arg2 ...
```

Here is an example:

```
$ cd examples/bin && make && cd -
$ ./scripts/utils/all_from_trace.sh --outdir <resdir> --all -- ./examples/bin/add
```

# References

[1] Menguy, G., Bardin, S., Bonichon, R., & Lima, C. D. S. (2021, November). Search-Based Local Black-Box Deobfuscation: Understand, Improve and Mitigate. In Proceedings of the 2021 ACM SIGSAC Conference on Computer and Communications Security (pp. 2513-2525).

[2] Blazytko, T., Contag, M., Aschermann, C., & Holz, T. (2017). Syntia: Synthesizing the semantics of obfuscated code. In 26th USENIX Security Symposium (USENIX Security 17) (pp. 643-659).
