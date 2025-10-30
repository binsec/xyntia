
# Table of Contents

1.  [Installation](#installation)
2.  [Usage](#usage)
    1.  [Synthesizing functions from sampling files](#synthesizing-functions-from-sampling-files)
    2.  [Synthesizing functions from a binary](#synthesizing-functions-from-a-binary)
    3.  [Grammar abbreviations](#grammar-abbreviations)
    4.  [Heuristic abbreviations](#heuristic-abbreviations)
    5.  [Inference rules](#inference-rules)
    6.  [Counter-Example Guided Inductive Synthesis (CEGIS)](#cegis)
    7.  [Generating SyGUS problems](#generating-sygus-problems)
3.  [Experiments](#experiments)
4.  [Synthesize all blocks from a execution trace](#synthesize-all-blocks-from-a-execution-trace)
5.  [References](#references)


# Installation

<details open><summary>Show</summary>

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
</details>

# Usage

The help of xyntia is available through `xyntia -help`. In the following we will explain the two ways for using xyntia. 

## Synthesizing functions from sampling files

<details open><summary>Show</summary>


To synthesize a function from a sampling file, execute the following command:

```
$ xyntia [-ops <grammar>] [-time <time>] [-heur <heur>] <file.json>
```
where *grammar* is the abbreviation of the grammar used to define the search space (see below), *heur* is the abbreviation for the search heuristic to be used (see below), *time* is the time budget of the synthesis in seconds, and *file* is the path of the sampling file.

The sampling file should be in the format produced by Syntia's random sampling module. The file `examples/samples/example.json` is an example of a sampling file.

</details>

## Synthesizing functions from a binary

<details open><summary>Show</summary>


You can let xyntia sample the output from a binary and synthesize them with the 
following command:
```
$ xyntia [-ops <grammar>] [-time <time>] [-heur <heur>] -bin <binary-file> -config <config-file> 
```
where *binary-file* is the path to the binary to analyze and *config-file* is a scriptig file to state which output to sample and how to sample. 

The scripting language used in *config-file* is an extension of the [Binsec scripting language](https://github.com/binsec/binsec/blob/master/doc/sse/references.md). It adds the following new declarations and instructions:
* `sample N [ reg_1, ..., reg_n ]`, which specify to generate N samples for each output. A list of target register outputs can be set, in such a case, only these are sampled otherwise all detected outputs are;
* `set domain VAR [MIN, MAX]` which specify the sampling domain for the VAR  input. VAR can be a register or any DBA variable but not a memory cell. To specify the domain of a memory cell, see [this example](sampler/examples/snapchat.ini);
* `prune constant outputs` which prunes all the constant outputs (i.e., with no input variables) from the set of sampled outputs;
* `set optimal sampling` to use the sampling strategy depicted in the Xyntia paper.


For example to synthesize the `eax` output of the add function, run
```
$ cd examples/bin && make && cd -
$ xyntia -bin examples/bin/add -config examples/bin/add.ini
```

In this case, the file `add.ini` is:
```
$ cat examples/bin/add.ini
starting from <add>

set sample output stdout

explore all

hook <add:last> with 
    sample 100 eax
    halt
end
```

The `starting from` instruction specifies the start of the reverse window.
The `hook` one is the end of the reverse window (remark: `<add:last>` stands for the last byte of the `add` function; it works because the last instruction of `add` is a `ret`, whose opcode is only 1 byte long).


Other examples of scripts can be found in the `sampler/examples` directory.

It is also possible to sample a symbolic expression directly. An example is given in `sampler/examples/expr.ini`. To sample it, use:
```
# The <() is here to replace the binary path. Indeed, in this case we do not need any binary (only an empty file)
xyntia -bin <() -config sampler/examples/expr.ini
```

### Equivalence checking

Leveraging Binsec to extract the semantics of a binary code snippet (or a synthetic expression) enables Xyntia to check the correctness of the synthesis. This can be done through the `-check` option.

Back to our example, you can check if the result of Xyntia is correct as follows:
```
xyntia -bin examples/bin/add -config examples/bin/add.ini -check
```
The result will be either `yes`, `no` or `ukn` (unknown i.e., the solver timeouted).

</details>

## Grammar abbreviations

<details open><summary>Show</summary>


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

</details>

## Heuristic abbreviations

<details open><summary>Show</summary>

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

</details>

## Inference rules

<details open><summary>Show</summary>


### When to use Inference Rules ?


**In Short:** If you expect your target expression to contain constant values.

**Full answer:** Program synthesis has core limitations, namely the handling of arbitrary constant values and big expressions. To bypass these limitations Xyntia includes *Inference Rules* to better guide the search and elevate in one step a candidate solutions into the target expression (possibly with arbitrary constant values). 
Hence if your target expression is likely to contain constant values use the inference rules.

To better understand, read our paper [3]:
> **Augmenting Search-based Program Synthesis with Local Inference Rules to Improve Black-box Deobfuscation**, Vidal Attias, Nicolas Bellec, Grégoire Menguy, Sébastien Bardin, Jean-Yves Marion, ACM Conference on Computer and Communications Security 2025


### Usage 

To use inference rules you must set the option 
`-infrules <val>` where `<val>` can be:
1. A list of rules (`r1, r2, ..., rk`) where each rules aim to handle a specific case as described below

| Inference Rule                             | Expression kind                 |
|--------------------------------------------|---------------------------------|
| $\diamond \in \{ +, *, \oplus, >>u, <<, ror \}$ | $e \diamond c$                  |
| maskotf                                    | $(e \land c_1) \lor c_2$        |
| affine                                     | $(c_1 * e) + c_2$               |
| poly2                                      | $(c_1 * e^2) + (c_2 * e) + c_3$ |

> where `e` is a expression from the grammar and `c, c1, c2, c3` are arbitrary constant values.



2. A keyword among `mba` (same as: `+, maskotf, <<, *, ^`) and `all` (same as: `+, maskotf, <<, >>u, *, ^, ror, poly2, affine`).


### You want to try it ? 

We provide an example of obfuscated expression from the snapchat app. To apply Xyntia over it, run:
```
$ xyntia -bin samplers/examples/snapchat -config samplers/examples/snapchat.ini -infrules all 
```

</details>

## CEGIS

<details open><summary>Show</summary>

Xyntia includes a *Counter-example guided inductive synthesis* loop. The idea to check if the synthesis result is correct and provide a counter-example if it is not. This counter-example is added to the set of input-output examples and synthesis is relaunched. 
CEGIS loops until an equivalent solution have been found or the time budget elapsed. 

To use CEGIS, you can use the `-cegis` option.
```
$ xyntia -bin examples/bin/add -config examples/bin/add.ini -cegis
```

**Remarks:**
* You can set a timeout of each synthesis step through the usual `-time` option;
* To set the timeout of the overall cegis loop, use `timeout <val> xyntia -cegis ...`;
* If `-cegis` is set the `-check` option is redundante (can be set but will change nothing).

</details>

## Generating SyGUS problems

<details open><summary>Show</summary>

To easily compare other synthesizers with Xyntia or apply them to deobfuscation tasks, we provide a way to extract the 
the synthesis problem in the standard SyGUS format. To do so, just use the `-sygus` option.

For instance, to extract the sygus problem from a binary code, run:
```
$ xyntia -bin <binary-file> -config <config-file> -sygus
```

</details>

# Experiments

<details open><summary>Show</summary>


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


</details>

# Synthesize all blocks from a execution trace

<details open><summary>Show</summary>


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

</details>

# References

[1] Menguy, G., Bardin, S., Bonichon, R., & Lima, C. D. S. (2021, November). Search-Based Local Black-Box Deobfuscation: Understand, Improve and Mitigate. In Proceedings of the 2021 ACM SIGSAC Conference on Computer and Communications Security.

[2] Blazytko, T., Contag, M., Aschermann, C., & Holz, T. (2017). Syntia: Synthesizing the semantics of obfuscated code. In 26th USENIX Security Symposium (USENIX Security 17).

[3] Attias, V., Bellec, N., Menguy, G., Bardin, S., Marion, J. (2025, October). Augmenting Search-based Program Synthesis with Local Inference Rules to Improve Black-box Deobfuscation. In Proceedings of the 2025 ACM SIGSAC Conference on Computer and Communications Security.
