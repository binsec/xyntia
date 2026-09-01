
We detail here the process to run experiments with Xyntia.

**Table of content:**
1.  [Experiments](#experiments)
2.  [Synthesize all blocks from a execution trace](#synthesize-all-blocks-from-a-execution-trace)


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

## Performance

Xyntia capabilities depends on the mutation speed. Hence, we recommend:
* to configure your machine in *performance mode*
* to use the Xyntia `-quiet` option (by default Xyntia spawn a second thread for user feedback but it  may impact performance in experiments)

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