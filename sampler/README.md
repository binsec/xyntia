
## Plugin Binsec to sample code blocks

### Build

Dependencies:
* ocaml
* dune
* Binsec (0.10.0) with its dependencies

You can compile and install the plugin as follows:
```bash
dune build
dune install
```

### Usage

```bash
binsec [ -isa ISA ] -sse -sse-engine sampling -sse-script SCRIPT BINARY -sse-quiet -termsampler
```

where:
* ISA is the target binary instruction set (e.g., x86 -- required if working on raw blocks)
* SCRIPT is the binsec script specifying what to sample (see below)
* BINARY is the binary code to sample (can be a full executable or raw binary)

##### Scripts

The plugin defines new declarations and instructions in DBA:
* `sample N [ reg_1, ..., reg_n ]`, which specify to generate N samples for each output. A list of target register outputs can be set, in such a case, only these are sampled otherwise all detected outputs are.
* `set sample output TARGET`, which specify where should be stored the sampling results. TARGET can equal `stdout` or `"path/to/directory"`.
* `set domain VAR [MIN, MAX]` which specify the sampling domain for the VAR  input. VAR can be a register or any DBA variable but not a memory cell. To specify the domain of a memory cell, see `examples/snapshat.ini`
* `prune constant outputs` which prunes all the constant outputs (i.e., with no input variables) from the set of sampled outputs

Examples of scripts can be found in the `examples` directory.

It is also possible to sample a symbolic expression directly. An example is given in `examples/expr.ini`. To sample it, use:
```
# The <() is here to replace the binary path. Indeed, in this case we do not need any binary (only an empty file)
binsec -isa x86 -sse -sse-engine sampling -sse-script examples/expr.ini <() -sse-quiet -termsampler
```

