# Using Xyntia as a library

:warning: do not use the library for scientific evaluation (use the Xyntia executable)

Xyntia can be used as a library to include program synthesis in your Ocaml project.
The following is a quick tutorial to explain how to create your first project using the Xyntia library.

## Create the project

The first step is of course to create your own project. 
```bash
$ dune init project <project-name>
$ cd <project-name>
$ opam switch create .  4.14.1 -y # or any version >= 4.14.1 but < 5.0
```

## Add Xyntia library to your project 

In another folder, called `<xyntia-folder>`, clone the Xyntia repository.
Then, in your project folder run:
```bash
$ cd <path-to-your-project>
$ eval $(opam env)
$ opam pin <xyntia-folder> -y
```

## Use Xyntia library

You can then add Xyntia to your dependencies, by adding Xyntia in the dune libraries field as follows:
```bash
$ cd <project-name>
$ cat bin/dune 
(executable
 (public_name <project-name>)
 (name main)
 (libraries <project-name> xyntia))
```


The documentation of the Xyntia Ocaml API is available by runnig:
```bash
$ dune ocaml doc
```

We provide multiple examples to use Xyntia:
* [Run Xyntia over input-output examples](./synthesize_from_samples.ml): Run Xyntia over a set input output examples you defined;
* [Run Xyntia over json specification](./synthesize_from_json.ml): Run Xyntia over a json file specifying the input output examples;
* [Run Xyntia over a binary code](./synthesize_from_binary.ml): Run Xyntia over binary code snippet using Binsec;
* [Run Xyntia over symbolic expressions](./synthesize_symbolic_expr.ml): Define a symbolic expression, sample it and sythesize it with Xyntia;
* [Run Xyntia with inference rules](./synthesize_with_inference_rules.ml): Use inference rules to synthesize complex expressions with constant values;

Each example can be copied to `<project-name>/bin/main.ml`.

You can then compile your project and run it as follows:
```bash
$ dune build 
$ _build/default/bin/main.exe
```
