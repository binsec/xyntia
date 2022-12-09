(lang dune 2.3)
(generate_opam_files true)

(name xyntia)
(version "0.1")
(authors
    "Grégoire Menguy"
    "Cauim de Souza Lima"
)
(license LGPL-2.1-or-later)

(package 
    (name xyntia)
    (synopsis "Xyntia: Search-based Local Black-box Deobfuscator")
    (description "Xyntia is a black-box deobfuscator, relying on S-metaheuristics to synthesize the semantics of highly obfuscated code blocks")
    (depends
        (ocaml (>= 4.07))
        (dune  (>= 1.11.4))
        (zarith (>= 1.10))
        (yojson (>= 1.7.0))
        (qcheck (>= 0.15))
    )
    (tags (
        "binary code analysis"
        "deobfuscation"
        "program synthesis"
    ))
)
