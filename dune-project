(lang dune 3.0)
(generate_opam_files true)

(using dune_site 0.1)
(name xyntia)
(version "0.2.2")
(authors
    "Grégoire Menguy"
    "Cauim de Souza Lima"
    "Nicolas Bellec"
)
(license LGPL-2.1-or-later)

(package 
    (name xyntia)
    (synopsis "Xyntia: Search-based Local Black-box Deobfuscator")
    (description "Xyntia is a black-box deobfuscator, relying on S-metaheuristics to synthesize the semantics of highly obfuscated code blocks")
    (depends
        (ocaml (>= 4.07))
        (zarith (>= 1.10))
        (yojson (>= 1.7.0))
        (qcheck (>= 0.15))
        (odoc (>= 3.2.1))
        (unisim_archisec (>= 0.0.10))
        (fileutils (>= 0.6.6))
        (progress (>= 0.5.0))
        (bitwuzla (>= 1.0.5))
        (binsec (= 0.11.3))
        (ppx_deriving (>= 6.1.0))
        (xyntiasampler (= :version))
    )
    (tags (
        "binary code analysis"
        "deobfuscation"
        "program synthesis"
    ))
)

(package
  (name xyntiasampler)
    (synopsis "Sampler based on Binsec for black-box deobfuscation")
    (description "Detects inputs and outputs of a binary code snippet and sample them")
  (depends (binsec (= 0.11.3)) yojson (progress (>= 0.5.0)))
)
