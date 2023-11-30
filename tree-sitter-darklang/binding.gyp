{
    "targets": [{
        "target_name": "tree_sitter_darklang_binding",
        "include_dirs": ["<!(node -e \"require('nan')\")", "src"],
        "sources": [
            "src/parser.c",
            # If your language uses an external scanner, add it here.
        ],
        "cflags_c": [
            "-std=c99",
        ]
    }]
}
