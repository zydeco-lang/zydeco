
use builtin;
use str;

set edit:completion:arg-completer[zydeco] = {|@words|
    fn spaces {|n|
        builtin:repeat $n ' ' | str:join ''
    }
    fn cand {|text desc|
        edit:complex-candidate $text &display=$text' '(spaces (- 14 (wcswidth $text)))$desc
    }
    var command = 'zydeco'
    for word $words[1..-1] {
        if (str:has-prefix $word '-') {
            break
        }
        set command = $command';'$word
    }
    var completions = [
        &'zydeco'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
            cand -V 'Print version'
            cand --version 'Print version'
            cand __doc-example-worker '__doc-example-worker'
            cand doc 'Read, search, generate, or verify project documentation'
            cand fmt 'Format Zydeco source files in place'
            cand run 'Run a zydeco program'
            cand check 'Check a zydeco program'
            cand repl 'Start the declaration-free terminal REPL'
            cand build 'build'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;__doc-example-worker'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;doc'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
            cand show 'Show a public subject''s classifier and complete documentation'
            cand search 'Search exposed names and documentation prose'
            cand build 'Generate a self-contained searchable HTML reference without executing examples'
            cand check 'Check links and explicitly verified examples in the entry and its dependencies'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;doc;show'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;doc;search'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;doc;build'= {
            cand -o 'o'
            cand --output 'output'
            cand --title 'title'
            cand --guide 'Explicit guide pages; public links use `zydeco:member:./field`'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;doc;check'= {
            cand --guide 'guide'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;doc;help'= {
            cand show 'Show a public subject''s classifier and complete documentation'
            cand search 'Search exposed names and documentation prose'
            cand build 'Generate a self-contained searchable HTML reference without executing examples'
            cand check 'Check links and explicitly verified examples in the entry and its dependencies'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;doc;help;show'= {
        }
        &'zydeco;doc;help;search'= {
        }
        &'zydeco;doc;help;build'= {
        }
        &'zydeco;doc;help;check'= {
        }
        &'zydeco;doc;help;help'= {
        }
        &'zydeco;fmt'= {
            cand --check 'Report files that would change without writing them, and exit unsuccessfully when at least one file would change'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;run'= {
            cand --dry 'Dry run (don''t execute)'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;check'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;repl'= {
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;build'= {
            cand --target-os 'Target OS (defaults to host OS)'
            cand --target-arch 'Target architecture (defaults to host architecture)'
            cand -t 'Target backend'
            cand --target 'Target backend'
            cand -b 'Build Directory'
            cand --build-dir 'Build Directory'
            cand -r 'Runtime directory'
            cand --runtime-dir 'Runtime directory'
            cand -x 'Run the program after building'
            cand --execute 'Run the program after building'
            cand --lint-types 'Re-validate the finished typed arena after every successful check, reporting internal compiler errors (debugging aid)'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;help'= {
            cand __doc-example-worker '__doc-example-worker'
            cand doc 'Read, search, generate, or verify project documentation'
            cand fmt 'Format Zydeco source files in place'
            cand run 'Run a zydeco program'
            cand check 'Check a zydeco program'
            cand repl 'Start the declaration-free terminal REPL'
            cand build 'build'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;help;__doc-example-worker'= {
        }
        &'zydeco;help;doc'= {
            cand show 'Show a public subject''s classifier and complete documentation'
            cand search 'Search exposed names and documentation prose'
            cand build 'Generate a self-contained searchable HTML reference without executing examples'
            cand check 'Check links and explicitly verified examples in the entry and its dependencies'
        }
        &'zydeco;help;doc;show'= {
        }
        &'zydeco;help;doc;search'= {
        }
        &'zydeco;help;doc;build'= {
        }
        &'zydeco;help;doc;check'= {
        }
        &'zydeco;help;fmt'= {
        }
        &'zydeco;help;run'= {
        }
        &'zydeco;help;check'= {
        }
        &'zydeco;help;repl'= {
        }
        &'zydeco;help;build'= {
        }
        &'zydeco;help;help'= {
        }
    ]
    $completions[$command]
}
