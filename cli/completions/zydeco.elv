
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
            cand -h 'Print help'
            cand --help 'Print help'
            cand -V 'Print version'
            cand --version 'Print version'
            cand run 'Run a zydeco program'
            cand check 'Check a zydeco program'
            cand build 'build'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;run'= {
            cand --bin 'Name of the binary'
            cand --dry 'Dry run (don''t execute)'
            cand -v 'Level of verbosity'
            cand --verbose 'Level of verbosity'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;check'= {
            cand -v 'Level of verbosity'
            cand --verbose 'Level of verbosity'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;build'= {
            cand --bin 'Name of the binary'
            cand -t 'Target architecture'
            cand --target 'Target architecture'
            cand --dry 'Dry run (don''t execute)'
            cand -v 'Level of verbosity'
            cand --verbose 'Level of verbosity'
            cand -h 'Print help'
            cand --help 'Print help'
        }
        &'zydeco;help'= {
            cand run 'Run a zydeco program'
            cand check 'Check a zydeco program'
            cand build 'build'
            cand help 'Print this message or the help of the given subcommand(s)'
        }
        &'zydeco;help;run'= {
        }
        &'zydeco;help;check'= {
        }
        &'zydeco;help;build'= {
        }
        &'zydeco;help;help'= {
        }
    ]
    $completions[$command]
}
