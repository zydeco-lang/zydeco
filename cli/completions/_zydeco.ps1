
using namespace System.Management.Automation
using namespace System.Management.Automation.Language

Register-ArgumentCompleter -Native -CommandName 'zydeco' -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)

    $commandElements = $commandAst.CommandElements
    $command = @(
        'zydeco'
        for ($i = 1; $i -lt $commandElements.Count; $i++) {
            $element = $commandElements[$i]
            if ($element -isnot [StringConstantExpressionAst] -or
                $element.StringConstantType -ne [StringConstantType]::BareWord -or
                $element.Value.StartsWith('-') -or
                $element.Value -eq $wordToComplete) {
                break
        }
        $element.Value
    }) -join ';'

    $completions = @(switch ($command) {
        'zydeco' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('-V', '-V ', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('--version', '--version', [CompletionResultType]::ParameterName, 'Print version')
            [CompletionResult]::new('fmt', 'fmt', [CompletionResultType]::ParameterValue, 'Format Zydeco source files in place')
            [CompletionResult]::new('run', 'run', [CompletionResultType]::ParameterValue, 'Run a zydeco program')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check a zydeco program')
            [CompletionResult]::new('repl', 'repl', [CompletionResultType]::ParameterValue, 'Start the declaration-free terminal REPL')
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'build')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'zydeco;fmt' {
            [CompletionResult]::new('--check', '--check', [CompletionResultType]::ParameterName, 'Report files that would change without writing them, and exit unsuccessfully when at least one file would change')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;run' {
            [CompletionResult]::new('--dry', '--dry', [CompletionResultType]::ParameterName, 'Dry run (don''t execute)')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;check' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;repl' {
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;build' {
            [CompletionResult]::new('--target-os', '--target-os', [CompletionResultType]::ParameterName, 'Target OS (defaults to host OS)')
            [CompletionResult]::new('--target-arch', '--target-arch', [CompletionResultType]::ParameterName, 'Target architecture (defaults to host architecture)')
            [CompletionResult]::new('-t', '-t', [CompletionResultType]::ParameterName, 'Target backend')
            [CompletionResult]::new('--target', '--target', [CompletionResultType]::ParameterName, 'Target backend')
            [CompletionResult]::new('-b', '-b', [CompletionResultType]::ParameterName, 'Build Directory')
            [CompletionResult]::new('--build-dir', '--build-dir', [CompletionResultType]::ParameterName, 'Build Directory')
            [CompletionResult]::new('-r', '-r', [CompletionResultType]::ParameterName, 'Runtime directory')
            [CompletionResult]::new('--runtime-dir', '--runtime-dir', [CompletionResultType]::ParameterName, 'Runtime directory')
            [CompletionResult]::new('-x', '-x', [CompletionResultType]::ParameterName, 'Run the program after building')
            [CompletionResult]::new('--execute', '--execute', [CompletionResultType]::ParameterName, 'Run the program after building')
            [CompletionResult]::new('-h', '-h', [CompletionResultType]::ParameterName, 'Print help')
            [CompletionResult]::new('--help', '--help', [CompletionResultType]::ParameterName, 'Print help')
            break
        }
        'zydeco;help' {
            [CompletionResult]::new('fmt', 'fmt', [CompletionResultType]::ParameterValue, 'Format Zydeco source files in place')
            [CompletionResult]::new('run', 'run', [CompletionResultType]::ParameterValue, 'Run a zydeco program')
            [CompletionResult]::new('check', 'check', [CompletionResultType]::ParameterValue, 'Check a zydeco program')
            [CompletionResult]::new('repl', 'repl', [CompletionResultType]::ParameterValue, 'Start the declaration-free terminal REPL')
            [CompletionResult]::new('build', 'build', [CompletionResultType]::ParameterValue, 'build')
            [CompletionResult]::new('help', 'help', [CompletionResultType]::ParameterValue, 'Print this message or the help of the given subcommand(s)')
            break
        }
        'zydeco;help;fmt' {
            break
        }
        'zydeco;help;run' {
            break
        }
        'zydeco;help;check' {
            break
        }
        'zydeco;help;repl' {
            break
        }
        'zydeco;help;build' {
            break
        }
        'zydeco;help;help' {
            break
        }
    })

    $completions.Where{ $_.CompletionText -like "$wordToComplete*" } |
        Sort-Object -Property ListItemText
}
