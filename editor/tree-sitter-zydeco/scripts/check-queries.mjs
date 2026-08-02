import { readdirSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';

const grammarRoot = fileURLToPath(new URL('..', import.meta.url));
const repositoryRoot = resolve(grammarRoot, '../..');
const queryRoot = resolve(repositoryRoot, 'editor/zed/languages/zydeco');
const fixture = resolve(repositoryRoot, 'lib/examples/algebra.zydeco');
const configPath = resolve(grammarRoot, 'tree-sitter-config.json');
const executable = resolve(
  grammarRoot,
  'node_modules',
  '.bin',
  process.platform === 'win32' ? 'tree-sitter.cmd' : 'tree-sitter',
);

const queries = readdirSync(queryRoot)
  .filter(name => name.endsWith('.scm'))
  .map(name => resolve(queryRoot, name))
  .sort();

for (const query of queries) {
  const result = spawnSync(
    executable,
    ['query', '--config-path', configPath, '--quiet', query, fixture],
    {
      cwd: grammarRoot,
      stdio: 'inherit',
    },
  );

  if (result.error) {
    throw result.error;
  }
  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }
}

console.log(`Compiled ${queries.length} Zed query files against the Zydeco grammar.`);
