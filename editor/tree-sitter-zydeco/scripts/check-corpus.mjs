import { readdirSync } from 'node:fs';
import { extname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';

const grammarRoot = fileURLToPath(new URL('..', import.meta.url));
const repositoryRoot = resolve(grammarRoot, '../..');
const sourceRoot = resolve(repositoryRoot, 'lib');
const configPath = resolve(grammarRoot, 'tree-sitter-config.json');
const sourceExtensions = new Set(['.zy', '.zydeco']);
const sourceFiles = readdirSync(sourceRoot, { recursive: true, withFileTypes: true })
  .filter(entry => entry.isFile() && sourceExtensions.has(extname(entry.name)))
  .map(entry => resolve(entry.parentPath, entry.name))
  .sort();

const executable = resolve(
  grammarRoot,
  'node_modules',
  '.bin',
  process.platform === 'win32' ? 'tree-sitter.cmd' : 'tree-sitter',
);
const result = spawnSync(
  executable,
  ['parse', '--config-path', configPath, '--quiet', ...sourceFiles],
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

console.log(`Parsed all ${sourceFiles.length} Zydeco source files without syntax errors.`);
