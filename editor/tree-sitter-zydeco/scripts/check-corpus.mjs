import { readdirSync } from 'node:fs';
import { extname, relative, resolve, sep } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';

const grammarRoot = fileURLToPath(new URL('..', import.meta.url));
const repositoryRoot = resolve(grammarRoot, '../..');
const sourceRoot = resolve(repositoryRoot, 'lib');
const configPath = resolve(grammarRoot, 'tree-sitter-config.json');
const sourceExtensions = new Set(['.zy', '.zydeco']);
const legacySourcePaths = [
  'tests/compile-more/',
  'tests/exec/iota.zy',
  'tests/oopsla/algebra.zydeco',
  'tests/oopsla/core.zydeco',
  'tests/oopsla/data.zydeco',
  'tests/oopsla/exnkt.zydeco',
  'tests/oopsla/exnt.zydeco',
];

const repositoryPath = sourcePath => relative(sourceRoot, sourcePath).split(sep).join('/');
const isLegacySource = sourcePath => {
  const source = repositoryPath(sourcePath);
  return legacySourcePaths.some(legacy =>
    legacy.endsWith('/') ? source.startsWith(legacy) : source === legacy,
  );
};

const sourceFiles = readdirSync(sourceRoot, { recursive: true, withFileTypes: true })
  .filter(entry => entry.isFile() && sourceExtensions.has(extname(entry.name)))
  .map(entry => resolve(entry.parentPath, entry.name))
  .filter(sourcePath => !isLegacySource(sourcePath))
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

console.log(`Parsed ${sourceFiles.length} current Zydeco source files without syntax errors.`);
