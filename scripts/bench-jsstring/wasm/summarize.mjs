// Turns run.mjs output into a Markdown report (stdout), and, with --annotate,
// GitHub annotations: one for the property check and the benchmark table in
// a few parts, on stdout. Usage: node summarize.mjs results.txt report.md [--annotate]
import { readFileSync, writeFileSync } from 'node:fs';

const [file, out, flag] = process.argv.slice(2);
const label = process.env.BENCH_LABEL ? process.env.BENCH_LABEL + ' ' : '';
const lines = readFileSync(file, 'utf8').split('\n');
const props = lines.filter(l => l.startsWith('PROP|')).map(l => l.split('|'));
const bench = lines.filter(l => l.startsWith('BENCH|')).map(l => l.split('|'));

const fmt = us => {
  const x = Number(us);
  if (x >= 1e6) return (x / 1e6).toPrecision(3) + ' s';
  if (x >= 1e3) return (x / 1e3).toPrecision(3) + ' ms';
  if (x >= 1) return x.toPrecision(3) + ' µs';
  return (x * 1e3).toPrecision(3) + ' ns';
};
const ratio = (o, n) => {
  const r = Number(o) / Number(n);
  if (r >= 100) return Math.round(r).toLocaleString('en-US') + '×';
  if (r >= 1.05) return r.toFixed(1) + '×';
  if (r > 0.95) return '1×';
  return '**' + r.toFixed(2) + '×**';
};

const failed = props.filter(([, , ok, total]) => ok !== total);
const propReport = [
  `Property check: ${props.length - failed.length} of ${props.length} agree with Data.Text on every case.`,
  ...failed.map(([, name, ok, total, example]) => `- ${name}: ${ok}/${total} — e.g. ${example}`),
].join('\n');

const header = '| Function | Input | Old | New | Speedup |\n|---|---|---:|---:|---:|';
const rows = bench.map(([, name, input, o, n]) => `| \`${name}\` | ${input} | ${fmt(o)} | ${fmt(n)} | ${ratio(o, n)} |`);
const done = lines.includes('DONE');

writeFileSync(out, `## Data.JSString: property check${label ? ' (' + label.trim() + ')' : ''}\n\n` + propReport + '\n\n'
  + `## Data.JSString: benchmark ${label ? '(' + label.trim() + ')' : '(WASM, Node)'}\n\n` + header + '\n' + rows.join('\n') + '\n'
  + (done ? '' : '\n**The run did not finish.**\n'));

if (flag === '--annotate') {
  const esc = s => s.replace(/%/g, '%25').replace(/\r/g, '%0D').replace(/\n/g, '%0A');
  console.log(`::notice title=${label}props ${props.length - failed.length}/${props.length}${done ? '' : ' (unfinished)'}::${esc(propReport)}`);
  const per = 40;
  for (let i = 0; i < rows.length; i += per) {
    const part = i / per + 1, parts = Math.ceil(rows.length / per);
    console.log(`::notice title=${label}bench ${part}/${parts}::${esc(header + '\n' + rows.slice(i, i + per).join('\n'))}`);
  }
}
