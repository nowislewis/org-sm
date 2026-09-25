// Browser interaction regressions without a DOM or network dependency.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

const html = fs.readFileSync(require('node:path').join(__dirname, '../web/index.html'), 'utf8');
const script = html.match(/<script>([\s\S]*?)<\/script>/)[1].replace(/\ninit\(\);\s*$/, '');
const elements = new Map();
const alerts = [];
const context = vm.createContext({
  URLSearchParams,
  location: { search: '' },
  sessionStorage: { getItem: () => null },
  window: { addEventListener: () => {} },
  document: {
    getElementById(id) {
      if (!elements.has(id)) elements.set(id, {
        value: '', selectionStart: 0, selectionEnd: 0,
        classList: { add() {}, remove() {}, contains: () => false },
        focus() {}, querySelectorAll: () => []
      });
      return elements.get(id);
    }
  },
  alert: message => alerts.push(message),
  setTimeout,
  clearTimeout
});
vm.runInContext(script + '\nrender = () => {}; toast = () => {};', context);
const evaluate = code => vm.runInContext(code, context);
const element = id => context.document.getElementById(id);

(async () => {
  evaluate('State.queue = [{id:"card", title:"Title", body:"😀alpha beta", revision:"v1"}]');
  element('edit-title').value = 'Title';
  element('edit-body').value = '😀alpha beta!';
  element('edit-body').selectionStart = 2;
  element('edit-body').selectionEnd = 7;
  const calls = [];
  evaluate('API.call = async (path, params) => { globalThis.calls.push({path, params}); throw Error("offline"); }');
  context.calls = calls;
  await evaluate('toolExtract("topic")');
  assert.equal(calls.length, 1, 'a failed patch must not create a child');
  assert.match(calls[0].path, /\/api\/patch\//);
  assert.equal(alerts.length, 1);

  calls.length = 0;
  evaluate(`API.call = async (path, params) => {
    globalThis.calls.push({path, params});
    return path.includes('/patch/')
      ? {body: '😀alpha beta!', title: 'Title', revision: 'v2'}
      : {parent: {body: '😀alpha[[id:child][<T>]] beta!', title: 'Title', revision: 'v3'}};
  }`);
  await evaluate('toolExtract("topic")');
  assert.equal(calls.length, 2);
  assert.equal(calls[1].params.start, '1', 'emoji is one Emacs character');
  assert.equal(calls[1].params.end, '6');
  assert.equal(calls[1].params.revision, 'v2', 'extract uses the saved revision');
  assert.equal(evaluate('State.current.revision'), 'v3');

  calls.length = 0;
  evaluate(`State.busy = false;
    API.call = (path, params) => { globalThis.calls.push({path, params});
      return new Promise(resolve => { globalThis.resolveReview = resolve; }); };`);
  const first = evaluate('reviewTopic("reschedule")');
  const second = evaluate('reviewTopic("reschedule")');
  assert.equal(calls.length, 1, 'double tap must not duplicate a grade');
  assert.equal(calls[0].params.revision, 'v3');
  context.resolveReview({ 'interval-days': 3 });
  await Promise.all([first, second]);
  console.log('web review interaction tests passed');
})().catch(error => { console.error(error); process.exitCode = 1; });
