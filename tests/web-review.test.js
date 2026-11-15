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
        innerHTML: '', textContent: '', scrollTop: 0, scrollHeight: 0,
        style: {}, disabled: false,
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

  // AI Rewrite converts UTF-16 selection offsets to code-point offsets and
  // refuses to overwrite manual edits made while the request is in flight.
  calls.length = 0;
  evaluate(`State.queue = [{id:"c-ai", type:"topic", title:"T", body:"前文。关键因果句子。后文。", revision:"v1"}];
    State.index = 0;`);
  element('edit-body').value = '😀第一段。需要改写的第二段。';
  element('edit-body').selectionStart = 6; // after '😀' (2 UTF-16 units) + '第一段。' (4)
  element('edit-body').selectionEnd = 15;
  evaluate(`API.call = (path, params) => {
    globalThis.calls.push({path, params});
    return new Promise(resolve => { globalThis.resolveRewrite = resolve; });
  };`);
  const rw1 = evaluate('toolAiRewrite("refine")');
  assert.equal(calls.length, 1);
  assert.equal(calls[0].params.start, '5', 'emoji counts as 1 code point');
  assert.equal(calls[0].params.end, '14');
  // User edits textarea before AI finishes -> AI result must be ignored.
  element('edit-body').value = '😀用户手动改了草稿';
  context.resolveRewrite({ ok: true, body: 'AI覆盖结果', picks: [] });
  await rw1;
  assert.equal(element('edit-body').value, '😀用户手动改了草稿');

  // Workbench commit: AI refine populates picks in Edit Sheet, and saveEditSheet
  // patches the body then extracts surviving picks from end to start.
  calls.length = 0;
  evaluate(`State.queue = [{id:"c-wb", type:"topic", title:"T", body:"旧正文", revision:"r1"}]; State.index = 0;`);
  element('edit-title').value = 'T';
  element('edit-body').value = '旧正文';
  element('edit-body').selectionStart = 0;
  element('edit-body').selectionEnd = 0;
  evaluate(`API.call = async (path, params) => {
    globalThis.calls.push({path, params});
    if (path.includes('/ai/rewrite/')) {
      return {
        ok: true,
        body: "第一句。核心句A。核心句B。",
        picks: [
          {start: 4, end: 9, quote: "核心句A。", why: "原因A"},
          {start: 9, end: 14, quote: "核心句B。", why: "原因B"}
        ]
      };
    }
    if (path.includes('/patch/')) {
      return {ok: true, title: "T", body: params.body, revision: "r2"};
    }
    if (path.includes('/extract/')) {
      return {ok: true, parent: {title: "T", body: "extracted", revision: params.revision + "x"}};
    }
  };`);
  await evaluate('toolAiRewrite("refine")');
  assert.equal(evaluate('State.pendingEditPicks.length'), 2);
  await evaluate('saveEditSheet()');
  // 1 rewrite + 1 patch + 2 extracts (end-to-start: B at [9,14] first, then A at [4,9])
  assert.equal(calls.length, 4);
  assert.match(calls[1].path, /\/api\/patch\/c-wb/);
  assert.equal(calls[2].params.start, '9', 'extracts from end to start');
  assert.equal(calls[3].params.start, '4');

  // Explain sheet initializes and sends follow-up reply to the Emacs chat buffer.
  calls.length = 0;
  evaluate(`State.queue = [{id:"c-ai", type:"topic", title:"T", body:"前文。关键因果句子。后文。", revision:"v1"}];
    API.call = async (path, params) => {
    globalThis.calls.push({path, params});
    if (params.reply) return {ok: true, reply: "回答正确"};
    return {ok: true, passage: "关键因果句子。", reply: "① 定义..."};
  };`);
  await evaluate('openExplainSheet({start: 3, end: 10, text: "关键因果句子。"})');
  assert.equal(calls.length, 1);
  assert.equal(calls[0].params.start, '3');
  element('explain-input').value = '去掉前提就不成立';
  await evaluate('sendExplainFollowup()');
  assert.equal(calls.length, 2);
  assert.equal(calls[1].params.reply, '去掉前提就不成立');
  assert.equal(evaluate('State.chatsByCard["c-ai"].turns.length'), 3);

  console.log('web review interaction tests passed');
})().catch(error => { console.error(error); process.exitCode = 1; });

// A light session is the head of the server's order, which the client must
// not re-shuffle: Emacs and the phone have to agree on what comes next.
{
  const cards = Array.from({ length: 9 }, (_, i) => ({ id: "x" + i, type: "cloze" }));
  evaluate(`State.all = ${JSON.stringify(cards)}; State.light = true; State.queue = buildQueue();`);
  const light = evaluate("State.queue.map(c => c.id)");
  assert.equal(light.length, 6);
  assert.equal(JSON.stringify(light), JSON.stringify(["x0", "x1", "x2", "x3", "x4", "x5"]));
  evaluate("State.light = false; State.queue = buildQueue();");
  assert.equal(evaluate("State.queue.length"), 9);
}

// Read points, images and links tokenise as atomic spans with exact offsets.
{
  const body = "前[[rp:X][📖]]后 [[file:a.png][图]] [[page:/b.pdf::3][《b》p.3]] {{c}}";
  const toks = evaluate(`tokenizeBody(${JSON.stringify(body)}, false)`);
  const kinds = toks.map(t => t.kind);
  assert.equal(JSON.stringify(kinds), JSON.stringify(["text", "rp", "text", "link", "text", "link", "text"]));
  const rp = toks[1];
  assert.equal(rp.start, 1);
  assert.equal(rp.end, 1 + Array.from("[[rp:X][📖]]").length);
  assert.equal(toks[3].target, "file:a.png");
  assert.equal(toks[5].desc, "《b》p.3");
  // {{c}} is plain text on a topic, a cloze on a cloze card.
  assert.equal(toks[6].text.includes("{{c}}"), true);
  const clozeToks = evaluate(`tokenizeBody("x {{c}} {{答案::提示}}", true)`);
  assert.equal(clozeToks[1].kind, "cloze");
  assert.equal(clozeToks[1].cloze, "c");
  assert.equal(clozeToks[1].hint, null);
  assert.equal(clozeToks[3].kind, "cloze");
  assert.equal(clozeToks[3].cloze, "答案");
  assert.equal(clozeToks[3].hint, "提示");
  // Offsets end exactly at the code-point length of the body.
  assert.equal(toks[toks.length - 1].end, Array.from(body).length);
}
console.log("reading tests passed");

// Link descriptions may contain single brackets, as org-link-make-string
// leaves them: Chinese book titles such as 《[美] 作者》 must still parse.
{
  const body = "来源：[[page:/b.pdf::3][《[美] 作者》p.3]] 后文";
  const toks = evaluate(`tokenizeBody(${JSON.stringify(body)}, false)`);
  const link = toks.find(t => t.kind === "link");
  assert.ok(link, "link token missing");
  assert.equal(link.target, "page:/b.pdf::3");
  assert.equal(link.desc, "《[美] 作者》p.3");
  assert.equal(toks[toks.length - 1].end, Array.from(body).length);
}
console.log("bracket tests passed");
