import { LAYER_LAYOUT, TERM_ANIM } from "../config.js";

export function clearFocus(cy) {
  cy.nodes().removeClass("focus");
}

export function applyFocus(cy, ids) {
  clearFocus(cy);
  if (!Array.isArray(ids)) return;
  for (const id of ids) {
    const n = cy.getElementById(id);
    if (n && n.length) n.addClass("focus");
  }
}

export function isTerminalNode(n) {
  const lab = (n.data("label") ?? "").toString();
  return n.hasClass("terminal") || lab === "0" || lab === "1";
}

export function isNonTerminal(n) {
  return n && n.isNode && n.isNode() && !isTerminalNode(n);
}

export function termValue(n) {
  const lab = (n.data("label") ?? "").toString();
  if (lab === "0" || lab === "1") return lab;
  const v = n.data("value");
  if (v === 0 || v === 1 || v === "0" || v === "1") return String(v);
  return lab;
}

export function setDimAllExcept(cy, keepEles) {
  cy.batch(() => {
    cy.elements().removeClass("dim");
    if (!keepEles || !keepEles.length) return;
    const keep = keepEles.union(keepEles.connectedEdges()).union(keepEles.connectedNodes());
    cy.elements().difference(keep).addClass("dim");
  });
}

export function clearDim(cy) {
  cy.elements().removeClass("dim");
}

export function lastNonTerminalY(cy) {
  const ys = cy.nodes().filter((n) => !isTerminalNode(n)).map((n) => n.position("y"));
  if (!ys.length) return 0;
  return Math.max(...ys);
}

export function graphCenterX(cy) {
  const ext = cy.extent();
  return (ext.x1 + ext.x2) / 2;
}

export function extentWidthWithMin(cy, minSpan = 240) {
  const ext = cy.extent();
  return Math.max(ext.x2 - ext.x1, minSpan);
}

export function applyOps(cy, ops) {
  if (!ops?.length) return;

  console.log("[applyOps]", ops.map(o => ({ kind: o.kind, ids: o.ids?.length, to: o.to })));

  cy.batch(() => {
    for (const op of ops) {
      if (!op) continue;

      if (op.kind === "redirectIncomingEdges") {
        const to = op.to;
        if (!to) continue;

        const dupSet = new Set(op.ids || []);
        const edgesToRewrite = cy.edges().filter(e => dupSet.has(e.data("target")));

        edgesToRewrite.forEach(e => {
          const src = e.data("source");
          const lab = (e.data("label") ?? "").toString();
          const newId = `e_${src}_${lab}_${to}`;

          // 只保留语义 class，别复制 dim/focus
          const cls =
            e.hasClass("zero") ? "zero" :
            e.hasClass("one")  ? "one"  :
            "";

          const existing = cy.getElementById(newId);
          if (existing.length) {
            existing.removeClass("dim");
            existing.removeClass("focus");
            e.remove();
            return;
          }

          cy.add({
            group: "edges",
            data: { id: newId, source: src, target: to, label: lab },
            classes: cls
          });

          e.remove();
        });
      }

      if (op.kind === "removeNodes") {
        const ids = op.ids || [];
        ids.forEach(id => cy.getElementById(id).remove());
      }
    }
  });
}


export async function spreadLayerX(cy, layerNodes, { duration = LAYER_LAYOUT.moveMs } = {}) {
  const ns = (layerNodes || cy.collection()).filter((n) => n.isNode() && !isTerminalNode(n));
  const n = ns.length;
  if (n <= 1) return;

  const ext = cy.extent();
  const width = Math.max(ext.x2 - ext.x1, LAYER_LAYOUT.minSpan);

  const ys = ns.map((x) => x.position("y"));
  const y = ys.reduce((a, b) => a + b, 0) / ys.length;

  const sorted = ns.sort((a, b) => a.position("x") - b.position("x"));
  const left = ext.x1;
  const step = width / (n + 1);

  const anims = [];
  sorted.forEach((node, i) => {
    const x = left + step * (i + 1);
    anims.push(node.animation({ position: { x, y } }, { duration, easing: "ease-in-out" }).play().promise());
  });

  await Promise.all(anims);
}

export async function spreadTerminalsX(cy, termNodes, { duration = 700, y = null } = {}) {
  const ns = (termNodes || cy.collection()).filter((n) => n.isNode() && isTerminalNode(n));
  const n = ns.length;
  if (n <= 1) return;

  const ext = cy.extent();
  const width = extentWidthWithMin(cy, LAYER_LAYOUT.minSpan);
  const left = ext.x1;
  const step = width / (n + 1);

  const yy =
    y != null ? y : ns.map((x) => x.position("y")).reduce((a, b) => a + b, 0) / n;

  const sorted = ns.sort((a, b) => a.position("x") - b.position("x"));

  const anims = [];
  sorted.forEach((node, i) => {
    const x = left + step * (i + 1);
    anims.push(
      node.animation({ position: { x, y: yy } }, { duration, easing: "ease-in-out" }).play().promise()
    );
  });

  await Promise.all(anims);
}

export async function relayoutTerminalLayerEvenly(cy, { duration = 900 } = {}) {
  const terms = cy.nodes().filter(isTerminalNode);
  if (terms.length <= 1) return;

  const baseY = lastNonTerminalY(cy) + TERM_ANIM.gapBelow;

  cy.batch(() => {
    terms.forEach((n) => n.position({ x: n.position("x"), y: baseY }));
  });

  await spreadTerminalsX(cy, terms, { duration, y: baseY });
}
