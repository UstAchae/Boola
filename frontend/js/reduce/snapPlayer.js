import { sleep } from "../graph/cy.js";
import { setDimAllExcept, clearDim } from "./common.js";

function applyFocus(cy, focusIds) {
  if (!focusIds?.length) {
    clearDim(cy);
    return { nodes: cy.collection(), edges: cy.collection() };
  }

  const focusSet = new Set(focusIds);
  const nodes = cy.nodes().filter((n) => focusSet.has(n.id()));
  const edges = nodes.union(nodes.incomers("edge")).union(nodes.outgoers("edge"));

  setDimAllExcept(cy, nodes.union(edges));
  nodes.addClass("focus");
  return { nodes, edges };
}

function clearFocus(cy, focusNodes) {
  focusNodes?.removeClass("focus");
  clearDim(cy);
}

export async function playSnapshotTrace(
  cy,
  trace,
  { setGraph, onAfterEach, highlightMs = 420, betweenMs = 150 } = {}
) {
  const initial = trace?.initial;
  const steps = trace?.steps || [];
  if (!initial || !steps.length) return;

  await setGraph(initial);

  for (const s of steps) {
    const { nodes } = applyFocus(cy, s.focus || []);
    await sleep(highlightMs);

    // snapshot swap (backend computed)
    await setGraph(s.snapshot);

    clearFocus(cy, nodes);
    onAfterEach?.(s);
    await sleep(betweenMs);
  }
}