import { getAxisVars } from "./layerAxis.js";

let graphAnimToken = 0;

export function getGraphAnimToken() {
  return graphAnimToken;
}

export function bumpGraphAnimToken() {
  graphAnimToken += 1;
  return graphAnimToken;
}

export function createCy(containerEl) {
  return cytoscape({
    container: containerEl,
    elements: [],
    style: [
      {
        selector: "node",
        style: {
          label: "data(label)",
          "text-valign": "center",
          "text-halign": "center",
          width: 42,
          height: 42,
          "border-width": 2,
          "border-color": "#cfd4df",
          "background-color": "#ffffff",
          "font-size": 12
        }
      },
      { selector: "node.focus", style: { "border-width": 6, "border-color": "#2563eb" } },
      {
        selector: "node.terminal",
        style: {
          shape: "round-rectangle",
          width: 48,
          height: 34,
          "border-color": "#b6bccb",
          "font-weight": 700
        }
      },
      {
        selector: "edge",
        style: {
          "curve-style": "bezier",
          "target-arrow-shape": "triangle",
          "arrow-scale": 0.9,
          width: 2,
          "line-color": "#cfd4df",
          "target-arrow-color": "#cfd4df",
          label: "data(label)",
          "font-size": 10,
          "text-rotation": "autorotate",
          "text-margin-y": -8
        }
      },
      { selector: ".dim", style: { opacity: 0.15 } },
      { selector: "edge.dim", style: { opacity: 0.08 } },
      { selector: "edge.zero", style: { "line-style": "dashed" } },
      { selector: "edge.one", style: { "line-style": "solid" } }
    ],
    layout: { name: "breadthfirst", directed: true, spacingFactor: 1.25 }
  });
}

export function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

export function cancelAllGraphAnims(cy) {
  bumpGraphAnimToken();
  cy.stop();
  cy.elements().stop();
}

export function hasPresetPositions(elements) {
  return !!(elements?.nodes || []).some(
    (n) => n && n.position && Number.isFinite(n.position.x) && Number.isFinite(n.position.y)
  );
}

export function setGraphInstant(cy, elements, { keepViewport = true, onAfterLayout } = {}) {
  bumpGraphAnimToken();
  cy.stop();
  cy.elements().stop();

  const vp = keepViewport ? cy.viewport() : null;

  cy.elements().remove();
  cy.add(elements);
  cy.nodes().ungrabify();

  const preset = hasPresetPositions(elements);

  cy.layout(
    preset
      ? { name: "preset", animate: false, fit: false, padding: 30 }
      : { name: "breadthfirst", directed: true, spacingFactor: 1.25, animate: false, fit: true, padding: 30 }
  ).run();

  cy.elements().style("opacity", 1);

  if (vp) cy.viewport(vp);

  onAfterLayout?.();
}

export function setGraphAnimated(
  cy,
  elements,
  { stepMs = 80, nodeFadeMs = 120, edgeFadeMs = 120 } = {},
  { onAfterLayout } = {}
) {
  const token = bumpGraphAnimToken();

  cy.stop();
  cy.elements().stop();

  cy.elements().remove();
  cy.add(elements);
  cy.nodes().ungrabify();
  cy.elements().style("opacity", 0);

  const preset = hasPresetPositions(elements);
  cy.layout(
    preset
      ? { name: "preset", animate: false, fit: false, padding: 30 }
      : { name: "breadthfirst", directed: true, spacingFactor: 1.35, animate: false, fit: true, padding: 30 }
  ).run();

  onAfterLayout?.();

  setTimeout(async () => {
    if (token !== getGraphAnimToken()) {
      cy.elements().style("opacity", 1);
      return;
    }

    const vp = cy.viewport();
    const nodes = cy.nodes();
    const edges = cy.edges();

    const indeg = new Map();
    nodes.forEach((n) => indeg.set(n.id(), 0));
    edges.forEach((e) => {
      const tgt = e.target().id();
      indeg.set(tgt, (indeg.get(tgt) || 0) + 1);
    });

    const roots = nodes.filter((n) => (indeg.get(n.id()) || 0) === 0);
    const root = roots[0];

//    if (!root) {
//      cy.elements().animate({ style: { opacity: 1 } }, { duration: nodeFadeMs });
//      return;
//    }

    const out = new Map();
    nodes.forEach((n) => out.set(n.id(), []));

    edges.forEach((e) => {
      const src = e.source().id();
      const tgt = e.target().id();
      out.get(src).push({ edge: e, child: cy.getElementById(tgt) });
    });

    function branchOrder(a, b) {
      const la = (a.edge.data("label") ?? "").toString();
      const lb = (b.edge.data("label") ?? "").toString();
      if (la === lb) return 0;
      if (la === "0") return -1;
      if (lb === "0") return 1;
      if (la === "1") return -1;
      if (lb === "1") return 1;
      return la.localeCompare(lb);
    }
    out.forEach((arr) => arr.sort(branchOrder));

    const seenNodes = new Set();
    const seenEdges = new Set();
    const actions = [];

    function dfs(n) {
      const nid = n.id();
      if (!seenNodes.has(nid)) {
        seenNodes.add(nid);
        actions.push({ kind: "node", id: nid });
      }

      const nexts = out.get(nid) || [];
      for (const { edge, child } of nexts) {
        const eid = edge.id();
        if (!seenEdges.has(eid)) {
          seenEdges.add(eid);
          actions.push({ kind: "edge", id: eid });
        }

        const cid = child.id();
        if (!seenNodes.has(cid)) {
          seenNodes.add(cid);
          actions.push({ kind: "node", id: cid });
          dfs(child);
        }
      }
    }

    dfs(root);

    for (const a of actions) {
      if (token !== getGraphAnimToken()) {
        cy.elements().style("opacity", 1);
        return;
      }

      cy.viewport(vp);

      const el = cy.getElementById(a.id);
      if (a.kind === "node") el.animate({ style: { opacity: 1 } }, { duration: nodeFadeMs });
      else el.animate({ style: { opacity: 1 } }, { duration: edgeFadeMs });

      await sleep(stepMs);
    }
  }, 0);
}

export function clearGraph(cy) {
  bumpGraphAnimToken();
  cy.stop();
  cy.elements().stop();
  cy.elements().remove();
}

export function snapNodesToLayers(cy, { layerGap = 120 } = {}) {
  const vars = getAxisVars();
  if (!vars.length) return;

  const yOf = new Map();
  vars.forEach((v, i) => yOf.set(v, (i + 1) * layerGap));
  const terminalY = (vars.length + 1) * layerGap;

  cy.batch(() => {
    cy.nodes().forEach(n => {
      const lab = (n.data("label") ?? "").toString();
      const isTerm = n.hasClass("terminal") || lab === "0" || lab === "1";
      const y = isTerm ? terminalY : yOf.get(lab);
      if (y != null) n.position({ x: n.position("x"), y });
    });
  });
}