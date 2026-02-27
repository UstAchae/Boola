export async function postJson(url, body) {
  return fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body)
  });
}

export async function fetchTruthTable(expr, vars) {
  return postJson("/api/truth-table", { expr, vars });
}

export async function fetchBdd(expr, vars) {
  return postJson("/api/bdd", { expr, vars });
}

export async function fetchReduceTerminalsTrace(expr, vars, applied = []) {
  return postJson("/api/bdd/reduce-terminals-trace", { expr, vars, applied });
}

export async function fetchReduceRedundantTrace(expr, vars, applied = []) {
  return postJson("/api/bdd/reduce-redundant-trace", { expr, vars, applied });
}

export async function fetchReduceMergeTrace(expr, vars, applied = []) {
  return postJson("/api/bdd/reduce-merge-trace", { expr, vars, applied });
}