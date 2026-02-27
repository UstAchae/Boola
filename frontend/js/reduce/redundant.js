// frontend/js/reduce/redundant.js
import { playSnapshotTrace } from "./snapPlayer.js";
import { REDUNDANT_ANIM } from "../config.js";

export async function playReduceRedundantTrace(cy, trace, opts = {}) {
  return playSnapshotTrace(cy, trace, {
    highlightMs: REDUNDANT_ANIM.highlightMs,
    betweenMs: REDUNDANT_ANIM.betweenBatchMs,
    ...opts
  });
}