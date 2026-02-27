import { playSnapshotTrace } from "./snapPlayer.js";
import { TERM_ANIM } from "../config.js";

export async function playReduceTerminalsTrace(cy, trace, opts = {}) {
  return playSnapshotTrace(cy, trace, {
    highlightMs: TERM_ANIM.highlightMs,
    betweenMs: TERM_ANIM.betweenBatchMs,
    ...opts
  });
}