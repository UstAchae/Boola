import { playSnapshotTrace } from "./snapPlayer.js";
import { MERGE_ANIM } from "../config.js";

export async function playReduceMergeTrace(cy, trace, opts = {}) {
  return playSnapshotTrace(cy, trace, {
    highlightMs: MERGE_ANIM.highlightMs,
    betweenMs: MERGE_ANIM.betweenBatchMs,
    ...opts
  });
}