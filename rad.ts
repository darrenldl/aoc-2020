import type { Task, Tasks } from "https://deno.land/x/rad/src/mod.ts";

const clean = `fd -H -I .merlin -x rm {}`
const format = `fd -E _build ml . -x opam exec ocamlformat -- -i --enable-outside-detected-project`;
export const tasks: Tasks = {
  ...{c: clean, clean},
  ...{ f: format, format },
};
