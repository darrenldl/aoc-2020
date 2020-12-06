import type { Task, Tasks } from "https://deno.land/x/rad/src/mod.ts";

const clean: Task = `fd -H -I .merlin -x rm {}`
const format: Task = `fd -E _build ml . -x opam exec ocamlformat -- -i --enable-outside-detected-project`;
export const tasks: Tasks = {
  ...{c: clean, clean},
  ...{ f: format, format },
};
