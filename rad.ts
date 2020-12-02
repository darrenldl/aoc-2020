import type { Task, Tasks } from "https://deno.land/x/rad/src/mod.ts";

const format = `fd -E _build ml . -x opam exec ocamlformat -- -i --enable-outside-detected-project`;
export const tasks: Tasks = {
  ...{ f: format, format },
};
