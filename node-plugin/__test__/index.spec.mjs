import test from "ava";

import { compileAsync } from "../index.js";

test("Basic parsing", async (t) => {
  let value = await compileAsync("default => 1;", "{}");
  t.truthy(value);
});

test("Invalid model parsing fails", (t) => {
  return compileAsync("default => 1", "{}").catch((err) => {
    t.is(err.message, "Failed to compile cost model");
  });
});

test("Invalid globals parsing fails", async (t) => {
  return compileAsync("default => 1", "!#@!%@$!").catch((err) => {
    t.is(err.message, "Failed to compile cost model");
  });
});
