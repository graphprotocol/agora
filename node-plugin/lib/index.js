var addon = require('../native')

function promisify(f) {
  return new Promise((resolve, reject) =>
    f((err, result) => {
      if (err) {
        reject(err)
      } else {
        resolve(result)
      }
    })
  )
}

class CostModel {
  constructor(native) {
    this._native = native
  }

  /**
   * query: A graphQL query string
   * variables: (Optional) a JSON string of variables to be substituted into the query
   */
  async costAsync(query, variables) {
    let cost = await promisify((r) => this._native.cost(r, query, variables))
    // See also e5d47bc3-9b14-490d-abec-90c286330a2d
    if (!cost) {
      throw new Error('Failed to cost query')
    }
    return cost
  }
}

/**
 * Compiles a cost model from text.
 * Throws if the cost model is invalid.
 * If this compiles successfully, you can call costAsync on the result.
 *
 * Performance Tip: Re-use the compiled cost model for many queries.
 *
 * code: A cost model as text.
 * globals: (Optional) a JSON object string of global variables for the cost model.
 */
async function compileAsync(code, globals) {
  let native = new addon.CostModel(code, globals)

  let result = await promisify(native.compile.bind(native))
  // See also e5d47bc3-9b14-490d-abec-90c286330a2d
  if (!result) {
    throw new Error('Failed to compile cost model')
  }

  return new CostModel(native)
}

// TODO: Move this to a unit test framework
/*
async function test_one(code, query, variables) {
    let model;
    try {
        model = await compileAsync(code);
    }
    catch (err) {
        throw (err);
    }

    return await model.costAsync(query, variables);
}

// These tests caught various problems (including 3 conditions that caused the process to exit, and one segfault...)
async function test() {
    async function throws(f, msg) {
        let fail = false;
        try {
            await f();
            fail = true;
        } catch {

        }
        if (fail) {
            throw msg;
        }
    }
    // Invalid model
    await throws(() => test_one("query;", "query { a }"), "Costed invalid model");

    // Invalid query
    await throws(() => test_one("query { a } => 10;", "fail"), "Costed invalid query");

    // Ok
    let cost = await test_one("query { a } => 10;", "{ a }");
    if (cost !== "10") {
        throw "Cost != 10";
    }

    // No match
    await throws(() => test_one("query { a } => 10;", "{ b }"), "Costed unmatched query");

    // With vars
    cost = await test_one("query { a(skip: 3) { b } } => 9;", "query Skip($skip: Int) { a(skip: $skip) { b } }", "{ \"skip\": 3 }");
    if (cost !== "9") {
        throw "Cost != 9";
    }

    console.log("SUCCESS");
}
*/

module.exports = {
  compileAsync,
}
