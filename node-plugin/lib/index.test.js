const { compileAsync } = require('.')

describe('Cost model', () => {
  test('Basic parsing', async () => {
    await expect(compileAsync('default => 1;', '{}')).resolves.toBeTruthy()
  })

  test('Invalid model parsing fails', async () => {
    await expect(compileAsync('default => 1', '{}')).rejects.toStrictEqual(
      new Error('Failed to compile cost model')
    )
  })

  test('Invalid globals parsing fails', async () => {
    await expect(
      compileAsync('default => 1;', '!#@!%@$!')
    ).rejects.toStrictEqual(new Error('Failed to compile cost model'))
  })
})
