export interface CostModel {
  costAsync(query: string, variables?: string): Promise<string>
}

export function compileAsync(code: string, globals: string): Promise<CostModel>
