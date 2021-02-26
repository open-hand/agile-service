export function split(str:string, gutter:string):string[] {
  for (let i = 0; i < str.length; i += 1) {
    if (str[i] === gutter) {
      return [str.slice(0, i), str.slice(i + 1)];
    }
  }
  return [str];
}
