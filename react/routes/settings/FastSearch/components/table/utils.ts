export function transformOperation(str: string) {
  // 注意该对象key的顺序
  const OPERATION = {
    '!=': '不等于',
    'not in': '不包含',
    in: '包含',
    'is not': '不是',
    is: '是',
    '<=': '小于或等于',
    '<': '小于',
    '>=': '大于或等于',
    '>': '大于',
    '=': '等于',
    OR: '或',
    AND: '与',
    'not like': '不包含',
    like: '包含',
  } as const;

  let transformKey = str;
  Object.keys(OPERATION).forEach((v: (keyof typeof OPERATION)) => {
    transformKey = transformKey.replace(new RegExp(` ${v} `, 'g'), ` ${OPERATION[v]} `);
  });
  return transformKey;
}
