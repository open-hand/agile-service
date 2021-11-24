type KeysDistributeObject<T extends string> = { [K in T]: string }
type StringWithoutEmpty<T> = T extends string ? T : never
type StringConcatString<T1, T2> = `${StringWithoutEmpty<T1>}${StringWithoutEmpty<T2>}`
interface LocaleExportObject<I extends string, K extends string> {
  intlPrefix: I
  intlObject: KeysDistributeObject<K>
}
/**
 * 对象key增加统一前缀
 * @param obj
 */

export function localeAppendPrefixObjectKey<T extends string, P extends string, SK extends string = 'agile'>(obj: LocaleExportObject<T, P>, servicePrefix: SK | 'agile' = 'agile') {
  const intlPrefix = `${servicePrefix}.${obj.intlPrefix}.` as const;
  const keys = Object.keys(obj.intlObject) as Array<P>;
  const intlObject = keys
    .reduce((pre, key) => ({ ...pre, [`${intlPrefix}.${key}`]: obj[key as keyof typeof obj] }), {}) as {} as KeysDistributeObject<StringConcatString<typeof intlPrefix, P>>;
  return intlObject;
}
