type KeysDistributeObject<T extends string> = { [K in T]: string }
type StringWithoutEmpty<T> = T extends string ? T : never
type StringConcatString<T1, T2> = `${StringWithoutEmpty<T1>}${StringWithoutEmpty<T2>}`
interface LocaleExportObject<T extends string, K extends string, SK extends string = 'agile'> {
  intlPrefix: T
  servicePrefix?: SK
  intlObject: KeysDistributeObject<K>
}
/**
 * 对象key增加统一前缀
 * @param obj
 */

export function localeAppendPrefixObjectKey<T extends string, P extends string, SK extends string>(obj: LocaleExportObject<T, P, SK>) {
  const intlPrefix = `${obj.servicePrefix || 'agile'}.${obj.intlPrefix}`;
  const keys = Object.keys(obj.intlObject) as Array<P>;
  const intlObject = keys
    .reduce((pre, key) => ({
      ...pre,
      [`${intlPrefix}.${key}`]: obj.intlObject[key as keyof typeof obj.intlObject],
    }), {}) as {} as KeysDistributeObject<StringConcatString<`${SK}.${T}.` | `agile.${T}.`, P>>;
  return [intlPrefix, intlObject] as [`${SK}.${T}` | `agile.${T}`, typeof intlObject];
}
