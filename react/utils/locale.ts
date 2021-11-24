export type KeysDistributeObject<T extends string> = { [K in T]: string }
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
    }), {}) as KeysDistributeObject<typeof intlPrefix>;
  return intlObject;
}
