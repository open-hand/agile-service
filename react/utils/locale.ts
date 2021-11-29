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
/**
 * 异步加载多个多语言模块
 * @param servicePrefix
 * @param args
 * @returns
 */
export async function asyncImportLocales<T extends { [key: string]: string }>(...args: Promise<any>[]): Promise<Record<string, T>> {
  const localePackages = await Promise.all(args);
  return Object.values(localePackages).reduce((pre, current, currentIndex) => {
    // 避免多模块重复覆盖
    const uniqueCurrentModules = Object.keys(current).reduce((m, key) => ({
      ...m,
      [`${key}${currentIndex}`]: current[key],
    }), {});
    return ({ ...pre, ...uniqueCurrentModules });
  }, {});
}
/**
 * 合并多个导入多语言导入模块
 * @param args
 * @returns
 */
export function mergeLocaleModule(...args: Array<Record<string, Record<string, string>> | Record<string, string>>): Record<string, string> {
  return args.reduce<Record<string, string>>((p, m) => {
    const currentModuleValues = Object.values(m);
    return ({ ...p, ...(currentModuleValues.some((i) => typeof i === 'string') ? [m] : currentModuleValues).reduce((mp, r) => ({ ...mp, ...r }), {}) });
  }, {});
}
