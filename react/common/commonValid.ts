/**
 * ts验证key是否在对象中，并返回可靠值
 * @param key
 * @param obj
 */
export function validKeyReturnValue<K extends keyof T, T extends object>(key: K, obj: T) {
  if (key in obj) {
    return obj[key];
  }
  throw Error(`${key} is not exist key in Object`);
}
