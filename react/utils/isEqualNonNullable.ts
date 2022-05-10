import isEqualWith from 'lodash/isEqualWith';

/**
* 无视空值（undefined、null、[]、""），空值视为相同, 深度比较两个值
*
*
* @category Lang
* @param value The value to compare.
* @param other The other value to compare.
* @returns Returns `true` if the values are equivalent, else `false`.
* @example
*
* var object = { 'user': 'fred', name: "" };
* var other = { 'user': 'fred', name: null };
*
* _.isEqualNonNullable(object, other);
* // => true
*
* object === other;
* // => false
*/
function isEqualNonNullable(value: any, other: any): boolean {
  function standardizeValue(v:any) {
    if (v === undefined || v === null || v === '' || (Array.isArray(v) && !v.length)) {
      return undefined;
    }
    return v;
  }
  function isEqualCustomizer(obj1: any, obj2: any) {
    if (standardizeValue(obj1) === standardizeValue(obj2)) {
      return true;
    }
    return undefined;
  }
  return isEqualWith(value, other, isEqualCustomizer);
}
export default isEqualNonNullable;
