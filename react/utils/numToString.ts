/**
 *  数字不保留0 的字符串
 * @param {*} num
 * @returns
 */
function numToStringWithoutZero(num: number, digits: number = 2) {
  const strNum = String(num);
  // 是数字并且不为0
  if (/^(-?\d+)(\.\d+)?$/.test(strNum) && !!Number(strNum.replace(/\./g, ''))) {
    return parseFloat(Number(num).toFixed(digits));
  }
  return '0';
}

export default numToStringWithoutZero;
