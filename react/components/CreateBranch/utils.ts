/**
 * 将任意颜色转为rgba颜色
 * @param color
 * @param alpha
 * @returns
 */
export function colorTransformRgba(color: string, alpha = 1) {
  const reg = /^#([0-9a-fA-f]{3}|[0-9a-fA-f]{6})$/;
  let currentColor = color.toLowerCase();
  const rgb = [];
  if (reg.test(color)) {
    if (color.length === 4) {
      const sColorNew = currentColor.slice(1).split('')
        .map((v, index) => currentColor.slice(index, index + 1)
          .concat(currentColor.slice(index, index + 1)));
      currentColor = `#${sColorNew.join('')}`;
    }
    // 处理六位的颜色值
    for (let index = 1; index < 7; index += 2) {
      const channel = parseInt(`0x${currentColor.slice(index, index + 2)}`, 16);
      rgb.push(channel);
    }
  } else if (currentColor.slice(0, 3) === 'rgb') {
    const startPos = currentColor.indexOf('(');
    const endPos = currentColor.indexOf(')');
    rgb.push(...currentColor.slice(startPos, endPos).split(',').slice(0, 3));
  }
  return `rgba(${rgb.join(',')},${alpha})`;
}
