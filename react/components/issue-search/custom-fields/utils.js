const canvas = document.createElement('canvas');
canvas.style.display = 'none';
document.body.append(canvas);
const ctx = canvas.getContext('2d');
export function getWidth(text, {
  fontSize = 13,
  fontFamily = "'Monospaced Number', 'Chinese Quote', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'PingFang SC', 'Hiragino Sans GB', 'Microsoft YaHei', 'Helvetica Neue', Helvetica, Arial, sans-serif",
} = {}) {
  ctx.font = `${fontSize}px ${fontFamily}`;

  const { width } = ctx.measureText(text);
  return width;
}

export function getSelectStyle(field, value) {
  const width = getWidth(field.name) + 28;
  // eslint-disable-next-line no-nested-ternary
  const autoWidth = value ? (Array.isArray(value) ? value.length > 0 : Boolean(value)) : false;
  return {
    minWidth: 'unset',
    width: autoWidth ? 'auto' : width,
    maxWidth: 150,
    margin: 0,
  };
}
