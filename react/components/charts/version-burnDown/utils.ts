import { OriginData, ChartData } from './index';

function transformZero2Placeholder(arr: (null | number)[]): (string | number)[] {
  // 处理小数精度和null
  return arr.map((item) => {
    if (item) {
      if (item % 1 > 0) {
        return item.toFixed(1);
      }
      return item;
    }
    return '-';
  });
}

export function getChartDataFromServerData(data: OriginData[]): ChartData {
  if (!data.length) {
    return [[], [], [], [], [], [], [], []];
  }
  const completed: number[] = [];
  const remaining: number[] = [];
  const added: number[] = [];
  const completedAgain: number[] = [];
  const assistant: number[] = [];
  const showZero: (0 | '-')[] = [];
  let showZeroBottom: (0 | '0.00001' | '-')[] = [];
  let showZeroTop: ('-' | '0.00001')[] = [];
  data.forEach((epicData) => {
    const {
      start, add, done,
    } = epicData;
    completed.push(start >= done ? done : start);
    remaining.push(start >= done ? start - done : 0);
    added.push(start >= done ? add : add - (done - start));
    completedAgain.push(start >= done ? 0 : done - start);
  });
  assistant.push(0);
  const len = completed.length;
  completed.forEach((v, i) => {
    if (i !== len - 1) {
      assistant.push(assistant[i] + v);
    }
  });
  assistant.forEach((v, i) => {
    showZero.push(
      !completed[i]
  && !remaining[i]
  && !added[i]
  && !completedAgain[i] ? 0 : '-',
    );
  });

  if (showZero.every((v) => v === 0)) {
    showZeroBottom = showZero;
    showZeroTop = Array.from({ length: showZero.length });
    showZeroTop = showZeroTop.map((v) => '-');
  } else {
    showZero.forEach((v, i) => {
      showZeroBottom.push(v === 0 && assistant[i] === 0 ? '0.00001' : '-');
      showZeroTop.push(v === 0 && assistant[i] !== 0 ? '0.00001' : '-');
    });
  }
  return [
    assistant,
    transformZero2Placeholder(completed),
    transformZero2Placeholder(remaining),
    transformZero2Placeholder(added),
    transformZero2Placeholder(completedAgain),
    showZeroBottom,
    showZeroTop,
    showZero,
  ];
}
