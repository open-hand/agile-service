import originMoment, { Moment } from 'moment';
import { extendMoment } from 'moment-range';
import { map } from 'lodash';
// @ts-ignore
const moment = extendMoment(originMoment);
export type IBurnDownData = {
  coordinate: {
    [key: string]: number
  },
  expectCount: number
} | null
function getBetweenDateStr(start: Moment, end: Moment, restDays: string[]): {
  result: string[]
  rest: string[]
} {
  // 是否显示非工作日
  const range = moment.range(start, end);
  const days = Array.from(range.by('day'));
  const result = days.map((day) => day.format('YYYY-MM-DD'));
  const rest = days.filter((day) => restDays.includes(day.format('YYYY-MM-DD'))).map((day) => day.format('YYYY-MM-DD'));
  return { result, rest };
}
interface IBurnDownConfig {
  endDate: string
  restDayShow: boolean
  restDays: string[],
}
type IExportAxis = number[] | [[string, number], [string, number]]
type IMarkAreaData = [{
  xAxis: string
}, {
  xAxis: string
}][]
export function transformBurnDownChartData(
  data: IBurnDownData,
  {
    endDate: end,
    restDayShow,
    restDays,
  }: IBurnDownConfig,
): {
  xAxis: string[],
  yAxis: number[],
  exportAxis: IExportAxis,
  markAreaData: IMarkAreaData,
} {
  if (!data) {
    return {
      xAxis: [],
      yAxis: [],
      exportAxis: [],
      markAreaData: [],
    };
  }
  const endDate = end || '';
  const keys = Object.keys(data.coordinate);
  let [minDate, maxDate] = [keys[0], keys[0]];
  for (let a = 1, len = keys.length; a < len; a += 1) {
    if (moment(keys[a]).isAfter(maxDate)) {
      maxDate = keys[a];
    }
    if (moment(keys[a]).isBefore(minDate)) {
      minDate = keys[a];
    }
  }
  // 如果后端给的最大日期小于结束日期
  let allDate;
  let rest: string[] = [];
  /* eslint-disable */
  if (moment(maxDate).isBefore(endDate.split(' ')[0])) {
    const result = getBetweenDateStr(moment(minDate), moment(endDate.split(' ')[0]), restDays);
    allDate = result.result;
    rest = result.rest;
  } else if (moment(minDate).isSame(maxDate)) {
    allDate = [minDate];
  } else {
    const result = getBetweenDateStr(moment(minDate), moment(maxDate), restDays);
    allDate = result.result;
    rest = result.rest;
  }
  const allDateValues: number[] = [data.expectCount];
  const markAreaData: IMarkAreaData = [];
  let exportAxis: IExportAxis = [data.expectCount];
  // 如果展示非工作日，期望为一条连续斜线
  if (!restDayShow) {
    if (allDate.length) {
      exportAxis = [
        ['', data.expectCount],
        [allDate[allDate.length - 1].split(' ')[0].slice(5).replace('-', '/'), 0],
      ];
    }
  }
  for (let b = 0, len = allDate.length; b < len; b += 1) {
    const nowKey = allDate[b];
    // 显示非工作日，则非工作日期望为水平线
    if (restDayShow) {
      // 工作日天数
      const countWorkDay = (allDate.length - rest.length) || 1;
      // 日工作量
      const dayAmount = data.expectCount / countWorkDay;
      if (rest.includes(allDate[b])) {
        // 非工作日
        if (b < len) {
          markAreaData.push([
            {
              xAxis: b === 0 ? '' : allDate[b - 1].split(' ')[0].slice(5).replace('-', '/'),
            },
            {
              xAxis: allDate[b].split(' ')[0].slice(5).replace('-', '/'),
            },
          ]);
        }
        exportAxis[b + 1] = exportAxis[b];
      } else {
        // 工作量取整
        exportAxis[b + 1] = (exportAxis[b] as number - dayAmount) < 0
          ? 0
          : exportAxis[b] as number - dayAmount;
      }
    }
    if (data.coordinate.hasOwnProperty(nowKey)) {
      allDateValues.push(data.coordinate[allDate[b]]);
    } else if (moment(nowKey).isAfter(moment())) {
      // @ts-ignore
      allDateValues.push(null);
    } else {
      const beforeKey = allDate[b - 1];
      allDateValues.push(data.coordinate[beforeKey]);
      data.coordinate[nowKey] = data.coordinate[beforeKey];
    }
  }
  const sliceDate = map(allDate, item => item.slice(5).replace('-', '/'));
  return {
    xAxis: ['', ...sliceDate],
    yAxis: allDateValues,
    exportAxis,
    markAreaData,
  };
}
