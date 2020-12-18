import moment from 'moment';
import { orderBy } from 'lodash';

export type IAccumulationData = {
  categoryCode: string
  color: string
  columnId: string
  coordinateVOList: { date: string, issueCount: number }[]
  name: string
}

function getAsterisk(count: number): string {
  let str = '';
  for (let index = 0, len = count; index < len; index += 1) {
    str += '*';
  }
  return str;
}
const colors = ['#743BE7', '#F953BA', '#4090FE', '#d07da6', '#FFB100', '#00BFA5'];
export function transformAccumulationData(
  accumulationData: IAccumulationData[],
) {
  if (!accumulationData) {
    return {
      legendData: [],
      newxAxis: [],
      legendSeries: [],
    };
  }
  const countMap = new Map<string, number>();
  // 处理name相同的数据，末尾加*
  const usedColors: string[] = [];
  const data = accumulationData.map((item, index) => {
    let { name } = item;
    if (countMap.has(item.name)) {
      name += getAsterisk(countMap.get(item.name) as number);
    } else {
      countMap.set(item.name, 0);
    }
    countMap.set(item.name, countMap.get(item.name) as number + 1);
    const color = usedColors.includes(item.color) ? colors[index % 6] : item.color;
    usedColors.push(color);
    return {
      ...item,
      name,
      color,
    };
  });
  const legendData = data.map((item) => ({ icon: 'rect', name: item.name }));
  const dates = data.reduce((result: string[], { coordinateVOList }) => {
    coordinateVOList.forEach(({ date }) => {
      const day = moment(date).format('YYYY-MM-DD');
      if (!result.includes(day)) {
        result.push(day);
      }
    });
    return result;
  }, []);
  const orderedDates = orderBy(dates, (item) => new Date(item).getTime());
  const legendSeries = data.reverse().map(({ name, color, coordinateVOList }) => {
    const countData = coordinateVOList.reduce((result: Map<string, number>, { date, issueCount }) => {
      const day = moment(date).format('YYYY-MM-DD');
      if (result.has(day)) {
        const count = result.get(day) || 0;
        result.set(day, Math.max(count, issueCount, 0));
      } else {
        result.set(day, Math.max(issueCount, 0));
      }
      return result;
    }, new Map());
    const counts: number[] = [];
    orderedDates.forEach((date, index) => {
      if (countData.has(date)) {
        counts.push(countData.get(date));
      } else {
        counts.push(counts[index - 1] || 0);
      }
    });
    return {
      name,
      type: 'line',
      stack: name,
      areaStyle: {
        normal: {
          color,
          opacity: 0.1,
        },
      },
      lineStyle: {
        normal: {
          color,
        },
      },
      itemStyle: {
        normal: { color },
      },
      data: counts,
      symbol: 'circle',
    };
  });
  const newxAxis = (orderBy(dates, (item) => new Date(item).getTime()));
  return {
    legendData,
    newxAxis,
    legendSeries,
  };
}
