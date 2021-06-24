import { EChartOption } from 'echarts';
import { map } from 'lodash';

export type IChartUnit = 'storyPoints' | 'quantity';
export type IChartType = 'line' | 'bar' | 'pie' | 'stackedBar';
export interface IChartData {
  comparedId: null | string,
  comparedValue: null | number | string,
  pointList: {
    analysisValue: string,
    analysisId: string,
    value: number,
    comparedValue: null | number | string,
    comparedId: null | string,
    percentage: number
  }[]
}

const xAxis = {
  type: 'category',
  boundaryGap: false,
  axisTick: { show: false },
  axisLine: {
    show: true,
    lineStyle: {
      color: '#eee',
      type: 'solid',
      width: 2,
    },
  },
  axisLabel: {
    show: true,
    color: '#0F1358',
    fontStyle: 'normal',
  },
  splitLine: {
    show: true,
    interval: 0,
    lineStyle: {
      color: '#eee',
      width: 1,
      type: 'solid',
    },
  },
};

const yAxis = {
  type: 'value',
  nameTextStyle: {
    color: 'rgba(15, 19, 88, 0.65)',
  },
  // nameGap: 22,
  axisTick: { show: false },
  axisLine: {
    show: true,
    lineStyle: {
      color: '#eee',
      type: 'solid',
      // width: 2,
    },
  },
  axisLabel: {
    show: true,
    margin: 18,
    color: '#0F1358',
    fontStyle: 'normal',
  },
  splitLine: {
    show: true,
    lineStyle: {
      color: '#eee',
      type: 'solid',
      width: 1,
    },
  },
};

const dataZoom = {
  bottom: 0,
  type: 'slider',
  // @ts-ignore
  height: 15,
  width: '85%',
  left: 60,
  startValue: 0,
  endValue: 11,
  zoomLock: true,
  handleIcon: 'M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z',
  handleSize: '100%',
  handleStyle: {
    color: '#fff',
    borderType: 'dashed',
    shadowBlur: 4,
    shadowColor: 'rgba(0, 0, 0, 0.6)',
    shadowOffsetX: 2,
    shadowOffsetY: 2,
  },
};

const legendPageStyle = {
  pageIconColor: '#5365EA',
  pageIconInactiveColor: 'rgba(83, 101, 234, 0.5)',
  pageTextStyle: {
    color: 'rgba(15, 19, 88, 0.65)',
  },
};
const getOptions = (chartType: IChartType, unit: IChartUnit, data: IChartData[], maxShow: number): any => {
  const xAxisData = map((data && data[0].pointList) || [], 'analysisValue');
  if (chartType === 'line' || chartType === 'bar') {
    return ({
      tooltip: {},
      xAxis: {
        ...xAxis,
        data: xAxisData,
      },
      yAxis: {
        ...yAxis,
        name: `单位：${unit === 'storyPoints' ? '故事点' : '问题计数'}`,
      },
      series: {
        type: chartType,
        data: map((data && data[0].pointList) || [], 'value'),
      },
      dataZoom: [{ ...dataZoom, show: xAxisData.length > maxShow }],
    });
  } if (chartType === 'pie') {
    return {
      tooltip: {
        trigger: 'item',
      },
      legend: {
        type: 'scroll',
        orient: 'vertical',
        right: 10,
        top: 'middle',
        ...legendPageStyle,
      },
      series: [
        {
          type: 'pie',
          radius: '75%',
          emphasis: {
            itemStyle: {
              shadowBlur: 10,
              shadowOffsetX: 0,
              shadowColor: 'rgba(0, 0, 0, 0.5)',
            },
          },
          data: map((data && data[0].pointList) || [], (point) => ({
            name: point.analysisValue,
            value: point.value,
          })),
        },
      ],
    };
  } if (chartType === 'stackedBar') {
    return {
      tooltip: {
        trigger: 'axis',
        axisPointer: {
          type: 'shadow',
        },
      },
      legend: {
        type: 'scroll',
        top: 10,
        ...legendPageStyle,
      },
      xAxis: {
        ...xAxis,
        data: xAxisData,
      },
      yAxis: {
        ...yAxis,
        name: unit === 'storyPoints' ? '故事点' : '问题计数',
      },
      series: (data || []).map((item) => ({
        name: item.comparedValue,
        type: 'bar',
        stack: 'total',
        label: {
          show: true,
        },
        emphasis: {
          focus: 'series',
        },
        data: map((item.pointList || []), 'value'),
      })),
      dataZoom: [{ ...dataZoom, show: xAxisData.length > maxShow }],
    };
  }
  return {};
};

export default getOptions;
