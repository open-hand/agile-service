import React, { useMemo, memo } from 'react';
import { Spin } from 'choerodon-ui';
import { EChartOption } from 'echarts';
import ReactEcharts from 'echarts-for-react';
import { transformBurnDownChartData, IBurnDownData } from './utils';

interface Props {
  select: 'remainingEstimatedTime' | 'storyPoints' | 'issueCount',
  loading: boolean
  data: IBurnDownData
  endDate: string
  restDayShow: boolean
  restDays: string[],
}

const BurndownChart: React.FC<Props> = ({
  select,
  loading,
  data,
  endDate,
  restDayShow,
  restDays,
}) => {
  const {
    xAxis, yAxis, exportAxis, markAreaData,
  } = useMemo(() => transformBurnDownChartData(data, {
    endDate,
    restDayShow,
    restDays,
  }), [data, endDate, restDayShow, restDays]);
  const renderChartTitle = () => {
    let result = '';
    if (select === 'remainingEstimatedTime') {
      result = '剩余时间';
    }
    if (select === 'storyPoints') {
      result = '故事点';
    }
    if (select === 'issueCount') {
      result = '问题计数';
    }
    return result;
  };
  const getOption = (): EChartOption => ({
    tooltip: {
      trigger: 'axis',
      backgroundColor: '#fff',
      textStyle: {
        color: '#000',
      },
      extraCssText:
        'box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2); border: 1px solid #ddd; border-radius: 0;',
      formatter(params) {
        let content = '';
        let unit = '';
        if (params instanceof Array) {
          params.forEach((item) => {
            if (item.seriesName === '剩余值') {
              if (item.value && select === 'remainingEstimatedTime') {
                unit = ' 小时';
              }
              if (item.value && select === 'storyPoints') {
                unit = ' 点';
              }
              if (item.value && select === 'issueCount') {
                unit = ' 个';
              }
              content = `${item.axisValue || '冲刺开启'}<br />${item.marker}${item.seriesName} : ${(item.value || item.value === 0) ? item.value : '-'}${unit && unit}`;
            }
          });
        }
        return content;
      },
    },
    legend: {
      top: '24px',
      right: '3.2%',
      data: [{
        name: '期望值',
        icon: 'line',
      }, {
        name: '剩余值',
        icon: 'line',
      }],
    },
    grid: {
      top: 60,
      bottom: 30,
      left: 0,
      right: 40,
      containLabel: true,
    },
    xAxis: {
      type: 'category',
      boundaryGap: false,
      data: xAxis,
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
        interval: Math.floor(xAxis.length / 7) - 1 || 0,
        color: 'rgba(0, 0, 0, 0.65)',
        fontSize: 12,
        fontStyle: 'normal',
      },
      splitLine: {
        show: true,
        interval: 0,
        lineStyle: {
          color: ['#eee'],
          width: 1,
          type: 'solid',
        },
      },
    },
    yAxis: {
      name: renderChartTitle(),
      nameTextStyle: {
        color: '#000',
      },
      nameGap: 22,
      type: 'value',
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
        // interval: 'auto',
        margin: 18,
        color: 'rgba(0, 0, 0, 0.65)',
        fontSize: 12,
        fontStyle: 'normal',
        formatter(value: string) {
          if (select === 'remainingEstimatedTime' && value) {
            return `${value}h`;
          }
          return value;
        },
      },
      splitLine: {
        show: true,
        lineStyle: {
          color: '#eee',
          type: 'solid',
          width: 1,
        },
      },
    },
    series: [
      {
        symbol: 'none',
        name: '期望值',
        type: 'line',
        data: exportAxis,
        itemStyle: {
          color: 'rgba(0,0,0,0.65)',
        },
        lineStyle: {
          type: 'dotted',
          color: 'rgba(0,0,0,0.65)',
        },
        markArea: {
          itemStyle: {
            color: 'rgba(235,235,235,0.65)',
          },
          emphasis: {
            itemStyle: {
              color: 'rgba(220,220,220,0.65)',
            },
          },
          // @ts-ignore
          data: markAreaData,
        },
      },
      {
        symbol: 'none',
        name: '剩余值',
        type: 'line',
        itemStyle: {
          color: '#4D90FE',
        },
        // stack: '总量',
        data: yAxis,
      },
    ],
  });

  return (
    <Spin spinning={loading}>
      <ReactEcharts option={getOption()} />
    </Spin>
  );
};

export default memo(BurndownChart);
