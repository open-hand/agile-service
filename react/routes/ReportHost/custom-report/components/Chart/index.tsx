import React, { useMemo, memo } from 'react';
import { Spin } from 'choerodon-ui';
import { EChartOption } from 'echarts';
import ReactEcharts from 'echarts-for-react';
import { useFontSize } from '@/components/charts/context';

export type IChartUnit = 'storyPoints' | 'quantity';
export type IChartType = 'line' | 'bar' | 'pie' | 'stackedBar';

export interface ChartProps {
  loading: boolean,
  data: any,
  chartType?: IChartType,
  type?: IChartUnit,
  option?: EChartOption
}

const Chart: React.FC<ChartProps> = ({
  type,
  loading,
  data,
  chartType,
  option,
}) => {
  const getFontSize = useFontSize();
  const FontSize = getFontSize(12);
  const renderChartTitle = () => {
    let result = '';
    if (type === 'storyPoints') {
      result = '故事点';
    }
    if (type === 'quantity') {
      result = '问题计数';
    }
    return result;
  };
  const getOption = (): EChartOption => ({
    textStyle: {
      fontSize: FontSize,
    },
    tooltip: {
      trigger: 'axis',
      backgroundColor: '#fff',
      textStyle: {
        color: 'var(--text-color)',
      },
      extraCssText:
        'box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2); border: 1px solid #ddd; border-radius: 0;',
      formatter(params) {
        let content = '';
        let unit = '';
        if (params instanceof Array) {
          params.forEach((item) => {
            if (item.value && type === 'storyPoints') {
              unit = ' 点';
            }
            if (item.value && type === 'quantity') {
              unit = ' 个';
            }
            content = `${item.axisValue || '冲刺开启'}<br />${item.marker}${item.seriesName} : ${(item.value || item.value === 0) ? item.value : '-'}${unit && unit}`;
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
      data: [],
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
        color: 'var(--text-color3)',
        fontSize: FontSize,
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
    },
    yAxis: {
      name: renderChartTitle(),
      nameTextStyle: {
        color: 'var(--text-color)',
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
        color: 'var(--text-color3)',
        fontSize: FontSize,
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
    },
    series: [],
    ...option,
  });

  return (
    <Spin spinning={loading}>
      <ReactEcharts option={getOption()} />
    </Spin>
  );
};

export default memo(Chart);
