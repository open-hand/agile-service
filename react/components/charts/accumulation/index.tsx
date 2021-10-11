import React, { useMemo, memo } from 'react';
import { Spin } from 'choerodon-ui/pro';
import { EChartOption } from 'echarts';
import ReactEcharts from 'echarts-for-react';
import { transformAccumulationData, IAccumulationData } from './utils';
import { useFontSize } from '../context';

export interface AccumulationChartProps {
  loading: boolean
  data: IAccumulationData[]
  animation?: boolean
}

const AccumulationChart: React.FC<AccumulationChartProps> = ({
  loading,
  data,
  animation = true,
}) => {
  const {
    legendData,
    newxAxis,
    legendSeries,
  } = useMemo(() => transformAccumulationData(data), [data]);
  const getFontSize = useFontSize();
  const getOption = (): EChartOption => ({
    animation,
    textStyle: {
      fontSize: getFontSize(12),
    },
    tooltip: {
      trigger: 'axis',
      formatter(params) {
        let content = '';
        if (params instanceof Array) {
          const paramsContent = params.map((item) => (
            `<div style="font-size: 11px">
                <div style={display:inline-block; width: 10px; height: 10px; margin-right: 3px; border-radius: 50%; background:${item.color}}></div>
                ${item.seriesName}：${item.data} ${item.data ? ' 个' : ''}
              </div>`
          ));
          params.forEach((item, index, arr) => {
            content = `<div>
              <span>${params[0].axisValue}</span>
              <br />
             ${paramsContent.join('\n')}
            </div>`;
          });
        }

        return content;
      },
    },
    legend: {
      right: '90',
      data: legendData,
      top: '3%',
      itemGap: 30,
      itemWidth: 14,
      itemHeight: 14,
    },
    grid: {
      left: '20',
      right: '40',
      top: '8%',
      containLabel: true,
    },
    xAxis: [
      {
        name: '日期',
        type: 'category',
        splitLine: {
          show: true,
          lineStyle: {
            // 使用深浅的间隔色
            color: 'rgba(116,59,231,0.10)',
            opacity: 0.9,
            // type: 'dashed',
          },
        },
        boundaryGap: false,
        data: newxAxis,
        axisLabel: {
          fontSize: getFontSize(12),
          show: true,
          formatter(value: string) {
            // return `${value.split('-')[2]}/${MONTH[value.split('-')[1] * 1]}月`;
            return value.slice(5);
          },
        },
      },
    ],
    yAxis: [
      {
        splitLine: {
          show: true,
          lineStyle: {
            // 使用深浅的间隔色
            color: 'rgba(116,59,231,0.10)',
            opacity: 0.9,
            // type: 'dashed',
          },
        },
        name: '工作项数',
        type: 'value',
        minInterval: 1,
        axisLabel: {
          fontSize: getFontSize(12),
        },
      },
    ],
    series: legendSeries,
    dataZoom: [{
      startValue: newxAxis[0],
      type: 'slider',
      handleIcon: 'M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z',
      handleSize: '80%',
      handleStyle: {
        color: '#fff',
        shadowBlur: 3,
        shadowColor: 'rgba(0, 0, 0, 0.6)',
        shadowOffsetX: 2,
        shadowOffsetY: 2,
      },
      // right: '50%',
      // left: '0%',
    }],

  });

  return (
    <Spin spinning={loading}>
      <ReactEcharts
        option={getOption()}
        style={{
          height: '600px',
        }}
        notMerge
        lazyUpdate
      />
    </Spin>
  );
};

export default memo(AccumulationChart);
