import React from 'react';
import { Spin } from 'choerodon-ui';
import { map } from 'lodash';
import ReactEcharts from 'echarts-for-react';
import { EChartOption } from 'echarts';
// @ts-ignore
import { useIntl } from 'react-intl';
import { useFontSize } from '../context';
import { ServiceCodeQualityType } from './search';

export interface ServiceCodeQualityData {
  legend: string[]
  dates: string[]
  duplicatedLines: string[]
  duplicatedLinesRate: string[]
  nclocs: string[]
}
export interface ServiceCodeQualityProps {
  data: ServiceCodeQualityData
  loading: boolean
  option?: EChartOption
  type: ServiceCodeQualityType
}
const OBJECT_TYPE = {
  issue: [
    { name: 'bugs', color: '#5266d4' },
    { name: 'codeSmells', color: '#2196f3' },
    { name: 'vulnerabilities', color: '#00bcd4' },
  ],
  coverage: [
    { name: 'linesToCover', color: '#2196f3' },
    { name: 'coverLines', color: '#00bcd4' },
  ],
  duplicate: [
    { name: 'nclocs', color: '#2196f3' },
    { name: 'duplicatedLines', color: '#00bcd4' },
  ],
};

const ServiceCodeQuality: React.FC<ServiceCodeQualityProps> = ({
  loading, data, option, type,
}) => {
  const getFontSize = useFontSize();
  const FontSize = getFontSize(12);
  const dates = data.dates || [];
  const intl = useIntl();
  const { formatMessage } = intl;
  const getOption = (): EChartOption => {
    const series: EChartOption.Series[] = [];
    const legend: EChartOption.Legend.LegendDataObject[] = [];
    map(OBJECT_TYPE[type], ({ name, color }) => {
      // @ts-ignore
      if (data[name]) {
        series.push(
          {
            name: formatMessage({ id: `report.code-quality.${name}` }),
            type: 'line',
            symbol: 'circle',
            showSymbol: false,
            itemStyle: {
              color,
            },
            // @ts-ignore
            data: map(dates, (item, index) => [item, data[name][index]]),
          },
        );
        legend.push(
          {
            name: formatMessage({ id: `report.code-quality.${name}` }),
            icon: 'line',
          },
        );
      }
    });

    return {
      textStyle: {
        fontSize: FontSize,
      },
      legend: {
        data: legend,
        left: 'right',
        itemGap: 40,
        itemWidth: 34,
        selectedMode: false,
        padding: [5, 10, 5, 0],
      },
      grid: {
        left: 'left',
        right: 10,
        bottom: '3%',
        containLabel: true,
      },
      dataZoom: [
        {
          startValue: dates[0],
        },
        {
          type: 'inside',
        },
      ],
      xAxis: {
        type: 'time',
        axisTick: { show: false },
        axisLine: {
          lineStyle: {
            color: '#eee',
            type: 'solid',
            width: 2,
          },
        },
        axisLabel: {
          margin: 13,
          color: 'rgba(0, 0, 0, 0.65)',
          fontSize: 12,
          align: 'right',
        },
        splitLine: {
          lineStyle: {
            color: '#eee',
            width: 1,
            type: 'solid',
          },
        },
      },
      yAxis: {
        name: formatMessage({ id: type === 'issue' ? 'report.code-quality.number' : 'report.code-quality.rows' }),
        type: 'value',
        nameTextStyle: {
          fontSize: 13,
          color: '#000',
          padding: dates && dates.length ? undefined : [0, 0, 0, 25],
        },
        axisTick: { show: false },
        axisLine: {
          lineStyle: {
            color: '#eee',
            type: 'solid',
            width: 2,
          },
        },
        axisLabel: {
          margin: 13,
          color: 'rgba(0, 0, 0, 0.65)',
          fontSize: 12,
        },
        splitLine: {
          lineStyle: {
            color: '#eee',
            type: 'solid',
            width: 1,
          },
        },
        min: dates && dates.length ? undefined : 0,
        max: dates && dates.length ? undefined : 4,
        scale: true,
      },
      series,
      ...option,
    };
  };
  return (
    <div>
      <Spin spinning={loading}>
        <ReactEcharts
          className="c7n-chart"
          option={getOption()}
        />
      </Spin>
    </div>
  );
};

export default ServiceCodeQuality;
