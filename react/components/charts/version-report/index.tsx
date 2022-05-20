import React from 'react';
import { observer } from 'mobx-react-lite';
import { Spin } from 'choerodon-ui/pro';
import ReactEcharts from 'echarts-for-react';
import { EChartOption } from 'echarts';
import { map } from 'lodash';
import { IUnit } from './search';
import total from './image/total.svg';
import noEstimated from './image/noEstimated.svg';
import finish from './image/finish.svg';
import { useFontSize } from '../context';
import { IChartSearchAdditionalProps } from '../types.';

const UNIT_STATUS = {
  issue_count: {
    committed: undefined,
    completed: undefined,
  },
  story_point: {
    committed: 'allStoryPoints',
    completed: 'completedStoryPoints',
  },
  remain_time: {
    committed: 'allRemainTimes',
    completed: 'completedRemainTimes',
  },
};
const UNIT2NAME = {
  story_point: '故事点',
  issue_count: '工作项计数',
  remain_time: '剩余时间',
};

export interface IVersionReportChart {
  allRemainTimes: number
  allStoryPoints: number
  completedRemainTimes: number
  completedStoryPoints: number
  groupDay: string
  issueCompletedCount: number
  issueCount: number
  unEstimateIssueCount: number
}

export interface IVersionReportTable {

}

export interface VersionReportProps {
  loading: boolean
  data: IVersionReportChart[]
  tableData: IVersionReportTable[]
  unit: IUnit
  animation?: boolean
}

// 处理后端返回值为null或小数精度工作项
const dealNullValue = (list: number[] = []) => map(list, (item: number) => {
  if (item) {
    if (item % 1 > 0) {
      return item.toFixed(1);
    }
    return item || 0;
  }
  return 0;
});

const VersionReport: React.FC<VersionReportProps> = ({
  loading, data, tableData, unit, animation = false,
}) => {
  const getFontSize = useFontSize();
  const getChartDataYAll = () => {
    const prop = UNIT_STATUS[unit].committed;
    if (!prop) {
      return [];
    }
    const all: number[] = map(data, prop);
    return dealNullValue(all);
  };

  const getChartDataYCompleted = () => {
    const prop = UNIT_STATUS[unit].completed;
    if (!prop) {
      return [];
    }
    const completed = map(data, prop);
    return dealNullValue(completed);
  };

  const getChartDataYIssueCountAll = () => {
    if (unit !== 'issue_count') {
      return [];
    }
    const all = map(data, 'issueCount');
    return all;
  };

  const getChartDataYIssueCountCompleted = () => {
    if (unit === 'issue_count') {
      const all = map(data, 'issueCompletedCount');
      return all;
    }
    return [];
  };

  const getChartDataYIssueCountUnEstimate = () => {
    if (unit === 'issue_count') {
      return [];
    }
    const all = map(data, (v) => (v.unEstimateIssueCount <= 0 ? 0 : Number((
      v.unEstimateIssueCount / v.issueCount
    ).toFixed(2)) * 100).toFixed(0));
    return all;
  };

  const getOption = (): EChartOption => {
    const UNIT = {
      总工作项数: '个',
      已完成工作项数: '个',
      未预估工作项百分比: '%',
      已完成故事点: '点',
      总计故事点: '点',
      已完成剩余时间: '小时',
      总计剩余时间: '小时',
    };
    const commonOption: EChartOption = {
      animation,
      tooltip: {
        trigger: 'axis',
        formatter: (params: any[]) => {
          let content = '';
          if (params && params.length) {
            content = `<div>${params[0].axisValue}</div>`;
            params.forEach((param) => {
              // @ts-ignore
              content += `<div style="font-size: 11px"><div style="display:inline-block; width:10px; height:10px; margin-right: 3px; border-radius: 50%; border-radius: 50%; background: ${param.color}"></div>${param.seriesName}：${param.value}${param.value ? UNIT[param.seriesName] : ''}</div>`;
            });
          }
          return content;
        },
      },
      legend: {
        orient: 'horizontal',
        padding: [0, 50, 0, 0],
        itemWidth: 14,
        data: [
          ...[
            unit === 'issue_count' ? {} : {
              name: `总计${UNIT2NAME[unit]}`,
              icon: `image://${total}`,
            },
          ],
          ...[
            unit === 'issue_count' ? {} : {
              name: `已完成${UNIT2NAME[unit]}`,
              icon: `image://${finish}`,
            },
          ],
          ...[
            unit === 'issue_count' ? {
              name: '总工作项数',
              icon: `image://${total}`,
            } : {},
          ],
          ...[
            unit === 'issue_count' ? {} : {
              name: '未预估工作项百分比',
              icon: `image://${noEstimated}`,
            },
          ],
          ...[
            unit === 'issue_count' ? {
              name: '已完成工作项数',
              icon: `image://${finish}`,
            } : {},
          ],
        ],
      },
      grid: {
        top: getFontSize(12) / 12 * (30),
        left: 0,
        right: '20',
        containLabel: true,
      },
      xAxis: {
        // name: '日期',
        type: 'category',
        boundaryGap: false,
        nameLocation: 'end',
        nameGap: -10,
        nameTextStyle: {
          color: 'var(--text-color)',
          padding: [35, 0, 0, 0],
        },
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
          interval: map(data, 'groupDay').length >= 20 ? 4 : 0,
          margin: 13,
          color: 'var(--text-color3)',
          fontSize: getFontSize(12),
          fontStyle: 'normal',
          formatter(value: string) {
            return value.slice(5);
          },
        },
        splitArea: {
          show: false,
          interval: 0,
          areaStyle: {
            color: ['rgba(0, 0, 0, 0.16)'],
          },
        },
        splitLine: {
          show: true,
          interval: 0,
          lineStyle: {
            color: '#eee',
            width: 2,
            type: 'solid',
          },
        },
        data: map(data, 'groupDay'),
      },
      dataZoom: [{
        startValue: map(data, 'groupDay')[0],
        type: 'slider',
        handleIcon: 'M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z',
        handleSize: '100%',
        handleStyle: {
          color: '#fff',
          shadowBlur: 3,
          shadowColor: 'rgba(0, 0, 0, 0.6)',
          shadowOffsetX: 2,
          shadowOffsetY: 2,
        },
      }],
    };
    let option: EChartOption;
    if (unit === 'issue_count') {
      option = {
        yAxis: [
          {
            name: '工作项数',
            nameTextStyle: {
              color: 'var(--text-color)',
            },
            type: 'value',
            minInterval: 1,
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
              margin: 18,
              color: 'var(--text-color3)',
              fontSize: getFontSize(12),
              fontStyle: 'normal',
            },
            splitLine: {
              show: true,
              lineStyle: {
                color: '#eee',
                type: 'solid',
                width: 2,
              },
            },
          },
        ],
        series: [
          {
            name: '总工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#78aafe',
            },
            areaStyle: {
              color: 'rgba(77, 144, 254, 0.1)',
            },
            data: getChartDataYIssueCountAll(),
          },
          {
            name: '已完成工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#00bfa4',
            },
            areaStyle: {
              color: 'rgba(0, 191, 165, 0.1)',
            },
            data: getChartDataYIssueCountCompleted(),
          },
          {
            name: '未预估工作项百分比',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#f44336',
            },
            areaStyle: {
              color: 'rgba(244, 67, 54, 0.1)',
            },
            data: getChartDataYIssueCountUnEstimate(),
          },
        ],
      };
    } else {
      option = {
        yAxis: [
          {
            name: UNIT2NAME[unit],
            nameTextStyle: {
              color: 'var(--text-color)',
            },
            type: 'value',
            minInterval: 1,
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
              margin: 18,
              color: 'var(--text-color3)',
              fontSize: getFontSize(12),
              fontStyle: 'normal',
              formatter(value: string) {
                if (value && unit === 'remain_time') {
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
                width: 2,
              },
            },
          },
          {
            name: '百分比',
            nameTextStyle: {
              color: 'var(--text-color)',
            },
            type: 'value',
            minInterval: 1,
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
              margin: 18,
              color: 'var(--text-color3)',
              fontSize: getFontSize(12),
              fontStyle: 'normal',
            },
            splitLine: {
              show: false,
            },
          },
        ],
        series: [
          {
            name: '总工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#78aafe',
            },
            areaStyle: {
              color: 'rgba(77, 144, 254, 0.1)',
            },
            yAxisIndex: 1,
            data: getChartDataYIssueCountAll(),
          },
          {
            name: '已完成工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#00bfa4',
            },
            areaStyle: {
              color: 'rgba(0, 191, 165, 0.1)',
            },
            yAxisIndex: 1,
            data: getChartDataYIssueCountCompleted(),
          },
          {
            name: '未预估工作项百分比',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#f44336',
            },
            areaStyle: {
              color: 'rgba(244, 67, 54, 0.1)',
            },
            yAxisIndex: 1,
            data: getChartDataYIssueCountUnEstimate(),
          },
          {
            name: `已完成${UNIT2NAME[unit]}`,
            type: 'line',
            step: true,
            yAxisIndex: 0,
            data: getChartDataYCompleted(),
            itemStyle: {
              color: '#00bfa4',
            },
            areaStyle: {
              color: 'rgba(0, 191, 165, 0.1)',
            },
          },
          {
            name: `总计${UNIT2NAME[unit]}`,
            type: 'line',
            step: true,
            yAxisIndex: 0,
            data: getChartDataYAll(),
            itemStyle: {
              color: '#78aafe',
            },
            areaStyle: {
              color: 'rgba(77, 144, 254, 0.1)',
            },
          },
        ],
      };
    }
    return {
      ...commonOption,
      ...option,
      textStyle: {
        fontSize: getFontSize(12),
      },
    };
  };

  return (
    <div>
      <Spin spinning={loading}>
        <div className="c7n-report">
          {
            data.length ? (
              <div className="c7n-chart">
                <ReactEcharts option={getOption()} style={{ height: 400 }} />
              </div>
            ) : (
              <div style={{ padding: '20px 0', textAlign: 'center', width: '100%' }}>
                {tableData.length ? '当前单位下工作项均未预估，切换单位或从下方工作项列表进行预估。' : '当前版本下没有工作项。'}
              </div>
            )
          }
        </div>
      </Spin>
    </div>
  );
};

export default observer(VersionReport);
