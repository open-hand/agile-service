import React from 'react';
import { observer } from 'mobx-react-lite';
import ReactEcharts from 'echarts-for-react';
import { map } from 'lodash';
import { Spin } from 'choerodon-ui/pro';
import { EChartOption } from 'echarts';
import { IUnit } from './search';
import { useFontSize } from '../context';

export interface ISprintSpeed {
  committedIssueCount: number
  committedRemainTime: number
  committedStoryPoints: number
  completedIssueCount: number
  completedRemainTime: number
  completedStoryPoints: number
  sprintId: string
  sprintName: string
}

export interface IterationSpeedProps {
  loading: boolean,
  unit: IUnit,
  data: ISprintSpeed[],
  option?: EChartOption
}

const UnitNameMap = new Map([
  ['story_point', '故事点'],
  ['issue_count', '工作项计数'],
  ['remain_time', '剩余时间'],
]);

const UnitMap = new Map([
  ['story_point', '点'],
  ['issue_count', '个'],
  ['remain_time', '小时'],
]);

const UNIT_STATUS = {
  issue_count: {
    committed: 'committedIssueCount',
    completed: 'completedIssueCount',
  },
  story_point: {
    committed: 'committedStoryPoints',
    completed: 'completedStoryPoints',
  },
  remain_time: {
    committed: 'committedRemainTime',
    completed: 'completedRemainTime',
  },
};

const IterationSpeed: React.FC<IterationSpeedProps> = ({
  loading, unit, data, option,
}) => {
  const getChartDataYCommitted = () => {
    const prop = UNIT_STATUS[unit].committed;
    const committed = map(data, prop);
    return committed;
  };

  const getChartDataYCompleted = () => {
    const prop = UNIT_STATUS[unit].completed;
    const completed = map(data, prop);
    return completed;
  };
  const getFontSize = useFontSize();
  const FontSize = getFontSize(12);
  const getOption = (): EChartOption => ({
    textStyle: {
      fontSize: FontSize,
    },
    tooltip: {
      trigger: 'axis',
      axisPointer: {
        type: 'shadow',
      },
      formatter: (params: EChartOption.Tooltip.Format[]) => {
        let content = '';
        if (params && params.length) {
          content = `<div>${params[0].axisValue}</div>`;
          params.forEach((param) => {
            content += `<div style="font-size: 11px"><div style="display:inline-block; width: 10px; height: 10px; margin-right: 3px; border-radius: 50%; background:${param.color}"></div>${param.seriesName}：${param.value}${param.value ? UnitMap.get(unit) : ''}</div>`;
          });
        }
        return content;
      },
    },
    legend: {
      right: 50,
      itemWidth: 14,
      data: [
        {
          name: '预估',
          icon: 'rectangle',
        },
        {
          name: '已完成',
          icon: 'rectangle',
        },
      ],
    },
    grid: {
      top: '30',
      left: 0,
      right: '50',
      containLabel: true,
    },
    xAxis: {
      name: '冲刺',
      type: 'category',
      boundaryGap: true,
      nameTextStyle: {
        color: 'var(--text-color)',
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
        interval: 0,
        color: 'var(--text-color3)',
        fontSize: FontSize,
        formatter(value: string) {
          if (value.length > 10) {
            return `${value.slice(0, 11)}...`;
          }
          return value;
        },
      },
      splitLine: {
        show: false,
        interval: 0,
        lineStyle: {
          color: '#eee',
          width: 1,
          type: 'solid',
        },
      },
      data: map(data, 'sprintName'),
    },
    yAxis: {
      name: UnitNameMap.get(unit),
      type: 'value',
      nameTextStyle: {
        color: 'var(--text-color)',
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
        color: 'var(--text-color3)',
        fontSize: FontSize,
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
    dataZoom: [{
      startValue: map(data, 'sprintName')[0],
      endValue: map(data, 'sprintName')[8],
      zoomLock: true,
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
    series: [
      {
        name: '预估',
        type: 'bar',
        itemStyle: {
          color: '#d3d3d3',
        },
        data: getChartDataYCommitted(),
        emphasis: {
          itemStyle: {
            color: 'var(--divider)',
          },
        },
      },
      {
        name: '已完成',
        type: 'bar',
        data: getChartDataYCompleted(),
        itemStyle: {
          color: '#00bfa5',
        },
        lineStyle: {
          type: 'dashed',
          color: 'grey',
        },
        emphasis: {
          itemStyle: {
            color: '#35e6ce',
          },
        },
      },
    ],
    ...option,
  });

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

export default observer(IterationSpeed);
