import React from 'react';
import { observer } from 'mobx-react-lite';
import { Spin, Icon } from 'choerodon-ui';
import ReactEcharts from 'echarts-for-react';
import { EChartOption } from 'echarts';
import { map } from 'lodash';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import { IUnit, IEpic } from './search';
import finish from './image/finish.svg';
import styles from './index.less';
import { useFontSize } from '../context';

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
  issue_count: '问题计数',
  remain_time: '剩余时间',
};

export interface IEpicReportChart {
  allRemainTimes: number
  allStoryPoints: number
  completedRemainTimes: number
  completedStoryPoints: number
  groupDay: string
  issueCompletedCount: number
  issueCount: number
  unEstimateIssueCount: number
}

export interface IEpicReportTable {

}

export interface EpicReportProps {
  loading: boolean
  data: IEpicReportChart[]
  tableData: IEpicReportTable[]
  unit: IUnit
  epicId: string
  epics: IEpic[]
  animation?: boolean
}

// 处理后端返回值为null或小数精度问题
const dealNullValue = (list: number[] = []) => map(list, (item: number) => {
  if (item) {
    if (item % 1 > 0) {
      return item.toFixed(1);
    }
    return item || 0;
  }
  return 0;
});

const transformStoryPoints = (storyPoints: number) => (storyPoints && storyPoints > 0
  ? `${storyPoints % 1 > 0 ? storyPoints.toFixed(1) : storyPoints} 点` : storyPoints);

const transformRemainTime = (remainTime: number) => {
  if (!remainTime) {
    return '0';
  }
  let time: number = remainTime * 1;
  const w = Math.floor(time / 40);
  time -= 40 * w;
  const d = Math.floor(time / 8);
  time -= 8 * d;
  if (time % 1 > 0) {
    time = Number(time.toFixed(1));
  }
  return `${w ? `${w} 周 ` : ''}${d ? `${d} 天 ` : ''}${time ? `${time} 小时 ` : ''}`;
};

const EpicReport: React.FC<EpicReportProps> = ({
  loading, data, tableData, unit, epicId, epics, animation = true,
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
    const all = map(data, 'unEstimateIssueCount');
    return all;
  };

  const getLatest = (): {
    issueCount: number,
    issueCompletedCount: number,
    unEstimateIssueCount: number,
    allStoryPoints: number,
    allRemainTimes: number,
    completedRemainTimes: number,
    completedStoryPoints: number,
  } => {
    const chartData = data.slice();
    if (chartData && chartData.length) {
      return chartData[chartData.length - 1];
    }
    return {
      issueCount: 0,
      issueCompletedCount: 0,
      unEstimateIssueCount: 0,
      allStoryPoints: 0,
      allRemainTimes: 0,
      completedRemainTimes: 0,
      completedStoryPoints: 0,
    };
  };

  const getOption = (): EChartOption => {
    const UNIT = {
      总问题数: '个',
      已完成问题数: '个',
      未预估问题数: '个',
      已完成故事点: '点',
      总计故事点: '点',
      已完成剩余时间: '小时',
      总计剩余时间: '小时',
    };
    const commonOption: EChartOption = {
      animation,
      textStyle: {
        fontSize: getFontSize(12),
      },
      tooltip: {
        trigger: 'axis',
        formatter: (params: any[]) => {
          let content = '';
          if (!params || !params.length) {
            content = '';
          } else {
            content = `<div>${params[0].axisValue}</div>`;
            params.forEach((param) => {
              // @ts-ignore
              content += `<div style="font-size: 11px"><div style="background: ${param.color}"></div>${param.seriesName}：${param.value}${param.value ? UNIT[param.seriesName] : ''}</div>`;
            });
          }
          return content;
        },
      },
      legend: {
        orient: 'horizontal',
        padding: [0, 50, 0, 0],
        itemWidth: 14,
        itemGap: 30,
        data: [
          ...[
            unit === 'issue_count' ? {} : {
              name: `已完成${UNIT2NAME[unit]}`,
              icon: `image://${finish}`,
            },
          ],
          ...[
            unit === 'issue_count' ? {} : {
              name: `总计${UNIT2NAME[unit]}`,
              icon: 'rectangle',
            },
          ],
          ...[
            {
              name: '总问题数',
              icon: 'line',
            },
          ],
          ...[
            unit === 'issue_count' ? {} : {
              name: '未预估问题数',
              icon: 'line',
            },
          ],
          ...[
            unit === 'issue_count' ? {
              name: '已完成问题数',
              icon: 'line',
            } : {},
          ],
        ],
      },
      grid: {
        top: getFontSize(12) / 12 * (30),
        left: 0,
        right: '50',
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
            name: '问题计数',
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
            name: '总问题数',
            type: 'line',
            step: true,
            itemStyle: {
              color: 'rgba(48, 63, 159, 1)',
            },
            data: getChartDataYIssueCountAll(),
          },
          {
            name: '已完成问题数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#00bfa4',
            },
            data: getChartDataYIssueCountCompleted(),
          },
          {
            name: '未预估问题数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#ff9915',
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
            name: '问题计数',
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
            name: '总问题数',
            type: 'line',
            step: true,
            itemStyle: {
              color: 'rgba(48, 63, 159, 1)',
            },
            yAxisIndex: 1,
            data: getChartDataYIssueCountAll(),
          },
          {
            name: '已完成问题数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#00bfa4',
            },
            yAxisIndex: 1,
            data: getChartDataYIssueCountCompleted(),
          },
          {
            name: '未预估问题数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#ff9915',
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
              color: '#4e90fe',
            },
            areaStyle: {
              color: 'rgba(77, 144, 254, 0.1)',
            },
          },
          {
            name: `总计${UNIT2NAME[unit]}`,
            type: 'line',
            step: true,
            yAxisIndex: 0,
            data: getChartDataYAll(),
            itemStyle: {
              color: 'rgba(0, 0, 0, 0.16)',
            },
            areaStyle: {
              color: 'rgba(245, 245, 245, 0.5)',
            },
          },
        ],
      };
    }
    return { ...commonOption, ...option };
  };

  return (
    <div>
      <Spin spinning={loading}>
        <div className={styles.epicReport}>
          {
            data.length ? (
              <div
                className={styles.epicReport_chart}
                style={{
                  display: 'flex',
                }}
              >
                <ReactEcharts key={unit} option={getOption()} style={{ width: '80%', height: 400 }} />
                <div className={styles.epicReport_toolbar}>
                  <h2>汇总</h2>
                  <h4>问题汇总</h4>
                  <ul>
                    <li>
                      <span className={styles.epicReport_toolbar_tip}>合计：</span>
                      <span>
                        {`${getLatest().issueCount}${getLatest().issueCount > 0 ? ' 个' : ''}`}
                      </span>
                    </li>
                    {
                      unit === 'issue_count' ? (
                        <li>
                          <span className={styles.epicReport_toolbar_tip}>已完成：</span>
                          <span>{`${getLatest().issueCompletedCount}${getLatest().issueCompletedCount > 0 ? ' 个' : ''}`}</span>
                        </li>
                      ) : null
                    }
                    {
                      unit === 'issue_count' ? null : (
                        <li>
                          <span className={styles.epicReport_toolbar_tip}>未预估：</span>
                          <span>{`${getLatest().unEstimateIssueCount}${getLatest().unEstimateIssueCount > 0 ? ' 个' : ''}`}</span>
                        </li>
                      )
                    }
                  </ul>
                  {
                    unit !== 'issue_count' ? (
                      <div>
                        <h4>
                          {`${UNIT2NAME[unit]}`}
                          汇总
                        </h4>
                        <ul>
                          <li>
                            <span className={styles.epicReport_toolbar_tip}>合计：</span>
                            <span>
                              {unit === 'story_point' ? transformStoryPoints(getLatest().allStoryPoints) : transformRemainTime(getLatest().allRemainTimes)}
                            </span>
                          </li>
                          <li>
                            <span className={styles.epicReport_toolbar_tip}>已完成：</span>
                            <span>
                              {unit === 'story_point' ? transformStoryPoints(getLatest().completedStoryPoints) : transformRemainTime(getLatest().completedRemainTimes)}
                            </span>
                          </li>
                        </ul>
                      </div>
                    ) : null
                  }
                  <p
                    className={styles.primary}
                    style={{
                      cursor: 'pointer',
                    }}
                    role="none"
                    onClick={() => {
                      to(LINK_URL.workListIssue, {
                        type: 'project',
                        params: {
                          paramType: 'epic',
                          paramId: epicId,
                          paramName: `${epics.find((x: IEpic) => x.issueId === epicId)?.epicName}下的问题`,
                        },
                      }, { blank: true });
                    }}
                  >
                    在“所有问题”中查看
                    <Icon style={{ fontSize: getFontSize(13) }} type="open_in_new" />
                  </p>
                </div>
              </div>
            ) : (
              <div style={{ padding: '20px 0', textAlign: 'center', width: '100%' }}>
                {tableData.length ? '当前单位下问题均未预估，切换单位或从下方问题列表进行预估。' : '当前史诗下没有问题。'}
              </div>
            )
          }
        </div>
      </Spin>
    </div>
  );
};

export default EpicReport;
