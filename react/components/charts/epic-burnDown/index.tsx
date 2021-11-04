import React from 'react';
import { observer } from 'mobx-react-lite';
import ReactEcharts from 'echarts-for-react';
import {
  map, trim, fill, sum, floor, last,
} from 'lodash';
import { Spin } from 'choerodon-ui/pro';
import { EChartOption } from 'echarts';
import LINK_URL from '@/constants/LINK_URL';
import pic from '@/assets/image/NoData.svg';
import to from '@/utils/to';
import completed from './image/completed.svg';
import sprintIcon from './image/sprintIcon.svg';
import storyPointIcon from './image/storyPointIcon.svg';
import speedIcon from './image/speedIcon.svg';
import styles from './index.less';
import { useFontSize } from '../context';
import useIsProgram from '@/hooks/useIsProgram';

export type ChartData = [number[], (string | number)[], (string | number)[], (string | number)[], (string | number)[], (0 | '0.00001' | '-')[], ('-' | '0.00001')[], (0 | '-')[]];
export interface OriginData {
  add: number,
  done: number,
  endDate: string
  left: number
  name: string
  start: number
  startDate: string
}
export interface EpicBurnDownChartProps {
  loading: boolean,
  checked: 'checked' | undefined,
  data: OriginData[],
  chartData: ChartData,
  option?: EChartOption
}

const EpicBurnDown: React.FC<EpicBurnDownChartProps> = ({
  checked, chartData, data, loading, option: propsOption,
}) => {
  const getFontSize = useFontSize();
  const { isProgram } = useIsProgram();
  const transformPlaceholder2Zero = (arr: any[]) => arr.map((v) => (v === '-' ? 0 : v));

  const getStoryPoints = () => {
    const lastRemain = last(transformPlaceholder2Zero(chartData[2]));
    const lastAdd = last(transformPlaceholder2Zero(chartData[3]));
    return Number(lastRemain) + Number(lastAdd);
  };

  const getSprintSpeed = () => {
    if (data.length > 3) {
      const lastThree = data.slice(data.length - 3, data.length);
      const lastThreeDone: number[] = [];
      lastThree.forEach((item) => {
        lastThreeDone.push(item.done);
      });
      return floor(sum(lastThreeDone) / 3, 2);
    }
    return 0;
  };

  const getSprintCount = () => Math.ceil(getStoryPoints() / getSprintSpeed());

  const renderToolbarTitle = () => {
    if (getSprintSpeed() === 0) {
      return `根据最近${data.length}次冲刺的数据，无法预估迭代次数`;
    }
    return `根据最近${data.length}次冲刺的数据，将花费${getSprintCount()}个迭代来完成此史诗。`;
  };

  const renderToolbar = () => {
    if (data.length < 3) {
      return (
        <div className={styles.toolbar_cannot_forcast}>
          <h3 className={styles.title}>尚不可预测</h3>
          <div className={styles.word}>至少3个冲刺完成，才能显示预测</div>
        </div>
      );
    }
    if (getStoryPoints() === 0) {
      return (
        <div className={styles.toolbar_complete}>
          <div className={styles.pic}>
            <img src={completed} alt="所有预估的工作项都已完成!" />
          </div>
          <div className={styles.word}>所有预估的工作项都已完成！</div>
        </div>
      );
    }
    return (
      <div className={styles.toolbar_forcast}>
        <h3 className={styles.title}>{renderToolbarTitle()}</h3>
        <div className={styles.toolbar_forcast_content}>
          <div className={styles.word}>
            <div className={styles.icon}>
              <img src={sprintIcon} alt="冲刺迭代" />
            </div>
            <span>{`冲刺迭代：${!getSprintSpeed() ? '无法预估' : getSprintCount()}`}</span>
          </div>
          <div className={styles.word}>
            <div className={styles.icon}>
              <img src={speedIcon} alt="冲刺速度" />
            </div>
            <span>{`冲刺速度：${getSprintSpeed()}`}</span>
          </div>
          <div className={styles.word}>
            <div className={styles.icon}>
              <img src={storyPointIcon} alt="剩余故事点" />
            </div>
            <span>{`剩余故事点：${getStoryPoints()}`}</span>
          </div>
        </div>

      </div>
    );
  };

  const getLegendData = () => {
    const arr = ['工作已完成', '工作剩余', '工作增加'];
    const legendData: { name: string, textStyle: object }[] = [];
    arr.forEach((item) => {
      legendData.push({
        name: item,
        textStyle: { fontSize: getFontSize(12) },
      });
    });
    return legendData;
  };

  const getOption = (): EChartOption => {
    const inverse = !checked;
    const option: EChartOption = {
      textStyle: {
        fontSize: getFontSize(12),
      },
      grid: {
        top: 30,
        left: 40,
        right: 50,
        containLabel: true,
      },
      xAxis: [
        {
          type: 'category',
          data: map(data, 'name'),
          axisTick: { show: false },
          axisLine: {
            show: true,
            lineStyle: {
              color: '#eee',
              type: 'solid',
              width: 1,
            },
          },
          axisLabel: {
            interval: 0,
            show: true,
            showMinLabel: true,
            align: 'right',
            // @ts-ignore
            textStyle: {
              color: 'var(--text-color3)',
              fontSize: getFontSize(12),
            },
            formatter(value: string) {
              if (data.length >= 7) {
                return value.length > 5 ? `${value.slice(0, 5)}...` : value;
              }
              if (data.length >= 10) {
                return value.length > 3 ? `${value.slice(0, 3)}...` : value;
              }
              return value.length > 7 ? `${value.slice(0, 7)}...` : value;
            },
          },
        },
      ],
      yAxis: [
        {
          inverse,
          type: 'value',
          position: 'left',
          axisTick: { show: false },
          axisLine: {
            show: true,
            lineStyle: {
              color: '#eee',
              type: 'solid',
              width: 1,
            },
          },
          axisLabel: {
            show: true,
            formatter(value: string) {
              return !value ? value : '';
            },
          },
          splitLine: {
            lineStyle: {
              color: '#eee',
            },
          },
        },
        {
          inverse,
          type: 'value',
          position: 'right',
          axisTick: { show: false },
          axisLine: {
            show: true,
            lineStyle: {
              color: '#eee',
              type: 'solid',
              width: 1,
            },
          },
          axisLabel: {
            show: true,
            formatter(value: string) {
              return !value ? value : '';
            },
          },
          splitLine: {
            lineStyle: {
              color: '#eee',
            },
          },
        },
      ],
      legend: {
        show: true,
        data: getLegendData(),
        right: 50,
        itemWidth: 14,
        itemHeight: 14,
        itemGap: 30,
        icon: 'rect',
      },
      tooltip: {
        show: true,
        trigger: 'axis',
        axisPointer: { // 坐标轴指示器，坐标轴触发有效
          type: 'shadow', // 默认为直线，可选为：'line' | 'shadow'
        },
        backgroundColor: '#fff',
        textStyle: {
          color: 'var(--text-color)',
          fontSize: getFontSize(13),
        },
        borderColor: '#ddd',
        borderWidth: 1,
        extraCssText: 'box-shadow: 0 2px 4px 0 rgba(0,0,0,0.20);',
        formatter(params: any[]) {
          // eslint-disable-next-line no-param-reassign
          params[0].name = trim(params[0].name, '\n\n');
          const sprint = data.filter((item) => item.name === params[0].name)[0];
          let res = `<span>${params[0].name}</span>`;
          res += `<span style="display:block; margin-top: 0px; margin-bottom: 2px; color: rgba(0,0,0,0.54); font-size: 11px;">${sprint.startDate && sprint.startDate.split(' ')[0].split('-').join('/')}-${sprint.endDate && sprint.endDate.split(' ')[0].split('-').join('/')}</span>`;
          res += `本迭代开始时故事点数：${sprint.start}`;
          res += `<br/>工作已完成: ${(params[1].value === '-' ? 0 : Number(params[1].value)) + (params[4].value === '-' ? 0 : Number(params[4].value))}`;
          res += `<br/>工作增加: ${sprint.add}`;
          res += `<br/>本迭代结束时剩余故事点数: ${(params[2].value === '-' ? 0 : Number(params[2].value)) + (params[3].value === '-' ? 0 : Number(params[3].value))}`;
          return res;
        },
      },
      series: [
        {
          name: '辅助',
          type: 'bar',
          stack: '总量',
          barWidth: 52,
          itemStyle: {
            normal: {
              barBorderColor: 'rgba(0,0,0,0)',
              color: 'rgba(0,0,0,0)',
            },
            emphasis: {
              barBorderColor: 'rgba(0,0,0,0)',
              color: 'rgba(0,0,0,0)',
            },
          },
          data: (checked === 'checked') ? fill(Array(chartData[0].length), 0) : chartData[0],
        },
        {
          name: '工作已完成',
          type: 'bar',
          stack: '总量',
          barMinHeight: 15,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'inside',
                color: '#fff',
                formatter(param: any) {
                  return param.value === '-' ? null : `-${param.value}`;
                },
              },
              color: 'rgba(0,191,165,0.8)',
            },
          },
          // data: ['-', '-', 16, 3, '-'],
          data: chartData[1],
        },
        {
          name: '工作剩余',
          type: 'bar',
          stack: '总量',
          barMinHeight: 15,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'inside',
                color: '#fff',
              },
              // color: 'rgb(0, 187, 255, 0.8)',
              color: 'rgba(69,163,252,0.80)',
            },
          },
          // data: [3, 3, '-', 13, 18],
          data: chartData[2],
        },
        {
          name: '工作增加',
          type: 'bar',
          stack: '总量',
          barMinHeight: 15,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'inside',
                color: '#fff',
                formatter(param: any) {
                  return param.value === '-' ? null : `+${param.value}`;
                },
              },
              // color: 'rgba(27,128,255,0.8)',
              color: 'rgba(27,128,223,0.80)',
              opacity: 0.75,
            },
          },
          // data: ['-', 13, 16, 5, '-'],
          data: chartData[3],
        },
        {
          name: 'compoleted again',
          type: 'bar',
          stack: '总量',
          barMinHeight: 15,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'inside',
                color: '#fff',
                formatter(param: any) {
                  return param.value === '-' ? null : `-${param.value}`;
                },
              },
              color: 'rgba(0,191,165,0.8)',
            },
          },
          // data: ['-', '-', 3, '-', '-'],
          data: chartData[4],
        },
        {
          name: 'showZeroBottom',
          type: 'bar',
          stack: '总量',
          barMinHeight: 2,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'bottom',
                color: 'var(--text-color)',
                formatter() {
                  return 0;
                },
              },
              color: 'rgba(0,0,0,0.54)',
            },
          },
          // data: ['-', '-', 3, 3, '-'],
          data: inverse ? chartData[5] : [],
        },
        {
          name: 'showZeroTop',
          type: 'bar',
          stack: '总量',
          barMinHeight: 2,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'top',
                color: 'var(--text-color)',
                formatter() {
                  return 0;
                },
              },
              color: 'rgba(0,0,0,0.54)',
            },
          },
          // data: ['-', '-', 3, 3, '-'],
          data: inverse ? chartData[6] : chartData[7],
        },
      ],
      ...propsOption,
    };
    return option;
  };

  const renderChart = () => {
    if (!data.length) {
      return (
        <div style={{
          display: 'flex', justifyContent: 'center', alignItems: 'center', padding: '50px 0', textAlign: 'center',
        }}
        >
          <img src={pic} alt="没有预估故事点" />
          <div style={{ textAlign: 'left', marginLeft: '50px' }}>
            <span style={{ fontSize: getFontSize(12), color: 'var(--text-color3)' }}>报表不能显示</span>
            <p style={{ marginTop: 10, fontSize: getFontSize(20) }}>
              在此史诗中没有预估的故事，请在
              {isProgram ? '待办事项' : (
                <span
                  style={{
                    color: '#5365EA',
                    cursor: 'pointer',
                  }}
                  role="none"
                  onClick={() => {
                    to(LINK_URL.workListBacklog);
                  }}
                >
                  待办事项
                </span>
              )}
              中创建故事并预估故事点。
            </p>
          </div>
        </div>
      );
    }
    return (
      <div className={styles.c7n_report}>
        <div className={styles.c7n_chart}>
          {
            loading ? null : (
              <div style={{ position: 'relative' }}>
                <div className={styles.c7n_chart_yaxixName}>
                  故事点
                </div>
                <ReactEcharts
                  option={getOption()}
                  style={{ height: 400, left: -31 }}
                />
              </div>
            )
          }
        </div>
        <div className={styles.c7n_toolbar}>
          {renderToolbar()}
        </div>
      </div>
    );
  };

  return (
    <Spin spinning={loading}>
      <div className={styles.epicBurnDown_chart}>
        {
          renderChart()
        }
      </div>
    </Spin>

  );
};

export default observer(EpicBurnDown);
