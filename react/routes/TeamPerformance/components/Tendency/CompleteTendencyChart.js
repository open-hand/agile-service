// 趋势分析-工作项完成趋势
import React, { useContext, useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Spin } from 'choerodon-ui/pro';
import ReactEchartsCore from 'echarts-for-react/lib/core';
import echarts from 'echarts';
import EmptyBlock from '../EmptyBlock';
import SwitchTabs from '../SwitchTabs';
import Store from '../../stores';
import './index.less';

const fieldMap = {
  STORY: {
    plan: 'storyPoints',
    complete: 'storyPointsComplete',
  },
  TASK: {
    plan: 'remainingTime',
    complete: 'remainingTimeComplete',
  },
};

const CompeleteTendencyChart = observer(() => {
  const { questionTendencyHandleDS, completeTaskDS, completeStoryDS } = useContext(Store);
  const [isLoading, setIsLoading] = useState(true);
  const [show, setShow] = useState(false);
  const [chartData, setChartData] = useState(null);

  useEffect(() => {
    queryChartData();
  }, []);

  const queryChartData = async () => {
    setIsLoading(true);
    const currentDataSet = questionTendencyHandleDS.current.get('tab') === 'STORY' ? completeStoryDS : completeTaskDS;
    await currentDataSet.query();
    setChartData(currentDataSet.toData());
    setShow(currentDataSet.length > 6);
    setIsLoading(false);
  };

  const getOpts = () => {
    const currentTabKey = questionTendencyHandleDS.current.get('tab');
    const currentTab = questionTendencyHandleDS.getField('tab').getText(currentTabKey);
    return {
      legend: {
        show: true,
        icon: 'rect',
        orient: 'horizontal',
        x: 'right',
        y: 0,
        padding: [18, 50, 0, 0],
        itemGap: 20,
        itemHeight: 2,
        itemWidth: 24,
      },
      dataset: {
        source: chartData,
      },
      tooltip: {
        trigger: 'axis',
        formatter(params) {
          let content = '';
          if (params && params.length) {
            content = `<div>冲刺：${params[0].name}</div>`;
            params.forEach(({
              color, seriesName, value,
            }) => {
              content += `<div>
              <div style="display:inline-block; width: 10px; height: 10px; margin-right: 3px; border-radius: 50%; background:${color}"></div>
              ${seriesName}：${seriesName === '完成' ? value[fieldMap[currentTabKey].complete] : value[fieldMap[currentTabKey].plan]}
              </div>`;
            });
          }
          return content;
        },
        textStyle: {
          color: 'var(--text-color)',
          fontSize: 12,
          lineHeight: 20,
        },
        extraCssText: 'background: #FFFFFF;\n'
          + 'border: 1px solid #DDDDDD;\n'
          + 'box-shadow: 0 2px 4px 0 rgba(0,0,0,0.20);\n'
          + 'border-radius: 0;\n'
          + 'padding: 8px',
      },
      grid: {
        top: 50,
        left: 30,
        right: 54,
        bottom: 30,
        containLabel: true,
      },
      xAxis: {
        type: 'category',
        boundaryGap: false,
        axisTick: { show: false },
        interval: 0,
        axisLine: {
          show: true,
          lineStyle: {
            color: '#eee',
          },
          onZero: true,
        },
        splitLine: {
          show: true,
          lineStyle: {
            color: ['#eee'],
          },
        },
        axisLabel: {
          show: true,
          textStyle: {
            color: 'var(--text-color3)',
            fontSize: 12,
            fontStyle: 'normal',
          },
        },
      },
      yAxis: {
        name: currentTab,
        nameTextStyle: {
          color: 'var(--text-color)',
          padding: [0, 10, 0, 0],
        },
        minInterval: 1,
        axisTick: { show: false },
        axisLine: {
          show: true,
          lineStyle: {
            color: '#eee',
          },
        },
        axisLabel: {
          show: true,
          textStyle: {
            color: 'var(--text-color3)',
            fontSize: 12,
            fontStyle: 'normal',
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
          type: 'line',
          name: '计划',
          color: 'rgba(136, 168, 240, 1)',
          smooth: true,
          smoothMonotone: 'x',
          symbol: 'circle',
          itemStyle: {
            normal: {
              color: 'rgba(136, 168, 240, 1)',
            },
          },
          areaStyle: {
            color: {
              type: 'linear',
              x: 0,
              y: 0,
              x2: 0,
              y2: 1,
              colorStops: [{
                offset: 1, color: 'rgba(136, 168, 240, 0)', // 0% 处的颜色
              }, {
                offset: 0, color: 'rgba(136, 168, 240, 1)', // 100% 处的颜色
              }],
              global: false,
            },
          },
          dimensions: [
            { name: 'sprintName', type: 'ordinal' },
            { name: currentTabKey === 'STORY' ? 'storyPoints' : 'remainingTime', type: 'number' },
          ],
        },
        {
          type: 'line',
          name: '完成',
          color: 'rgba(136, 231, 240, 1)',
          smooth: true,
          smoothMonotone: 'x',
          symbol: 'circle',
          itemStyle: {
            normal: {
              color: 'rgba(136, 231, 240, 1)',
            },
          },
          areaStyle: {
            color: {
              type: 'linear',
              x: 0,
              y: 0,
              x2: 0,
              y2: 1,
              colorStops: [{
                offset: 1, color: 'rgba(136, 231, 240, 0)', // 0% 处的颜色
              }, {
                offset: 0, color: 'rgba(136, 231, 240, 1)', // 100% 处的颜色
              }],
              global: false,
            },
          },
          dimensions: [
            { name: 'sprintName', type: 'ordinal' },
            { name: currentTabKey === 'STORY' ? 'storyPointsComplete' : 'remainingTimeComplete', type: 'number' },
          ],
        },
      ],
      dataZoom: [{
        show,
        type: 'slider',
        left: 'center',
        bottom: 2,
        height: 15,
        width: '84%',
        startValue: 0,
        endValue: 5,
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
      }],
    };
  };

  return (
    <Spin spinning={isLoading}>
      <div className="chart-container chart-container-complete">
        <div className="line-chart-container">
          <div className="chart-handle line-chart-handle">
            <span className="chart-title">工作项完成趋势</span>
            {
              (chartData && chartData.length > 0) && (
                <SwitchTabs
                  dataSet={questionTendencyHandleDS}
                  onChange={() => queryChartData()}
                  style={{ flexShrink: 0, height: '37px', marginLeft: '40px' }}
                />
              )
            }
          </div>
          {
            (chartData && chartData.length > 0) && (
              <div className="line-chart-main">
                <ReactEchartsCore
                  echarts={echarts}
                  option={getOpts()}
                  notMerge
                  lazyUpdate
                  style={{
                    height: 410,
                  }}
                />
              </div>
            )
          }
          {
            (Array.isArray(chartData) && chartData.length === 0) && (
              (
                <EmptyBlock
                  height={422}
                />
              )
            )
          }
        </div>
      </div>
    </Spin>
  );
});

export default CompeleteTendencyChart;
