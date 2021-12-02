// 进度与效率-柱状图
import React, { useContext, useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Spin } from 'choerodon-ui/pro';
import ReactEchartsCore from 'echarts-for-react/lib/core';
import echarts from 'echarts';
import EmptyBlock from '../EmptyBlock';
import SwitchTabs from '../SwitchTabs';
import Store from '../../stores';
import './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const LineChart = observer(() => {
  const { lineDimensionDS, lineEfficiencyStoryDS, lineEfficiencyTaskDS } = useContext(Store);
  const [isLoading, setIsLoading] = useState(true);
  const [show, setShow] = useState(false);
  const [xDatas, setXdatas] = useState(null);
  const formatMessage = useFormatMessage('agile.performance');

  useEffect(() => {
    queryLineData();
  }, []);

  const queryLineData = async () => {
    setIsLoading(true);
    const currentDataSet = lineDimensionDS.current.get('tab') === 'STORY' ? lineEfficiencyStoryDS : lineEfficiencyTaskDS;
    await currentDataSet.query();
    const { xData = [] } = currentDataSet.current.toData();
    setShow(xData.length > 10);
    setXdatas(xData);
    setIsLoading(false);
  };

  /**
   * 切换故事点和任务工时
   * @param {string} key
   */
  const handleChangeTab = () => {
    queryLineData();
    lineDimensionDS.current.set('dimension', 'PLAN');
  };

  const getOpts = () => {
    const currentTabKey = lineDimensionDS.current.get('tab');
    const currentTab = lineDimensionDS.getField('tab').getText(currentTabKey);
    const currentDataSet = currentTabKey === 'STORY' ? lineEfficiencyStoryDS : lineEfficiencyTaskDS;
    const {
      xData = [], plan = [], complete = [], percent = [],
    } = currentDataSet.current ? currentDataSet.current.toData() : {};

    return {
      textStyle: {
        fontSize: 12,
      },
      legend: {
        orient: 'horizontal',
        x: 'right',
        y: 0,
        padding: [10, 78, 0, 0],
        itemWidth: 20,
        itemHeight: 10,
        itemGap: 20,
        data: [
          {
            name: '计划',
            icon: 'rect',
          },
          {
            name: '实际',
            icon: 'rect',
          },
          {
            name: '百分比',
            icon: 'line',
          },
        ],
      },
      tooltip: {
        trigger: 'axis',
        axisPointer: {
          type: 'shadow',
          shadowStyle: {
            color: 'rgba(176, 183, 224, 0.2)',
          },
        },
        formatter: (params) => {
          let content = '';
          if (params && params.length) {
            content = `<div>${params[0].name}</div>`;
            params.forEach(({
              color, seriesName, value, seriesType,
            }) => {
              content += `<div>
              <div style="display:inline-block; width: 10px; height: 10px; margin-right: 3px; border-radius: 50%; background:${color}"></div>
              ${seriesName}：${value}${seriesType === 'line' ? '%' : ''}
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
        left: 13,
        right: 12,
        bottom: 20,
        containLabel: true,
      },
      xAxis: {
        type: 'category',
        axisTick: { show: false },
        axisLine: {
          show: false,
          lineStyle: {
            color: '#eee',
          },
        },
        nameTextStyle: {
          color: 'var(--text-color)',
        },
        axisLabel: {
          textStyle: {
            color: 'var(--text-color3)',
            fontSize: 12,
            fontStyle: 'normal',
          },
          formatter(value, index) {
            if (value.length > 10) {
              return `${value.slice(0, 11)}...`;
            }
            return value;
          },
        },
        splitLine: { show: false },
        data: xData,
      },
      yAxis: [{
        type: 'value',
        name: currentTab,
        nameTextStyle: {
          color: 'var(--text-color)',
          padding: [0, 10, 0, 0],
        },
        axisTick: { show: false },
        axisLine: { show: false },
        axisLabel: {
          show: true,
          interval: 'auto',
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
      }, {
        name: '百分比',
        type: 'value',
        nameTextStyle: {
          color: 'var(--text-color)',
          padding: [0, 0, 0, 50],
        },
        min: 0,
        axisLabel: {
          show: true,
          interval: 'auto',
          textStyle: {
            color: 'var(--text-color3)',
            fontSize: 12,
            fontStyle: 'normal',
          },
          formatter: '{value}%',
        },
        axisTick: { show: false },
        axisLine: {
          show: false,
          lineStyle: {
            color: '#eee',
            type: 'solid',
            width: 2,
          },
        },
        splitLine: { show: false },
      }],
      series: [
        {
          name: '计划',
          type: 'bar',
          itemStyle: {
            color: '#88A8F0',
          },
          barWidth: 10,
          barGap: '0.6',
          data: plan,
        },
        {
          name: '实际',
          type: 'bar',
          barWidth: 10,
          itemStyle: {
            color: '#88DFF0',
          },
          data: complete,
        },
        {
          name: '百分比',
          type: 'line',
          yAxisIndex: 1,
          itemStyle: {
            normal: {
              color: '#743BE7',
            },
          },
          data: percent,
          lineStyle: {
            color: '#743BE7',
          },
        },
      ],
      dataZoom: [{
        show,
        type: 'slider',
        bottom: 6,
        left: 100,
        height: 15,
        width: '82%',
        startValue: 0,
        endValue: 9,
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

  const currentTabKey = lineDimensionDS.current.get('tab');
  const currentTab = lineDimensionDS.getField('tab').getText(currentTabKey);

  return (
    <Spin spinning={isLoading}>
      <div className="chart-container chart-container-line">
        <div className="chart-handle">
          <span className="chart-title">{formatMessage({ id: 'story.completed' })}</span>
          <SwitchTabs
            dataSet={lineDimensionDS}
            onChange={handleChangeTab}
            style={{ flexShrink: 0, height: '37px', marginLeft: '40px' }}
          />
        </div>
        {
          (xDatas && xDatas.length > 0) && (
            <div className="line-chart-main">
              <ReactEchartsCore
                echarts={echarts}
                option={getOpts()}
                notMerge
                lazyUpdate
                style={{
                  height: 330,
                }}
              />
            </div>
          )
        }
        {
          (Array.isArray(xDatas) && xDatas.length === 0) && (
            <EmptyBlock
              height={400}
            />
          )
        }
      </div>
    </Spin>
  );
});

export default LineChart;
