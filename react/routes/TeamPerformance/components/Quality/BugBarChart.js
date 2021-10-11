// 质量分析-柱状图
import React, { useContext, useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Spin, Form, Select } from 'choerodon-ui/pro';
import ReactEchartsCore from 'echarts-for-react/lib/core';
import echarts from 'echarts';
import SwitchTabs from '../SwitchTabs';
import EmptyBlock from '../EmptyBlock';
import Store from '../../stores';
import emptyChartPic from '../../image/empty_chart.svg';
import './index.less';

const BugBarChart = observer(() => {
  const { bugChartHandleDataSet, bugDistributionDataSet } = useContext(Store);
  const [barData, setBarData] = useState(null);
  const [show, setShow] = useState(false);

  useEffect(() => {
    queryBarData();
  }, []);

  const queryBarData = async () => {
    const { type, environment } = bugChartHandleDataSet.current.toData();
    bugDistributionDataSet.setQueryParameter('environment', environment);
    bugDistributionDataSet.setQueryParameter('type', type);
    await bugDistributionDataSet.query();
    setShow(bugDistributionDataSet.length > 10);
    setBarData(bugDistributionDataSet.toData());
  };

  const getOpts = () => ({
    legend: {
      show: false,
    },
    dataset: {
      source: barData,
    },
    tooltip: {
      trigger: 'axis',
      axisPointer: {
        type: 'shadow',
        shadowStyle: {
          color: 'rgba(176, 183, 224, 0.2)',
        },
      },
      formatter(params) {
        let content = '';
        if (params && params.length) {
          const { name, color, value } = params[0];
          content = `<div>
            <div style="display:inline-block; width: 10px; height: 10px; margin-right: 3px; border-radius: 50%; background:${color}"></div>
              ${name}：${value.bugCount}
            </div>
          `;
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
      left: 10,
      right: 12,
      bottom: 30,
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
        onZero: true,
      },
      splitLine: {
        show: false,
      },
      axisLabel: {
        textStyle: {
          color: 'var(--text-color3)',
          fontSize: 12,
          fontStyle: 'normal',
        },
      },
    },
    yAxis: {
      name: '工作项计数',
      minInterval: 1,
      nameGap: 23,
      axisTick: { show: false },
      axisLine: { show: false },
      splitLine: {
        show: true,
        lineStyle: {
          color: 'rgba(238, 238, 238, 1)',
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
    series: [
      {
        type: 'bar',
        color: 'rgba(249, 136, 148, 1)',
        barWidth: 10,
        itemStyle: {
          barBorderRadius: [5, 5, 0, 0],
        },
        dimensions: [
          { name: 'realName', type: 'ordinal' },
          { name: 'bugCount', type: 'number' },
        ],
      },
    ],
    dataZoom: [{
      show,
      type: 'slider',
      bottom: 0,
      left: 'center',
      height: 15,
      width: '77%',
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
  });

  return (
    <div className="quality-rank-container">
      <div className="bar-chart-container">
        <div className="chart-handle">
          <span className="chart-title">缺陷分布</span>
          <SwitchTabs
            dataSet={bugChartHandleDataSet}
            field="type"
            onChange={() => queryBarData()}
            style={{ flexShrink: 0, height: '37px', marginLeft: 'auto' }}
          />
          <Form dataSet={bugChartHandleDataSet} style={{ width: '130px', marginLeft: '15px' }}>
            <Select
              name="environment"
              clearButton={false}
              onChange={() => queryBarData()}
            />
          </Form>
        </div>
        <Spin dataSet={bugDistributionDataSet} style={{ height: '510px' }}>
          {
            (barData && barData.length > 0) && (
              <div className="line-chart-main">
                <ReactEchartsCore
                  echarts={echarts}
                  option={getOpts()}
                  notMerge
                  lazyUpdate
                  style={{
                    height: 520,
                  }}
                />
              </div>
            )
          }
          {
            (Array.isArray(barData) && barData.length === 0) && (
              <EmptyBlock
                pic={emptyChartPic}
                // height={330}
                height={510}
                des="当前暂无数据"
              />
            )
          }
        </Spin>
      </div>
    </div>
  );
});

export default BugBarChart;
