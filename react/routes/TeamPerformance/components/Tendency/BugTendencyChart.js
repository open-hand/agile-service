// 趋势分析-缺陷趋势分析
import React, { useContext, useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Spin, Form, Select } from 'choerodon-ui/pro';
import ReactEchartsCore from 'echarts-for-react/lib/core';
import echarts from 'echarts';
import { groupBy, uniq } from 'lodash';
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

const BugTendencyChart = observer(() => {
  const { bugTendencyChartHandleDS, bugTendencyDS, bugResponsibleDS } = useContext(Store);
  const [show, setShow] = useState(false);
  const [chartData, setChartData] = useState(null);

  useEffect(() => {
    queryChartData();
  }, []);

  const queryChartData = async () => {
    const { type, environment } = bugTendencyChartHandleDS.current.toData();
    const { responsibleId } = bugResponsibleDS.current.toData();
    bugTendencyDS.setQueryParameter('environment', environment);
    bugTendencyDS.setQueryParameter('type', type);
    bugTendencyDS.setQueryParameter('responsibleIds', responsibleId);
    await bugTendencyDS.query();
    setShow(bugTendencyDS.length > 6);
    setChartData(bugTendencyDS.toData());
  };

  const getOpts = () => {
    const colors = ['#9665E2', '#F0657D', '#FAD352', '#FF9915', '#45A3FC', '#5365EA', '#47CBCA', '#59CB79', '#F953BA', '#D3D3D3'];
    const xData = uniq(chartData.map((item) => item.sprintName));
    const groupByIdData = groupBy(chartData, 'responsibleId');
    if (groupByIdData.length > 10) {
      for (let i = 10; i < groupByIdData.length; i += 1) {
      // eslint-disable-next-line
        colors.push(`#${(`00000${((Math.random() * 16777215 + 0.5) >> 0).toString(16)}`).slice(-6)}`);
      }
    }
    const sourceData = Object.keys(groupByIdData).map((key) => ({ source: groupByIdData[key] }));
    const seriesData = Object.keys(groupByIdData).map((key, index) => ({
      name: groupByIdData[key][0].realName || '未分配',
      type: 'line',
      datasetIndex: index,
      encode: { y: 'bugCount', x: 'sprintName' },
      smooth: true,
      smoothMonotone: 'x',
    }));
    return {
      legend: {
        show: true,
        type: 'scroll',
        orient: 'vertical', // 图例纵向排列
        top: 0,
        left: '86.4%',
        width: 140,
        height: 300,
        itemGap: 16,
        itemWidth: 30,
        borderRadius: 4,
        shadowColor: 'rgba(38, 38, 52, 0.09)',
        shadowOffsetX: 0,
        shadowOffsetY: 0,
        shadowBlur: 8,
        backgroundColor: '#fff',
        padding: [16, 40, 14, 16],
      },
      color: colors,
      dataset: sourceData,
      tooltip: {
        trigger: 'axis',
        confine: true,
        formatter(params) {
          let content = '';
          if (params && params.length) {
            content = `<div>冲刺：${params[0].name}</div>`;
            params.forEach(({
              color, seriesName, value,
            }) => {
              content += `<div>
              <div style="display:inline-block; width: 10px; height: 10px; margin-right: 3px; border-radius: 50%; background:${color}"></div>
              ${seriesName}：${value.bugCount}
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
        left: 40,
        bottom: 30,
        width: '78%',
        containLabel: true,
      },
      xAxis: {
        type: 'category',
        axisTick: { show: false },
        boundaryGap: false,
        interval: 0,
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
          show: true,
          textStyle: {
            color: 'var(--text-color3)',
            fontSize: 12,
            fontStyle: 'normal',
          },
        },
        data: xData,
      },
      yAxis: {
        name: '工作项计数',
        nameTextStyle: {
          color: 'var(--text-color)',
          padding: [0, 10, 0, 0],
        },
        minInterval: 1,
        axisTick: { show: false },
        axisLine: { show: false },
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
      series: seriesData,
      dataZoom: [{
        show,
        type: 'slider',
        left: 118,
        bottom: 2,
        height: 15,
        width: '66%',
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
    <Spin dataSet={bugTendencyDS}>
      <div className="chart-container chart-container-bug">
        <div className="line-chart-container">
          <div className="chart-handle  line-chart-handle">
            <span className="chart-title">缺陷趋势分析</span>
            <SwitchTabs
              field="type"
              dataSet={bugTendencyChartHandleDS}
              onChange={() => queryChartData()}
              style={{ marginLeft: '40px' }}
            />
            <Form dataSet={bugTendencyChartHandleDS} style={{ width: '130px', marginLeft: '15px' }}>
              <Select
                name="environment"
                clearButton={false}
                onChange={() => queryChartData()}
              />
            </Form>
            <Form dataSet={bugResponsibleDS} style={{ maxWidth: '500px', marginLeft: '15px' }}>
              <Select
                name="responsibleId"
                maxTagCount={5}
                searchable
                onChange={() => queryChartData()}
              />
            </Form>
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
                    height: 340,
                  }}
                />
              </div>
            )
          }
          {
            (Array.isArray(chartData) && chartData.length === 0) && (
              <EmptyBlock
                height={352}
                des="当前暂无数据"
              />
            )
          }
        </div>
      </div>
    </Spin>
  );
});

export default BugTendencyChart;
