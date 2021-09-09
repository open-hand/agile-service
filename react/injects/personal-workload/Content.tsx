import React, { useCallback, useState, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import Echart from 'echarts-for-react';
import { EChartOption } from 'echarts';
import { map } from 'lodash';
import {
  OverviewWrap, SprintEmptyPage, LoadingBar, useProjectOverviewStore,
} from '@choerodon/master';
import { usePersonalWorkloadStore } from './stores';
import './index.less';

const PersonalWorkload = () => {
  const clsPrefix = 'c7n-project-overview-personalWorkload-chart';
  const {
    startSprintDs,
    startedRecord,
  } = useProjectOverviewStore();
  const options = useMemo(() => [{ value: 'issueCount', text: '问题计数' }, { value: 'workTime', text: '工时' }], []);
  const [chartOption, setChartOption] = useState('issueCount');
  const {
    workloadChartDs,
  } = usePersonalWorkloadStore();
  const renderTitle = () => (
    <div className={`${clsPrefix}-title`}>
      <span>个人工作量统计</span>
      {/* {startedRecord && workloadChartDs.length ? <OverviewWrap.Switch defaultValue="issueCount" onChange={setChartOption} options={options} /> : ''} */}
    </div>
  );
  const getCategoryAndCategoryCount = useCallback(() => {
    const personalWorkloadData = workloadChartDs.toData();
    const data: {undone: number, done: number, total: number}[] = [];
    const xAxisData: string[] = [];
    personalWorkloadData?.forEach((item: {
      remainingIssueCount: number | null,
      remainingTime: number | null,
      issueCount: number | null,
      totalRemainingTime: number | null,
      assigneeRealName: string,
    }) => {
      const undoneCount = chartOption === 'issueCount' ? item.remainingIssueCount || 0 : item.remainingTime || 0;
      const doneCount = chartOption === 'issueCount' ? (item.issueCount || 0) - (item.remainingIssueCount || 0) : (item.totalRemainingTime || 0) - (item.remainingTime || 0);
      const personalNum = { undone: undoneCount, done: doneCount, total: undoneCount + doneCount };
      xAxisData.push(item.assigneeRealName);
      data.push(personalNum);
    });
    console.log(data, xAxisData);
    return { data, xAxisData };
  }, [chartOption, workloadChartDs]);

  const getOption = useCallback(() => {
    const { xAxisData, data } = getCategoryAndCategoryCount();
    const undoneCountArr = map(data, 'undone');
    const doneCountArr = map(data, 'done');
    const totalCountArr = map(data, 'total');

    const option: EChartOption = {
      tooltip: {
        trigger: 'axis',
        axisPointer: { // 坐标轴指示器，坐标轴触发有效
          type: 'shadow', // 默认为直线，可选为：'line' | 'shadow'
        },
        backgroundColor: '#fff',
        textStyle: {
          color: 'rgba(0,0,0,0.64)',
        },
        formatter: (params: EChartOption.Tooltip.Format[]) => {
          let content = '';
          const unit = chartOption === 'issueCount' ? ' 个' : ' 小时';
          params.forEach((item) => {
            content = `<div>
            <span>${params[0].axisValue}</span>
            <br />
            <div style="font-size: 11px"><div style="display:inline-block; width: 10px; height: 10px; margin-right: 3px; border-radius: 50%; background: #C0CBD4;"></div>未完成：${undoneCountArr[item.dataIndex || 0]} ${undoneCountArr[item.dataIndex || 0] ? unit : ''}</div>
            <div style="font-size: 11px"><div style="display:inline-block; width: 10px; height: 10px; margin-right: 3px; border-radius: 50%; background: #00D2C1;"></div>已完成：${doneCountArr[item.dataIndex || 0]} ${doneCountArr[item.dataIndex || 0] ? unit : ''}</div>
          </div>`;
          });
          return content;
        },
      },
      legend: {
        data: ['未完成', '已完成'],
        itemWidth: 20,
        itemHeight: 10,
        itemGap: 14,
        icon: 'rect',
        right: '10px',
        top: 0,
      },
      grid: {
        left: 10,
        top: 40,
        right: '10px',
        // right: '28%',
        bottom: data.length > 8 ? 34 : 14, // 14
        containLabel: true,
      },
      xAxis: {
        type: 'category',
        data: xAxisData,
        axisLabel: {
          margin: 15,
          color: 'rgba(0,0,0,0.87)',
        },
        axisLine: {
          lineStyle: {
            opacity: 0,
          },
        },
        axisTick: {
          lineStyle: {
            color: 'transparent',
          },
        },
      },
      yAxis: {
        name: chartOption === 'issueCount' ? '问题计数' : '工时',
        minInterval: 1,
        nameTextStyle: {
          color: 'var(--text-color3)',
        },
        type: 'value',
        splitLine: {
          // show: true,
          //  改变轴线颜色
          lineStyle: {
            // 使用深浅的间隔色
            color: 'rgba(0,0,0,0.12)',
          },
        },
        nameGap: 20,
        axisLine: {
          lineStyle: {
            opacity: 0,
          },
        },
        axisTick: {
          lineStyle: {
            color: 'transparent',
          },
        },
        axisLabel: {
          color: 'rgba(0,0,0,0.87)',
        },
      },
      series: [
        {
          name: '已完成',
          type: 'bar',
          stack: '计数',
          data: doneCountArr,
          itemStyle: {
            color: '#00D2C1',
          },
          barWidth: 30,
          // barMinHeight: 18,
          label: {
            show: true,
            color: 'rgba(255, 255, 255, 0.87)',
            fontWeight: 500,
            fontSize: 13,
            lineHeight: 18,
          },
        },
        {
          name: '未完成',
          type: 'bar',
          stack: '计数',
          data: undoneCountArr,
          itemStyle: {
            color: '#C0CBD4',
          },
          barWidth: 30,
          // barMinHeight: 18,
          label: {
            show: true,
            color: 'rgba(255, 255, 255, 0.87)',
            fontWeight: 500,
            fontSize: 13,
            lineHeight: 18,
          },
        },
        {
          name: '总共',
          type: 'bar',
          data: totalCountArr,
          itemStyle: {
            color: 'rgba(0, 0, 0, 0.001)',
          },
          barWidth: 30,
          label: {
            position: 'top',
            show: true,
            color: 'var(--text-color)',
            fontWeight: 500,
            fontSize: 13,
            lineHeight: 18,
          },
          barGap: '-100%',
        },
      ],
      dataZoom: [{
        bottom: 0,
        show: data.length > 12,
        type: 'slider',
        // @ts-ignore
        height: 15,
        width: '85%',
        left: 60,
        startValue: 0,
        endValue: 11,
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
    return option;
  }, [chartOption, getCategoryAndCategoryCount]);

  function getContent() {
    if (startSprintDs.status === 'loading') {
      return <LoadingBar display />;
    }
    if (!startedRecord) {
      return <SprintEmptyPage />;
    }
    const { xAxisData, data } = getCategoryAndCategoryCount();

    if (!xAxisData.length || !data.length) {
      return <SprintEmptyPage content="当前暂无数据" />;
    }
    return <Echart option={getOption()} style={{ height: '100%' }} />;
  }

  return (
    <OverviewWrap style={{ height: '100%', width: '100%' }}>
      <OverviewWrap.Header
        title={renderTitle()}
        style={{
          margin: '0 0 10px 4px',
        }}
      />
      {getContent()}
    </OverviewWrap>
  );
};

export default observer(PersonalWorkload);
