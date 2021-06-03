import React from 'react';
import { Spin } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import ReactEcharts from 'echarts-for-react';
import echarts, { EChartOption } from 'echarts';
import { useFontSize } from '../context';

export interface CodeQualityVaryData {
  sonarIssueHistoryDetails: {
    name: string
    data: number[]
  }[]
  date: string[]
  legend: string[]
}
export interface CodeQualityVaryProps {
  data: CodeQualityVaryData
  loading: boolean
  option?: EChartOption
}
const colorObj: { [name: string]: string } = {
  代码缺陷: '#F48590',
  安全漏洞: '#6887E8',
  代码异味: '#514FA0',
  技术债务: '#CACAE4',
  重复部分: '#FFB96A',
  单元测试: '#6887E8',
};
function judgeStyle(item: {
  name: string;
  data: (string | number)[][]
}) {
  switch (item.name) {
    case '代码缺陷':
      return {
        normal: {
          color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [{
            offset: 0,
            color: 'rgba(244, 133, 144, 0.3)',
          }, {
            offset: 1,
            color: 'rgba(244, 133, 144, 0)',
          }]),
        },
      };
    case '安全漏洞':
      return {
        normal: {
          color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [{
            offset: 0,
            color: 'rgba(80, 107, 255, 0.3)',
          }, {
            offset: 1,
            color: 'rgba(82, 102, 212, 0)',
          }]),
        },
      };
    case '代码异味':
      return {
        normal: {
          color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [{
            offset: 0,
            color: 'rgba(81, 79, 160, 0.2)',
          }, {
            offset: 1,
            color: 'rgba(81, 79, 160, 0)',
          }]),
        },
      };
    case '技术债务':
      return {
        ...item,
        normal: {
          color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [{
            offset: 0,
            color: 'rgba(202, 202, 228, 0.3)',
          }, {
            offset: 1,
            color: 'rgba(202, 202, 228, 0)',
          }]),
        },
      };
    case '重复部分':
      return {
        ...item,
        normal: {
          color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [{
            offset: 0,
            color: 'rgba(255, 185, 106, 0.3)',
          }, {
            offset: 1,
            color: 'rgba(255, 185, 106, 0)',
          }]),
        },
      };
    case '单元测试':
      return {
        normal: {
          color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [{
            offset: 0,
            color: 'rgba(104, 135, 232, 0.2)',
          }, {
            offset: 1,
            color: 'rgba(104, 135, 232, 0)',
          }]),
        },
      };
    default:
      return {
      };
  }
}
const CodeQualityVary: React.FC<CodeQualityVaryProps> = ({
  loading, data, option,
}) => {
  const getFontSize = useFontSize();
  const FontSize = getFontSize(12);
  const { date = [], sonarIssueHistoryDetails = [], legend = [] } = data;
  const getOption = (): EChartOption => {
    const handledDetails = sonarIssueHistoryDetails.map((item) => ({
      name: item.name,
      data: item.data.map((i, index) => [date[index], i]),
    }));
    return ({
      textStyle: {
        fontSize: FontSize,
      },
      legend: {
        data: legend,
        icon: 'circle',
        right: 0,
        textStyle: {
          fontSize: 13,
          fontWeight: 400,
          color: 'rgba(58, 52, 95, 0.65)',
        },
      },
      grid: {
        top: '30px',
        left: 0,
        bottom: 24,
        right: '50px',
        containLabel: true,
      },
      dataZoom: [
        {
          startValue: date[0],
        },
        {
          type: 'inside',
        },
      ],
      tooltip: {
        trigger: 'axis',
        confine: true,
        formatter(params: any) {
          return `
            日期: ${params[0].value[0]}</br>
            ${params.map((item: any) => `${item.seriesName}: ${item.value[1]}个</br>`).join('')}
          `;
        },
        backgroundColor: 'rgba(0,0,0,0.75)',
        extraCssText: 'box-shadow: 0px 2px 8px 0px rgba(0,0,0,0.12)',
        textStyle: {
          fontSize: 13,
          color: 'rgba(255,255,255,1)',
        },
      },
      xAxis: {
        type: 'time',
        // data: date,
        nameLocation: 'end',
        nameTextStyle: {
          color: 'rgba(0,0,0,1)',
          fontSize: 13,
          fontWeight: 400,

        },
        boundaryGap: false,
        axisTick: { show: false },
        axisLine: {
          lineStyle: {
            color: '#eee',
            type: 'solid',
            width: 2,
          },
          onZero: true,
        },
        axisLabel: {
          margin: 15, // X轴文字和坐标线之间的间距
          color: 'var(--text-color3)',
          fontSize: 12,
        },
        splitLine: {
          show: true,
          lineStyle: {
            color: '#eee',
            width: 1,
            type: 'solid',
          },
        },
      },
      yAxis: {
        nameTextStyle: {
          color: 'rgba(0,0,0,1)',
          fontSize: 13,
        },
        name: '数量',
        type: 'value',
        axisLabel: { color: 'rgba(0,0,0,0.65)' },
        axisLine: {
          lineStyle: {
            color: '#EEEEEE',
          },
        },
      },
      color: legend.map((e) => colorObj[e]),
      series: handledDetails.map((e) => (
        {
          data: e.data,
          type: 'line',
          name: e.name,
          lineStyle: {
            width: 3,
          },
          smooth: 0.3,
          smoothMonotone: 'x',
          symbol: 'circle',
          areaStyle: judgeStyle(e),
        }
      )),
      ...option,
    });
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

export default observer(CodeQualityVary);
