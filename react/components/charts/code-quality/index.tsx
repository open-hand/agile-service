import React, { useMemo } from 'react';
import { Tooltip, Spin } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import ReactEcharts from 'echarts-for-react';

import { EChartOption } from 'echarts';
import { useFontSize } from '../context';
import styles from './index.less';

// eslint-disable-next-line no-bitwise
const toFixed = (n: number, fixed: number) => ~~(Math.pow(10, fixed) * n) / Math.pow(10, fixed);
export interface CodeQualityData {
  series: {
    name: string
    value: number
  }[]
  legend: string[]
}
export interface CodeQualityProps {
  data: CodeQualityData
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

const CodeQuality: React.FC<CodeQualityProps> = ({
  loading, data, option,
}) => {
  const getFontSize = useFontSize();
  const FontSize = getFontSize(12);
  const { series = [], legend = [] } = data;
  const total: number = useMemo(() => series.reduce((result, current) => result + current.value, 0), [series]);
  const percents = useMemo(() => series.map((item) => toFixed((item.value / total) * 100, 2)), [series, total]);
  const getOption = (): EChartOption => ({
    textStyle: {
      fontSize: FontSize,
    },
    // 提示框
    tooltip: {
      confine: true,
      trigger: 'item', // 触发类型： 图像，坐标轴
      formatter: '{b} : {c} ({d}%)', // 提示的模板  在饼图中： {a}（系列名称），{b}（数据项名称），{c}（数值）, {d}（百分比）
    },
    color: series.map((e) => colorObj[e.name as string]),
    series: [
      {
        type: 'pie',
        center: ['50%', '47%'],
        minAngle: 5,
        data: series,
        emphasis: {
          itemStyle: {
            shadowBlur: 10, // 图形阴影的模糊的级数，0 表示不模糊
            shadowOffsetX: 0, // 阴影水平方向上的偏移距离
            shadowColor: 'rgba(0, 0, 0, 0.5)', // 阴影颜色
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
        <div className={styles.pie_legend}>
          <p className={styles.pie_legend_title}>数据统计</p>
          <table>
            <thead>
              <tr>
                <td style={{ width: '280px' }}>类型</td>
                <td style={{ width: '150px' }}>数量</td>
                <td style={{ paddingRight: 35 }}>百分比</td>
              </tr>
            </thead>
          </table>
          <table className={styles.pie_legend_tbody}>
            {
              legend.map((item, index) => (
                <tr>
                  <td style={{ width: '280px', display: 'flex', alignItems: 'center' }}>
                    <div className={styles.pie_legend_icon} style={{ background: colorObj[item] }} />
                    <Tooltip title={item}>
                      <div className={styles.pie_legend_text}>{item}</div>
                    </Tooltip>
                  </td>
                  <td style={{ width: '150px' }}>
                    {series[index].value}
                  </td>
                  <td style={{ width: '150px', paddingRight: 15 }}>{`${(percents[index])}%`}</td>
                </tr>
              ))
            }
          </table>
        </div>
      </Spin>
    </div>
  );
};

export default observer(CodeQuality);
