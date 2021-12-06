// 进度与效率-饼图
import React, {
  useContext, useEffect, useState, useRef, useMemo,
} from 'react';
import {
  observer,
} from 'mobx-react-lite';
import {
  Spin, Tooltip, Form, Select,
} from 'choerodon-ui/pro';
import { reduce, filter } from 'lodash';
import ReactEchartsCore from 'echarts-for-react/lib/core';
import echarts from 'echarts';
import EmptyBlock from '../EmptyBlock';
import SwitchTabs from '../SwitchTabs';
import Store from '../../stores';
import './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

function compare(pro) {
  return (obj1, obj2) => {
    const val1 = obj1[pro];
    const val2 = obj2[pro];
    if (val1 < val2) {
      return 1;
    } if (val1 > val2) {
      return -1;
    }
    return 0;
  };
}

const PieChart = observer(() => {
  const { dimensionDS, efficiencyStoryDS, efficiencyTaskDS } = useContext(Store);
  const currentTabKey = dimensionDS.current.get('tab');
  const formatMessage = useFormatMessage();
  const currentTab = useMemo(() => {
    const code = dimensionDS.getField('tab').getOptions().find((item) => item.get('value') === currentTabKey)?.get('code');
    return code ? formatMessage({ id: code }) : dimensionDS.getField('tab').getText(currentTabKey);
  }, [currentTabKey, dimensionDS, formatMessage]);

  const [pieData, setPieData] = useState(null);
  const [colorsArr, setColorsArr] = useState([]);
  const [isLoading, setIsLoading] = useState(true);
  const otherTooltipRef = useRef();

  useEffect(() => {
    queryPieData();
  }, []);

  const queryPieData = async () => {
    setIsLoading(true);
    const currentDataSet = dimensionDS.current.get('tab') === 'STORY' ? efficiencyStoryDS : efficiencyTaskDS;
    await currentDataSet.query();
    const { plan = [], complete = [], colors = [] } = currentDataSet.current.toData();
    const dimension = dimensionDS.current.get('dimension');
    const currentData = dimension.toLowerCase() === 'plan' ? plan : complete;
    setPieData(currentData);
    setColorsArr(colors);
    setIsLoading(false);
  };

  /**
   * 切换维度
   * @param {string} value - 维度值
   */
  const handleChangeDimension = (value) => {
    const data = dimensionDS.current.get('tab') === 'STORY' ? efficiencyStoryDS.current.toData() : efficiencyTaskDS.current.toData();
    const { plan = [], complete = [] } = data;
    if (value === 'PLAN') {
      setPieData(plan);
    } else {
      setPieData(complete);
    }
  };

  const renderOtherTooltip = () => {
    const otherDates = pieData.filter((item) => item.percent < 2).sort(compare('percent'));
    if (otherDates && otherDates.length > 0) {
      if (otherDates.length <= 6) {
        return (
          otherDates.map((item, i) => (
            <div className="pie-otherTooltip-item">
              <p
                className="pie-otherTooltip-item-percent"
                style={{
                  background: `rgba(250,211,82,${1 - i * 0.1 > 0 ? 1 - i * 0.1 : 0.9})`,
                }}
              >
                <span>{`${item.percent}%`}</span>
              </p>
              <p>
                <Tooltip title={item.name} placement="bottom">
                  <span>{item.realName ? item.realName : item.name}</span>
                </Tooltip>
              </p>
            </div>
          ))
        );
      }
      return (
        <>
          {otherDates.slice(0, 6).map((item, i) => (
            <div className="pie-otherTooltip-item">
              <p
                className="pie-otherTooltip-item-percent"
                style={{
                  background: `rgba(250,211,82,${1 - i * 0.1 > 0 ? 1 - i * 0.1 : 0.9})`,
                }}
              >
                <span>{`${item.percent}%`}</span>
              </p>
              <p>
                <Tooltip title={item.name} placement="bottom">
                  <span>{item.realName ? item.realName : item.name}</span>
                </Tooltip>
              </p>
            </div>
          ))}
          <div className="pie-otherTooltip-item">
            <span className="pie-otherTooltip-item-ignore">...</span>
          </div>
        </>
      );
    }
    return '';
  };

  const getOpts = () => {
    const nextPieData = pieData.filter((item) => item.percent >= 2);
    const otherData = {
      name: '其它',
      typeName: null,
      // eslint-disable-next-line
      value: reduce(filter(pieData, (item) => item.percent < 2), (sum, item) => sum += item.value, 0),
      // eslint-disable-next-line
      percent: Number(reduce(filter(pieData, (item) => item.percent < 2), (sum, item) => sum += item.percent, 0).toFixed(2)),
    };
    if (otherData.value > 0) {
      nextPieData.push(otherData);
    }

    return {
      textStyle: {
        fontSize: 12,
      },
      color: colorsArr,
      tooltip: {
        trigger: 'item',
        formatter: (value) => {
          if (value.name !== '其它') {
            if (otherTooltipRef && otherTooltipRef.current) {
              otherTooltipRef.current.style.display = 'none';
            }
            return `<div><span>${currentTab}：${value.value} 个</span><br/><span>百分比：${(value.percent)}%</span></div>`;
          }
          if (otherTooltipRef && otherTooltipRef.current) {
            otherTooltipRef.current.style.display = 'block';
          }
          return '';
        },
        padding: 10,
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
      series: [
        {
          name: '',
          type: 'pie',
          startAngle: 245,
          center: ['50%', '47%'],
          data: nextPieData,
          label: {
            color: 'var(--text-color3)',
            position: 'outside',
            formatter: (value) => {
              if (value.data.name === null) {
                return '未分配';
              }
              return value.data.name;
            },
          },
          itemStyle: {
            normal: {
              borderWidth: 2,
              borderColor: '#ffffff',
            },
          },
        },
      ],
    };
  };

  return (
    <Spin spinning={isLoading}>
      <div className="chart-container chart-container-pie">
        <div className="chart-handle">
          <SwitchTabs
            dataSet={dimensionDS}
            onChange={() => queryPieData()}
            style={{ flexShrink: 0, height: '37px' }}
          />
          <Form dataSet={dimensionDS} style={{ marginLeft: '15px' }}>
            <Select name="dimension" clearButton={false} style={{ width: '130px' }} onChange={handleChangeDimension} />
          </Form>
        </div>
        {
          (pieData && pieData.length > 0) && (
            <div className="pie-chart-container">
              <div className="pie-chart">
                <div className="pie-chart-main">
                  <span className="chart-title">{formatMessage({ id: 'agile.performance.distribution' }, { name: currentTab })}</span>
                  <ReactEchartsCore
                    echarts={echarts}
                    option={getOpts()}
                    notMerge
                    lazyUpdate
                    style={{
                      height: 360,
                    }}
                  />
                </div>
                <div
                  className="pie-otherTooltip"
                  ref={otherTooltipRef}
                  style={{ display: 'none' }}
                >
                  <div className="pie-otherTooltip-wrap" />
                  <div className="pie-otherTooltip-item-wrap">
                    {renderOtherTooltip()}
                  </div>
                </div>
              </div>
              <div className="pie-list">
                <p className="pie-legend-title">数据统计</p>
                <table>
                  <thead>
                    <tr>
                      <td style={{ width: '166px' }}>主要负责人</td>
                      <td style={{ width: '104px' }}>{currentTab}</td>
                      <td style={{ paddingRight: 38 }}>百分比</td>
                    </tr>
                  </thead>
                </table>
                <table className="pie-legend-tbody">
                  {
                    pieData.map((item, index) => (
                      <tr>
                        <td style={{ width: '166px', display: 'flex', alignItems: 'center' }}>
                          <div className="pie-legend-icon" style={{ background: colorsArr[index] }} />
                          <Tooltip title={item && item.name}>
                            <div className="pie-legend-text">{item.name ? (item.realName || item.name) : '未分配'}</div>
                          </Tooltip>
                        </td>
                        <td style={{ width: '104px' }}>
                          {item.value}
                        </td>
                        <td style={{ width: '75px', paddingRight: 15 }}>{`${(item.percent)}%`}</td>
                      </tr>
                    ))
                  }
                </table>
              </div>
            </div>
          )
        }
        {
          (Array.isArray(pieData) && pieData.length === 0) && (
            <EmptyBlock
              height={450}
              des="当前暂无数据"
            />
          )
        }
      </div>
    </Spin>
  );
});

export default PieChart;
