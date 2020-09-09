// @ts-nocheck
import React from 'react';
import { Tooltip } from 'choerodon-ui';
import echarts from 'echarts/lib/echarts';
import ReactEchartsCore from 'echarts-for-react/lib/core';
import Loading from '@/components/Loading';
import to, { linkUrl } from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import styles from './index.less';

export interface PieChartProps {
  loading: boolean,
  sourceData: object[],
}

function compare(pro) {
  return function (obj1, obj2) {
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

function getQueryString(type, value) {
  const QUERY = {
    assignee: 'paramType=assigneeId&paramId=',
    component: 'paramType=component&paramId=',
    typeCode: 'paramType=typeCode&paramId=',
    version: 'paramType=fixVersion&paramId=',
    priority: 'paramType=priority&paramId=',
    status: 'paramType=statusId&paramId=',
    sprint: 'paramType=sprint&paramId=',
    epic: 'paramType=epic&paramId=',
    label: 'paramType=label&paramId=',
  };
  if (!QUERY[type]) return null;
  return `${QUERY[type]}${value === null ? '0' : value}`;
}

const PieChart:React.FC<PieChartProps> = ({ loading, sourceData }) => {
  const renderOtherTooltip = () => {
    const otherDates = sourceData.filter((item) => item.percent < 2).sort(compare('percent'));
    if (otherDates && otherDates.length > 0) {
      if (otherDates.length <= 6) {
        return (
          otherDates.map((item) => (
            <div className="pie-otherTooptip-item">
              <p className="pie-otherTooptip-item-percent">
                <span>{`${item.percent.toFixed(2)}%`}</span>
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
          {otherDates.slice(0, 6).map((item) => (
            <div className="pie-otherTooptip-item">
              <p className="pie-otherTooptip-item-percent">
                <span>{`${item.percent.toFixed(2)}%`}</span>
              </p>
              <p>
                <Tooltip title={item.name} placement="bottom">
                  <span>{item.realName ? item.realName : item.name}</span>
                </Tooltip>
              </p>
            </div>
          ))}
          <div className="pie-otherTooptip-item">
            <span className="pie-otherTooptip-item-ignore">...</span>
          </div>
        </>
      );
    }
    return '';
  };

  const getCurrentChoose = () => {
    const {
      currentChooseDimension, currentSprintChoose, currentVersionChoose,
    } = this.state;
    const CHOOSEQUERY = {
      sprint: { paramChoose: 'sprint', paramCurrentSprint: currentSprintChoose },
      version: { paramChoose: 'version', paramCurrentSprint: currentVersionChoose },
    };
    return currentChooseDimension ? CHOOSEQUERY[currentChooseDimension] : ({});
  };

  const handleLinkToIssue = (item) => {
    const {
      value, sprintAndVersion, currentChooseDimension,
      currentSprintChoose, currentVersionChoose,
    } = this.state;
    const { typeName, name } = item;
    const queryString = getQueryString(value, typeName);
    const queryObj = getCurrentChoose();
    let paramName = name || '未分配';
    if (currentChooseDimension === 'sprint') {
      paramName += `、冲刺为${sprintAndVersion.sprint.find((sprintItem) => sprintItem.sprintId === currentSprintChoose).sprintName}`;
    }

    if (currentChooseDimension === 'version') {
      paramName += `、版本为${sprintAndVersion.version.find((versionItem) => versionItem.versionId === currentVersionChoose).name}`;
    }

    paramName += '下的问题';

    if (!queryString) return;
    to(LINK_URL.workListIssue, {
      params: {
        paramName,
        ...queryObj,
      },
    });
  };

  const getOption = () => {
    const { colors } = VersionReportStore;
    const datas = VersionReportStore.pieData;
    return {
      color: colors,
      tooltip: {
        trigger: 'item',
        formatter: (value) => {
          if (value.data.name !== '其它') {
            if (this.otherTooltipRef && this.otherTooltipRef.current) {
              this.otherTooltipRef.current.style.display = 'none';
            }
            return `<div><span>问题：${value.data.value} 个</span><br/><span>百分比：${(value.data.percent.toFixed(2))}%</span></div>`;
          }
          if (this.otherTooltipRef && this.otherTooltipRef.current) {
            this.otherTooltipRef.current.style.display = 'block';
            const otherTooptipItem = document.getElementsByClassName('pie-otherTooptip-item-percent');
            let opacity = 0.9;
            for (let i = 0; i < otherTooptipItem.length; i += 1) {
              opacity = 1 - i * 0.1 > 0 ? 1 - i * 0.1 : 0.9;
              otherTooptipItem[i].style.backgroundColor = `rgba(250,211,82,${opacity})`;
            }
          }
          return '';
        },
        padding: 10,
        textStyle: {
          color: '#000',
          fontSize: 12,
          lineHeight: 20,
        },
        extraCssText: 'background: #FFFFFF;\n'
          + 'border: 1px solid #DDDDDD;\n'
          + 'box-shadow: 0 2px 4px 0 rgba(0,0,0,0.20);\n'
          + 'border-radius: 0',
      },
      series: [
        {
          name: '',
          type: 'pie',
          startAngle: 245,
          center: ['50%', '47%'],
          data: datas,
          label: {
            color: 'rgba(0,0,0,0.65)',
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
    <>
      <Loading loading={loading} />
      <div
        className={styles.pie_chart}
      >
        <ReactEchartsCore
          style={{ width: '58%', height: 500 }}
          echarts={echarts}
          option={getOption()}
        />

        <div className="pie-otherTooltip" ref={this.otherTooltipRef} style={{ display: 'none' }}>
          <div className="pie-otherTooltip-wrap" />
          <div className="pie-otherTooltip-item-wrap">
            {renderOtherTooltip()}
          </div>

        </div>
        <div className="pie-title">
          <p className="pie-legend-title">数据统计</p>
          <table>
            <thead>
              <tr>
                <td style={{ width: '158px' }}>{this.state.type}</td>
                <td style={{ width: '62px' }}>问题</td>
                <td style={{ paddingRight: 35 }}>百分比</td>
              </tr>
            </thead>
          </table>
          <table className="pie-legend-tbody">
            {
            sourceData.map((item, index) => (
              <tr>
                <td style={{ width: '158px' }}>
                  <div className="pie-legend-icon" style={{ background: colors[index] }} />
                  <Tooltip title={item && item.name}>
                    <div className="pie-legend-text">{item.name ? (item.realName || item.name) : '未分配'}</div>
                  </Tooltip>
                </td>
                <td style={{ width: '62px' }}>
                  <a
                    role="none"
                    onClick={handleLinkToIssue.bind(this, item)}
                  >
                    {item.value}
                  </a>
                </td>
                <td style={{ width: '62px', paddingRight: 15 }}>{`${(item.percent).toFixed(2)}%`}</td>
              </tr>
            ))
          }
          </table>
        </div>
      </div>
    </>
  );
};

export default PieChart;
