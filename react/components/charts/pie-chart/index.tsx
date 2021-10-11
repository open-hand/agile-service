/* eslint-disable no-param-reassign */
import React, { useRef } from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import { EChartOption } from 'echarts/lib/echarts';
import ReactEcharts from 'echarts-for-react';
import { reduce, filter } from 'lodash';
import Loading from '@/components/Loading';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import { Priority, ISprint, IVersion } from '@/common/types';
import { types } from './search';
import styles from './index.less';
import { useFontSize } from '../context';

export type IPieChartType = 'assignee' | 'component' | 'typeCode' | 'version' | 'priority' | 'status' | 'sprint' | 'epic' | 'label'

export type IDimension = 'version' | 'sprint' | 'status' | '';

export interface IPieData {
  jsonObject?: null
  loginName?: null | string
  name: null | string
  percent: number
  priorityVO?: null | Priority
  realName?: null | string
  typeName: null | string
  value: number
}
export interface PieChartProps {
  loading: boolean,
  type: IPieChartType,
  data: IPieData[],
  colors: string[],
  chooseDimension: IDimension,
  chooseId: '' | string,
  sprints: ISprint[],
  versions: IVersion[],
  option?: EChartOption
  link?: boolean
}

function compare(pro: string) {
  return (obj1: any, obj2: any) => {
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

const PieChart: React.FC<PieChartProps> = ({
  loading, data, colors, chooseDimension, chooseId, type, sprints, versions, link = true, option,
}) => {
  const otherTooltipRef = useRef();
  const getFontSize = useFontSize();
  const FontSize = getFontSize(12);
  const renderOtherTooltip = () => {
    const otherDates = data.filter((item) => item.percent < 2).sort(compare('percent'));
    if (otherDates && otherDates.length > 0) {
      if (otherDates.length <= 6) {
        return (
          otherDates.map((item, i) => (
            <div className={styles.pie_otherTooltip_item}>
              <p
                className={styles.pie_otherTooltip_item_percent}
                style={{
                  background: `rgba(250,211,82,${1 - i * 0.1 > 0 ? 1 - i * 0.1 : 0.9})`,
                }}
              >
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
          {otherDates.slice(0, 6).map((item, i) => (
            <div className={styles.pie_otherTooltip_item}>
              <p
                className={styles.pie_otherTooltip_item_percent}
                style={{
                  background: `rgba(250,211,82,${1 - i * 0.1 > 0 ? 1 - i * 0.1 : 0.9})`,
                }}
              >
                <span>{`${item.percent.toFixed(2)}%`}</span>
              </p>
              <p>
                <Tooltip title={item.name} placement="bottom">
                  <span>{item.realName ? item.realName : item.name}</span>
                </Tooltip>
              </p>
            </div>
          ))}
          <div className={styles.pie_otherTooltip_item}>
            <span className={styles.pie_otherTooltip_item_ignore}>...</span>
          </div>
        </>
      );
    }
    return '';
  };

  const getCurrentChoose = () => {
    const CHOOSEQUERY = {
      sprint: { paramChoose: 'sprint', paramCurrentSprint: chooseId },
      version: { paramChoose: 'version', paramCurrentVersion: chooseId },
      status: { paramChoose: 'status', paramCurrentStatus: chooseId },
    };
    return chooseDimension ? CHOOSEQUERY[chooseDimension] : ({});
  };

  const handleLinkToIssue = (item: any) => {
    const { typeName, name } = item;
    const queryObj = getCurrentChoose();
    let paramName = name || '未分配';
    if (chooseDimension === 'sprint') {
      paramName += `、冲刺为${sprints.find((sprint: ISprint) => sprint.sprintId === chooseId)?.sprintName}`;
    }

    if (chooseDimension === 'version') {
      paramName += `、版本为${versions.find((version: IVersion) => version.versionId === chooseId)?.name}`;
    }

    paramName += '下的工作项';

    let paramType: string = type;
    if (type === 'typeCode') {
      paramType = 'issueTypeId';
    } else if (type === 'priority') {
      paramType = 'priorityId';
    } else if (type === 'status') {
      paramType = 'statusId';
    } else if (type === 'assignee') {
      paramType = 'assigneeId';
    }
    to(LINK_URL.workListIssue, {
      type: 'project',
      params: {
        paramName,
        paramType,
        paramId: typeName === null ? '0' : typeName,
        ...queryObj,
      },
    }, { blank: true });
  };

  const getOption = (): EChartOption => {
    const pieData = data.filter((item) => item.percent >= 2);
    const otherData = {
      name: '其它',
      typeName: null,
      // eslint-disable-next-line no-return-assign
      value: reduce(filter(data, (item) => item.percent < 2), (sum, item) => sum += item.value, 0),
      // eslint-disable-next-line no-return-assign
      percent: Number(reduce(filter(data, (item) => item.percent < 2), (sum, item) => sum += item.percent, 0).toFixed(2)),
    };
    if (otherData.value > 0) {
      pieData.push(otherData);
    }

    return {
      textStyle: {
        fontSize: FontSize,
      },
      color: colors,
      tooltip: {
        trigger: 'item',
        formatter: (value: any) => {
          if (value.data.name !== '其它') {
            if (otherTooltipRef && otherTooltipRef.current) {
              // @ts-ignore
              otherTooltipRef.current.style.display = 'none';
            }
            return `<div><span>工作项：${value.data.value} 个</span><br/><span>百分比：${(value.data.percent.toFixed(2))}%</span></div>`;
          }
          // @ts-ignore
          if (otherTooltipRef && otherTooltipRef.current) {
            // @ts-ignore
            otherTooltipRef.current.style.display = 'block';
          }
          return '';
        },
        padding: 10,
        textStyle: {
          color: 'var(--text-color)',
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
          // @ts-ignore
          data: pieData,
          label: {
            color: 'var(--text-color3)',
            position: 'outside',
            formatter: (value: any) => {
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
      ...option,
    };
  };

  const currentType = types.find((item) => item.value === type) || { title: '' };

  return (
    <>
      <Loading loading={loading} />
      <div
        className={styles.pie_chart}
      >
        <div style={{
          position: 'relative', flex: 1,
        }}
        >
          <ReactEcharts
            style={{ height: 500 }}
            option={getOption()}
          />
          <div
            className={styles.pie_otherTooltip}
            // @ts-ignore
            ref={otherTooltipRef}
            style={{ display: 'none' }}
          >
            <div className={styles.pie_otherTooltip_wrap} />
            <div className={styles.pie_otherTooltip_item_wrap}>
              {renderOtherTooltip()}
            </div>
          </div>
        </div>

        <div className={styles.pie_legend}>
          <p className={styles.pie_legend_title}>数据统计</p>
          <table style={{ tableLayout: 'fixed', width: '100%' }}>
            <thead>
              <tr>
                <td>{currentType.title}</td>
                <td>工作项</td>
                <td style={{ paddingRight: 35 }}>百分比</td>
              </tr>
            </thead>
            <tbody className={styles.pie_legend_tbody}>
              {
                data.map((item, index) => (
                  <tr>
                    <td style={{ display: 'flex', alignItems: 'center' }}>
                      <div className={styles.pie_legend_icon} style={{ background: colors[index] }} />
                      <Tooltip title={item && item.name}>
                        <div className={styles.pie_legend_text}>{item.name ? (item.realName || item.name) : '未分配'}</div>
                      </Tooltip>
                    </td>
                    <td>
                      {link ? (
                        <span
                          style={{
                            color: '#5365EA',
                            cursor: 'pointer',
                          }}
                          role="none"
                          onClick={handleLinkToIssue.bind(this, item)}
                        >
                          {item.value}
                        </span>
                      ) : item.value}
                    </td>
                    <td style={{ paddingRight: 15 }}>{`${(item.percent).toFixed(2)}%`}</td>
                  </tr>
                ))
              }
            </tbody>
          </table>
        </div>
      </div>
    </>
  );
};

export default PieChart;
