import React, { Component } from 'react';
import { observer } from 'mobx-react';
import ReactEcharts from 'echarts-for-react';
import {
  Page, Header, Content, stores, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import {
  Tabs, Table, Icon, Tooltip, Spin,
} from 'choerodon-ui';
import { Form, Select } from 'choerodon-ui/pro';
import { EmptyPage } from '@choerodon/components';
import STATUS from '@/constants/STATUS';
import LINK_URL, { LINK_URL_TO } from '@/constants/LINK_URL';
import to from '@/utils/to';
import pic from '../../../../assets/image/NoData.svg';
import finish from './legend/finish.svg';
import total from './legend/total.svg';
import noEstimated from './legend/noEstimated.svg';
import SwithChart from '../../Component/switchChart';
import StatusTag from '../../../../components/StatusTag';
import PriorityTag from '../../../../components/PriorityTag';
import TypeTag from '../../../../components/TypeTag';
import VS from '../../../../stores/project/versionReportNew';
import BackBtn from '../../back-btn';
import './VersionReport.less';
import Loading, { LoadingHiddenWrap, LoadingProvider } from '@/components/Loading';

const { TabPane } = Tabs;
const { AppState } = stores;
const { Option } = Select;

@observer
class EpicReport extends Component {
  componentDidMount() {
    VS.loadEpicAndChartAndTableData();
  }

  getLabel(record) {
    if (VS.beforeCurrentUnit === 'story_point') {
      if (record.issueTypeVO.typeCode === 'story') {
        return record.storyPoints === null ? '未预估' : record.storyPoints;
      }
      return '';
    }
    return record.remainTime === null ? '未预估' : record.remainTime;
  }

  getOption() {
    const UNIT = {
      总工作项数: '个',
      已完成工作项数: '个',
      未预估工作项百分比: '%',
      已完成故事点: '点',
      总计故事点: '点',
      已完成剩余时间: '小时',
      总计剩余时间: '小时',
    };
    const commonOption = {
      tooltip: {
        trigger: 'axis',
        formatter: (params) => {
          let content = '';
          if (params && params.length) {
            content = `<div>${params[0].axisValue}</div>`;
            params.forEach((param) => {
              content += `<div style="font-size: 11px"><div style="display:inline-block; width:10px; height:10px; margin-right: 3px; border-radius: 50%; border-radius: 50%; background: ${param.color}"></div>${param.seriesName}：${param.value}${param.value ? UNIT[param.seriesName] : ''}</div>`;
            });
          }
          return content;
        },
      },
      legend: {
        orient: 'horizontal',
        x: 'center',
        y: 0,
        padding: [0, 50, 0, 0],
        itemWidth: 14,
        data: [
          ...[
            VS.beforeCurrentUnit === 'issue_count' ? {} : {
              name: `总计${VS.getChartYAxisName}`,
              icon: `image://${total}`,
            },
          ],
          ...[
            VS.beforeCurrentUnit === 'issue_count' ? {} : {
              name: `已完成${VS.getChartYAxisName}`,
              icon: `image://${finish}`,
            },
          ],
          ...[
            VS.beforeCurrentUnit === 'issue_count' ? {
              name: '总工作项数',
              icon: `image://${total}`,
            } : {},
          ],
          ...[
            VS.beforeCurrentUnit === 'issue_count' ? {} : {
              name: '未预估工作项百分比',
              icon: `image://${noEstimated}`,
            },
          ],
          ...[
            VS.beforeCurrentUnit === 'issue_count' ? {
              name: '已完成工作项数',
              icon: `image://${finish}`,
            } : {},
          ],
        ],
      },
      grid: {
        y2: 50,
        top: '30',
        left: 0,
        right: '20',
        containLabel: true,
      },
      calculable: true,
      xAxis: {
        // name: '日期',
        type: 'category',
        boundaryGap: false,
        nameLocation: 'end',
        nameGap: -10,
        nameTextStyle: {
          color: 'var(--text-color)',
          // verticalAlign: 'bottom',
          padding: [35, 0, 0, 0],
        },
        axisTick: { show: false },
        axisLine: {
          show: true,
          lineStyle: {
            color: '#eee',
            type: 'solid',
            width: 2,
          },
        },
        axisLabel: {
          show: true,
          interval: VS.getChartDataX.length >= 20 ? 4 : 0,
          margin: 13,
          textStyle: {
            color: 'var(--text-color3)',
            fontSize: 12,
            fontStyle: 'normal',
          },
          formatter(value, index) {
            // return `${value.split('-')[2]}/${MONTH[value.split('-')[1] * 1]}月`;
            return value.slice(5);
          },
        },
        splitArea: {
          show: false,
          interval: 0,
          color: 'rgba(0, 0, 0, 0.16)',
        },
        splitLine: {
          show: true,
          onGap: false,
          interval: 0,
          lineStyle: {
            color: ['#eee'],
            width: 2,
            type: 'solid',
          },
        },
        data: VS.getChartDataX,
      },
      dataZoom: [{
        startValue: VS.getChartDataX[0],
        type: 'slider',
        handleIcon: 'M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z',
        handleSize: '100%',
        handleStyle: {
          color: '#fff',
          shadowBlur: 3,
          shadowColor: 'rgba(0, 0, 0, 0.6)',
          shadowOffsetX: 2,
          shadowOffsetY: 2,
        },
      }],
    };
    let option;
    if (VS.beforeCurrentUnit === 'issue_count') {
      option = {
        yAxis: [
          {
            name: '工作项数',
            nameTextStyle: {
              color: 'var(--text-color)',
            },
            type: 'value',
            minInterval: 1,
            axisTick: { show: false },
            axisLine: {
              show: true,
              lineStyle: {
                color: '#eee',
                type: 'solid',
                width: 2,
              },
            },
            axisLabel: {
              show: true,
              interval: 'auto',
              margin: 18,
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
                width: 2,
              },
            },
          },
        ],
        series: [
          {
            name: '总工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#78aafe',
            },
            areaStyle: {
              color: 'rgba(77, 144, 254, 0.1)',
            },
            data: VS.getChartDataYIssueCountAll,
          },
          {
            name: '已完成工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#00bfa4',
            },
            areaStyle: {
              color: 'rgba(0, 191, 165, 0.1)',
            },
            data: VS.getChartDataYIssueCountCompleted,
          },
          {
            name: '未预估工作项百分比',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#f44336',
            },
            areaStyle: {
              color: 'rgba(244, 67, 54, 0.1)',
            },
            data: VS.getChartDataYIssueCountUnEstimate,
          },
        ],
      };
    } else {
      option = {
        yAxis: [
          {
            name: VS.getChartYAxisName,
            nameTextStyle: {
              color: 'var(--text-color)',
            },
            type: 'value',
            minInterval: 1,
            axisTick: { show: false },
            axisLine: {
              show: true,
              lineStyle: {
                color: '#eee',
                type: 'solid',
                width: 2,
              },
            },
            axisLabel: {
              show: true,
              interval: 'auto',
              margin: 18,
              textStyle: {
                color: 'var(--text-color3)',
                fontSize: 12,
                fontStyle: 'normal',
              },
              formatter(value, index) {
                if (value && VS.beforeCurrentUnit === 'remain_time') {
                  return `${value}h`;
                }
                return value;
              },
            },
            splitLine: {
              show: true,
              lineStyle: {
                color: '#eee',
                type: 'solid',
                width: 2,
              },
            },
          },
          {
            name: '百分比',
            nameTextStyle: {
              color: 'var(--text-color)',
            },
            type: 'value',
            minInterval: 1,
            axisTick: { show: false },
            axisLine: {
              show: true,
              lineStyle: {
                color: '#eee',
                type: 'solid',
                width: 2,
              },
            },
            axisLabel: {
              show: true,
              interval: 'auto',
              margin: 18,
              textStyle: {
                color: 'var(--text-color3)',
                fontSize: 12,
                fontStyle: 'normal',
              },
              // formatter(value, index) {
              //   if (value && VS.beforeCurrentUnit !== 'issue_count') {
              //     return `${value}%`;
              //   }
              //   return value;
              // },
            },
            splitLine: {
              show: false,
            },
          },
        ],
        series: [
          {
            name: '总工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#78aafe',
            },
            areaStyle: {
              color: 'rgba(77, 144, 254, 0.1)',
            },
            yAxisIndex: 1,
            data: VS.getChartDataYIssueCountAll,
          },
          {
            name: '已完成工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#00bfa4',
            },
            areaStyle: {
              color: 'rgba(0, 191, 165, 0.1)',
            },
            yAxisIndex: 1,
            data: VS.getChartDataYIssueCountCompleted,
          },
          {
            name: '未预估工作项百分比',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#f44336',
            },
            areaStyle: {
              color: 'rgba(244, 67, 54, 0.1)',
            },
            yAxisIndex: 1,
            data: VS.getChartDataYIssueCountUnEstimate,
          },
          {
            name: `已完成${VS.getChartYAxisName}`,
            type: 'line',
            step: true,
            yAxisIndex: 0,
            data: VS.getChartDataYCompleted,
            itemStyle: {
              color: '#00bfa4',
            },
            areaStyle: {
              color: 'rgba(0, 191, 165, 0.1)',
            },
          },
          {
            name: `总计${VS.getChartYAxisName}`,
            type: 'line',
            step: true,
            yAxisIndex: 0,
            data: VS.getChartDataYAll,
            itemStyle: {
              color: '#78aafe',
            },
            areaStyle: {
              color: 'rgba(77, 144, 254, 0.1)',
            },
          },
        ],
      };
    }
    return { ...commonOption, ...option };
  }

  getTableDta(type) {
    if (type === 'compoleted') {
      return VS.tableData.filter((v) => v.completed === 1);
    }
    if (type === 'unFinish') {
      return VS.tableData.filter((v) => v.completed === 0);
    }
    if (type === 'unFinishAndunEstimate') {
      return VS.tableData.filter((v) => v.completed === 0
        && ((v.storyPoints === null && v.issueTypeVO && v.issueTypeVO.typeCode === 'story')
          || (v.remainTime === null && v.issueTypeVO && v.issueTypeVO.typeCode !== 'story')));
    }
    return [];
  }

  refresh() {
    if (!VS.currentVersionId) {
      VS.loadEpicAndChartAndTableData();
    } else {
      VS.loadChartData();
      VS.loadTableData();
    }
  }

  handleChangeCurrentVersion(versionId) {
    VS.setCurrentVersion(versionId);
    VS.loadChartData();
    VS.loadTableData();
  }

  handleChangeCurrentUnit(unit) {
    VS.setCurrentUnit(unit);
    VS.loadChartData();
  }

  transformRemainTime(remainTime) {
    if (!remainTime) {
      return '0';
    }
    let time = remainTime * 1;
    const w = Math.floor(time / 40);
    time -= 40 * w;
    const d = Math.floor(time / 8);
    time -= 8 * d;
    return `${w ? `${w}周 ` : ''}${d ? `${d}天 ` : ''}${time ? `${time}小时 ` : ''}`;
  }

  renderTable(type) {
    const column = [
      ...[
        {
          width: '15%',
          title: '工作项编号',
          dataIndex: 'issueNum',
          render: (issueNum, record) => (
            <span
              className="primary"
              style={{
                cursor: 'pointer',
                display: 'block',
                minWidth: 85,
              }}
              role="none"
              onClick={() => {
                LINK_URL_TO.issueLinkTo(record.issueId, issueNum);
              }}
            >
              {issueNum}
              {' '}
              {record.addIssue ? '*' : ''}
            </span>
          ),
        },
        {
          width: '30%',
          title: '概要',
          dataIndex: 'summary',
          render: (summary) => (
            <div style={{ width: '100%', overflow: 'hidden' }}>
              <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={`工作项概要：${summary}`}>
                <p style={{
                  overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
                }}
                >
                  {summary}
                </p>
              </Tooltip>
            </div>
          ),
        },
        {
          width: '15%',
          title: '工作项类型',
          dataIndex: 'typeCode',
          render: (typeCode, record) => (
            <div>
              <TypeTag
                style={{ minWidth: 90 }}
                data={record.issueTypeVO}
                showName
              />
            </div>
          ),
        },
        {
          width: '15%',
          title: '优先级',
          dataIndex: 'priorityId',
          render: (priorityId, record) => (
            <div>
              <PriorityTag
                style={{ minWidth: 55 }}
                priority={record.priorityVO}
              />
            </div>
          ),
        },
        {
          width: '10%',
          title: '状态',
          dataIndex: 'statusCode',
          render: (statusCode, record) => (
            <div>
              <Tooltip mouseEnterDelay={0.5} title={`任务状态： ${record.statusVO.name}`}>
                <div>
                  <StatusTag
                    style={{ display: 'inline-block' }}
                    name={record.statusVO.name}
                    color={STATUS[record.statusVO.type]}
                  />
                </div>
              </Tooltip>
            </div>
          ),
        },
      ],
      ...[
        VS.beforeCurrentUnit === 'issue_count' ? {} : {
          width: '15%',
          title: VS.beforeCurrentUnit === 'story_point' ? '故事点(点)' : '剩余时间(小时)',
          dataIndex: 'storyPoints',
          render: (storyPoints, record) => (
            <div style={{ minWidth: 15 }}>
              {this.getLabel(record)}
            </div>
          ),
        },
      ],
    ];
    return (
      <Table
        pagination={this.getTableDta(type).length > 10}
        rowKey={(record) => record.issueId}
        dataSource={this.getTableDta(type)}
        filterBar={false}
        columns={column}
        scroll={{ x: true }}
        loading={false}
      />
    );
  }

  render() {
    return (
      <Page
        className="c7n-versionReport"
      >
        <Header
          title="版本报告图"
        >
          <HeaderButtons
            items={[{
              name: '切换',
              element: <SwithChart
                current="versionReport"
              />,
              display: true,
            }, {
              name: '返回',
              element: <BackBtn />,
              display: true,
            }, {
              name: '刷新',
              icon: 'refresh',
              iconOnly: true,
              handler: () => {
                this.refresh();
              },
              display: true,
            }]}
          />
        </Header>
        <Breadcrumb title="版本报告图" />
        <Content style={{ paddingTop: 20 }}>
          <LoadingProvider>
            <Loading loading={!VS.versionFinishLoading} />
            {
            !(!VS.versions.length && VS.versionFinishLoading) ? (
              <div>
                <div style={{ display: 'flex' }}>
                  <Form columns={2} style={{ width: 508 }}>
                    <Select
                      label="版本"
                      value={VS.currentVersionId}
                      onChange={(versionId) => this.handleChangeCurrentVersion(versionId)}
                      clearButton={false}
                    >
                      {
                        VS.versions.map((version) => (
                          <Option
                            key={version.versionId}
                            value={version.versionId}
                          >
                            {version.name}
                          </Option>
                        ))
                      }
                    </Select>
                    <Select
                      style={{ marginLeft: 6 }}
                      label="单位"
                      value={VS.currentUnit}
                      onChange={(unit) => this.handleChangeCurrentUnit(unit)}
                      clearButton={false}
                    >
                      <Option key="story_point" value="story_point">故事点</Option>
                      <Option key="issue_count" value="issue_count">工作项计数</Option>
                      <Option key="remain_time" value="remain_time">剩余时间</Option>
                    </Select>
                  </Form>
                </div>
                <div style={{ marginTop: 10, display: 'flex', justifyContent: 'space-between' }}>
                  <p style={{ marginBottom: 0, marginTop: -20, color: 'var(--text-color)' }}>{VS.getCurrentVersion.versionId && VS.getCurrentVersion.statusCode === 'released' ? `发布于 ${VS.getCurrentVersion.releaseDate ? VS.getCurrentVersion.releaseDate.split(' ')[0] : '未指定发布日期'}` : '未发布'}</p>
                  <p
                    className="primary"
                    style={{
                      cursor: 'pointer',
                      display: 'flex',
                      alignItems: 'center',
                      marginBottom: 0,
                    }}
                    role="none"
                    onClick={() => {
                      to(LINK_URL.workListIssue, {
                        params: {
                          paramType: 'version',
                          paramId: VS.currentVersionId,
                          paramName: `${VS.getCurrentVersion.name}下的工作项`,
                        },
                      }, { blank: true });
                    }}
                  >
                    在“所有工作项中”查看
                    <Icon style={{ fontSize: 13 }} type="open_in_new" />
                  </p>
                </div>
                <div className="c7n-report">
                  {
                      VS.chartData.length ? (
                        <div className="c7n-chart">
                          {
                            VS.reload ? null : (
                              <ReactEcharts option={this.getOption()} style={{ height: 400 }} />
                            )
                          }
                        </div>
                      ) : (
                        <LoadingHiddenWrap>
                          <div style={{
                            padding: '20px 0', textAlign: 'center', width: '100%', color: 'var(--text-color)',
                          }}
                          >
                            {VS.tableData.length ? '当前单位下工作项均未预估，切换单位或从下方工作项列表进行预估。' : '当前版本下没有工作项。'}
                          </div>
                        </LoadingHiddenWrap>
                      )
                    }
                </div>
                <Loading loading={VS.chartLoading} />
                <Loading loading={VS.tableLoading} />
                <Tabs>
                  <TabPane tab="已完成的工作项" key="done">
                    {this.renderTable('compoleted')}
                  </TabPane>
                  <TabPane tab="未完成的工作项" key="todo">
                    {this.renderTable('unFinish')}
                  </TabPane>
                  {
                    VS.beforeCurrentUnit === 'issue_count' ? null : (
                      <TabPane tab="未完成的未预估工作项" key="undo">
                        {this.renderTable('unFinishAndunEstimate')}
                      </TabPane>
                    )
                  }
                </Tabs>
              </div>
            ) : (
              <LoadingHiddenWrap>
                <EmptyPage
                  image={pic}
                  description={(
                    <div>
                      <span>当前项目无可用版本，请在</span>
                      <EmptyPage.Button
                        onClick={() => {
                          to(LINK_URL.workListVersion);
                        }}
                      >
                        【版本列表】
                      </EmptyPage.Button>
                      <span>中创建一个版本</span>
                    </div>
                )}
                />
              </LoadingHiddenWrap>
            )
          }
          </LoadingProvider>

        </Content>
      </Page>
    );
  }
}

export default EpicReport;
