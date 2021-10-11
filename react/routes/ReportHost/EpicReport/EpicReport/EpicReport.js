import React, { Component } from 'react';
import { observer } from 'mobx-react';
import ReactEcharts from 'echarts-for-react';
import _ from 'lodash';
import {
  Page, Header, Content, stores, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import {
  Tabs, Table, Icon, Tooltip, Spin,
} from 'choerodon-ui';
import { Form, Select } from 'choerodon-ui/pro';
// import pic from './no_epic.svg';
import { EmptyPage } from '@choerodon/components';
import STATUS from '@/constants/STATUS';
import LINK_URL, { LINK_URL_TO } from '@/constants/LINK_URL';
import to from '@/utils/to';

import pic from '../../../../assets/image/NoData.svg';
import finish from './legend/finish.svg';
import SwithChart from '../../Component/switchChart';
import StatusTag from '../../../../components/StatusTag';
import PriorityTag from '../../../../components/PriorityTag';
import TypeTag from '../../../../components/TypeTag';
import ES from '../../../../stores/project/epicReport';
import BackBtn from '../../back-btn';
import './EpicReport.less';
import Loading, { LoadingHiddenWrap, LoadingProvider } from '@/components/Loading';

const { TabPane } = Tabs;
const { AppState } = stores;
const { Option } = Select;

@observer
class EpicReport extends Component {
  constructor(props) {
    super(props);
    this.state = {
      linkFromParamUrl: undefined,
    };
  }

  componentDidMount() {
    const { location: { search } } = this.props;
    const linkFromParamUrl = _.last(search.split('&')).split('=')[0] === 'paramUrl' ? _.last(search.split('&')).split('=')[1] : undefined;
    this.setState({
      linkFromParamUrl,
    });
    ES.loadEpicAndChartAndTableData();
  }

  getLabel(record) {
    if (ES.beforeCurrentUnit === 'story_point') {
      if (record.issueTypeVO && record.issueTypeVO.typeCode === 'story') {
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
      未预估工作项数: '个',
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
          if (!params || !params.length) {
            content = '';
          } else {
            content = `<div>${params[0].axisValue}</div>`;
            params.forEach((param) => {
              content += `<div style="font-size: 11px"><div class="c7n-tooltip-icon" style="background: ${param.color}"></div>${param.seriesName}：${param.value}${param.value ? UNIT[param.seriesName] : ''}</div>`;
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
        itemGap: 30,
        data: [
          ...[
            ES.beforeCurrentUnit === 'issue_count' ? {} : {
              name: `已完成${ES.getChartYAxisName}`,
              icon: `image://${finish}`,
            },
          ],
          ...[
            ES.beforeCurrentUnit === 'issue_count' ? {} : {
              name: `总计${ES.getChartYAxisName}`,
              icon: 'rectangle',
            },
          ],
          ...[
            {
              name: '总工作项数',
              icon: 'line',
            },
          ],
          ...[
            ES.beforeCurrentUnit === 'issue_count' ? {} : {
              name: '未预估工作项数',
              icon: 'line',
            },
          ],
          ...[
            ES.beforeCurrentUnit === 'issue_count' ? {
              name: '已完成工作项数',
              icon: 'line',
            } : {},
          ],
        ],
      },
      grid: {
        y2: 50,
        top: '30',
        left: 0,
        right: '50',
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
          interval: ES.getChartDataX.length >= 20 ? 4 : 0,
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
            width: 1,
            type: 'solid',
          },
        },
        data: ES.getChartDataX,
      },
      dataZoom: [{
        startValue: ES.getChartDataX[0],
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
    if (ES.beforeCurrentUnit === 'issue_count') {
      option = {
        yAxis: [{
          name: '工作项计数',
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
              width: 1,
            },
          },
        }],
        series: [
          {
            name: '总工作项数',
            type: 'line',
            step: true,
            // symbol: ES.getChartDataYIssueCountAll.length === 1 ? 'auto' : 'none',
            itemStyle: {
              color: 'rgba(48, 63, 159, 1)',
            },
            // yAxisIndex: ES.beforeCurrentUnit === 'issue_count' ? 0 : 1,
            data: ES.getChartDataYIssueCountAll,
          },
          {
            name: '已完成工作项数',
            type: 'line',
            step: true,
            // symbol: ES.getChartDataYIssueCountCompleted.length === 1 ? 'auto' : 'none',
            itemStyle: {
              color: '#00bfa4',
            },
            // yAxisIndex: ES.beforeCurrentUnit === 'issue_count' ? 0 : 1,
            data: ES.getChartDataYIssueCountCompleted,
          },
          {
            name: '未预估工作项数',
            type: 'line',
            step: true,
            // symbol: ES.getChartDataYIssueCountUnEstimate.length === 1 ? 'auto' : 'none',
            itemStyle: {
              color: '#ff9915',
            },
            // yAxisIndex: ES.beforeCurrentUnit === 'issue_count' ? 0 : 1,
            data: ES.getChartDataYIssueCountUnEstimate,
          },
        ],
      };
    } else {
      option = {
        yAxis: [{
          name: ES.getChartYAxisName,
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
              if (value && ES.beforeCurrentUnit === 'remain_time') {
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
              width: 1,
            },
          },
        },
        {
          name: '工作项计数',
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
            show: false,
          },
        }],
        series: [
          {
            name: '总工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: 'rgba(48, 63, 159, 1)',
            },
            yAxisIndex: 1,
            data: ES.getChartDataYIssueCountAll,
          },
          {
            name: '已完成工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#00bfa4',
            },
            yAxisIndex: 1,
            data: ES.getChartDataYIssueCountCompleted,
          },
          {
            name: '未预估工作项数',
            type: 'line',
            step: true,
            itemStyle: {
              color: '#ff9915',
            },
            yAxisIndex: 1,
            data: ES.getChartDataYIssueCountUnEstimate,
          },
          {
            name: `已完成${ES.getChartYAxisName}`,
            type: 'line',
            step: true,
            yAxisIndex: 0,
            data: ES.getChartDataYCompleted,
            itemStyle: {
              color: '#00BFA5',
            },
            areaStyle: {
              color: 'rgba(77, 144, 254, 0.1)',
            },
          },
          {
            name: `总计${ES.getChartYAxisName}`,
            type: 'line',
            step: true,
            yAxisIndex: 0,
            data: ES.getChartDataYAll,
            itemStyle: {
              color: 'rgba(0, 0, 0, 0.16)',
            },
            areaStyle: {
              color: 'rgba(245, 245, 245, 0.5)',
            },
          },
        ],
      };
    }
    return { ...commonOption, ...option };
  }

  getTableDta(type) {
    if (type === 'compoleted') {
      return ES.tableData.filter((v) => v.completed === 1);
    }
    if (type === 'unFinish') {
      return ES.tableData.filter((v) => v.completed === 0);
    }
    if (type === 'unFinishAndunEstimate') {
      if (ES.currentUnit === 'story_point') {
        return ES.tableData.filter((v) => v.completed === 0
          && (v.storyPoints === null && v.issueTypeVO && v.issueTypeVO.typeCode === 'story'));
      }
      return ES.tableData.filter((v) => v.completed === 0 && v.remainTime === null);
    }
    return [];
  }

  refresh() {
    if (!ES.currentEpicId) {
      ES.loadEpicAndChartAndTableData();
    } else {
      ES.loadChartData();
      ES.loadTableData();
    }
  }

  handleChangeCurrentEpic(epicId) {
    ES.setCurrentEpic(epicId);
    ES.loadChartData();
    ES.loadTableData();
  }

  handleChangeCurrentUnit(unit) {
    ES.setCurrentUnit(unit);
    ES.loadChartData();
    // ES.loadTableData();
    // const instance = this.echarts_react.getEchartsInstance();
    // instance.dispose();
    // instance.init();
    // instance.setOption(this.getOption());
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
    if (time % 1 > 0) {
      time = time?.toFixed(1) ?? time;
    }
    return `${w ? `${w} 周 ` : ''}${d ? `${d} 天 ` : ''}${time ? `${time} 小时 ` : ''}`;
  }

  transformStoryPoints(storyPoints) {
    return storyPoints && storyPoints > 0
      ? `${storyPoints % 1 > 0 ? storyPoints?.toFixed(1) ?? storyPoints : storyPoints} 点` : storyPoints;
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
          width: '25%',
          title: '概要',
          dataIndex: 'summary',
          render: (summary) => (
            <div style={{ width: '100%', overflow: 'hidden' }}>
              <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={summary}>
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
          width: '15%',
          title: '状态',
          dataIndex: 'statusCode',
          render: (statusCode, record) => (
            <div>
              <Tooltip mouseEnterDelay={0.5} title={`任务状态： ${record.statusVO.name}`}>
                <span>
                  <StatusTag
                    style={{ display: 'inline-block', minWidth: 50 }}
                    name={record.statusVO.name}
                    color={STATUS[record.statusVO.type]}
                  />
                </span>
              </Tooltip>
            </div>
          ),
        },
      ],
      ...[
        ES.beforeCurrentUnit === 'issue_count' ? {} : {
          width: '15%',
          title: ES.beforeCurrentUnit === 'story_point' ? '故事点(点)' : '剩余时间(小时)',
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
    const { linkFromParamUrl } = this.state;
    const urlParams = AppState.currentMenuType;
    return (
      <Page
        className="c7n-epicReport"
      >
        <Header
          title="史诗报告图"
        >
          <HeaderButtons
            items={[{
              name: '切换',
              element: <SwithChart
                current="epicReport"
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
        <Breadcrumb title="史诗报告图" />
        <Content style={{ paddingTop: 20 }}>
          <LoadingProvider>
            <Loading loading={ES.chartLoading} />
            <Loading loading={!ES.epicFinishLoading} />
            <Loading loading={ES.tableLoading} />
            {
            !(!ES.epics.length && ES.epicFinishLoading) ? (
              <div>
                <div style={{ display: 'flex' }}>
                  <Form columns={2} style={{ width: 508 }}>
                    <Select
                      label="史诗选择"
                      value={ES.currentEpicId}
                      onChange={(epicId) => this.handleChangeCurrentEpic(epicId)}
                      clearButton={false}
                    >
                      {
                      ES.epics.map((epic) => (
                        <Option key={epic.issueId} value={epic.issueId}>{epic.epicName}</Option>
                      ))
                    }
                    </Select>
                    <Select
                      label="单位选择"
                      style={{ marginLeft: 6 }}
                      value={ES.currentUnit}
                      onChange={(unit) => this.handleChangeCurrentUnit(unit)}
                      clearButton={false}
                    >
                      <Option key="story_point" value="story_point">故事点</Option>
                      <Option key="issue_count" value="issue_count">工作项计数</Option>
                      <Option key="remain_time" value="remain_time">剩余时间</Option>
                    </Select>
                  </Form>
                </div>
                <div>
                  {
                      ES.chartData.length ? (
                        <div className="c7n-report">
                          <div className="c7n-chart">
                            <LoadingHiddenWrap>
                              <ReactEcharts
                                ref={(e) => { this.echarts_react = e; }}
                                option={this.getOption()}
                                style={{ height: 400 }}
                              />
                            </LoadingHiddenWrap>

                          </div>
                          <div className="c7n-toolbar">
                            <h2>汇总</h2>
                            <h4>工作项汇总</h4>
                            <ul>
                              <li>
                                <span className="c7n-tip">合计：</span>
                                <span>
                                  {`${ES.getLatest.issueCount}${ES.getLatest.issueCount > 0 ? ' 个' : ''}`}
                                </span>
                              </li>
                              {
                                ES.beforeCurrentUnit === 'issue_count' ? (
                                  <li>
                                    <span className="c7n-tip">已完成：</span>
                                    <span>{`${ES.getLatest.issueCompletedCount}${ES.getLatest.issueCompletedCount > 0 ? ' 个' : ''}`}</span>
                                  </li>
                                ) : null
                              }
                              {
                                ES.beforeCurrentUnit === 'issue_count' ? null : (
                                  <li>
                                    <span className="c7n-tip">未预估：</span>
                                    <span>{`${ES.getLatest.unEstimateIssueCount}${ES.getLatest.unEstimateIssueCount > 0 ? ' 个' : ''}`}</span>
                                  </li>
                                )
                              }
                            </ul>
                            {
                              ES.beforeCurrentUnit !== 'issue_count' ? (
                                <div>
                                  <h4>
                                    {`${ES.getChartYAxisName}`}
                                    {'汇总'}
                                  </h4>
                                  <ul>
                                    <li>
                                      <span className="c7n-tip">合计：</span>
                                      <span>
                                        {ES.beforeCurrentUnit === 'story_point' ? this.transformStoryPoints(ES.getLatest.allStoryPoints) : this.transformRemainTime(ES.getLatest.allRemainTimes)}
                                      </span>
                                    </li>
                                    <li>
                                      <span className="c7n-tip">已完成：</span>
                                      <span>
                                        {ES.beforeCurrentUnit === 'story_point' ? this.transformStoryPoints(ES.getLatest.completedStoryPoints) : this.transformRemainTime(ES.getLatest.completedRemainTimes)}
                                      </span>
                                    </li>
                                  </ul>
                                </div>
                              ) : null
                            }
                            <p
                              className="primary"
                              style={{
                                cursor: 'pointer',
                              }}
                              role="none"
                              onClick={() => {
                                to(LINK_URL.workListIssue, {
                                  params: {
                                    paramType: 'epic',
                                    paramId: ES.currentEpicId,
                                    paramName: `${ES.epics.find((x) => x.issueId === ES.currentEpicId).epicName}下的工作项`,
                                  },
                                }, { blank: true });
                              }}
                            >
                              在“所有工作项”中查看
                              <Icon style={{ fontSize: 13 }} type="open_in_new" />
                            </p>
                          </div>
                        </div>
                      ) : (
                        <LoadingHiddenWrap>
                          <div style={{ padding: '30px 0 20px', textAlign: 'center' }}>
                            {ES.tableData.length ? '当前单位下工作项均未预估，切换单位或从下方工作项列表进行预估。' : '当前史诗下没有工作项。'}
                          </div>
                        </LoadingHiddenWrap>
                      )
                    }
                </div>
                <Tabs>
                  <TabPane tab="已完成的工作项" key="done">
                    {this.renderTable('compoleted')}
                  </TabPane>
                  <TabPane tab="未完成的工作项" key="todo">
                    {this.renderTable('unFinish')}
                  </TabPane>
                  {
                    ES.beforeCurrentUnit === 'issue_count' ? null : (
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
                      <span>当前项目无可用史诗，请在</span>
                      <EmptyPage.Button
                        onClick={() => {
                          to(LINK_URL.workListBacklog);
                        }}
                      >
                        【待办事项】
                      </EmptyPage.Button>
                      <span>或</span>
                      <EmptyPage.Button
                        onClick={() => {
                          to(LINK_URL.workListIssue);
                        }}
                      >
                        【所有工作项】
                      </EmptyPage.Button>
                      <span>中创建一个史诗</span>
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
