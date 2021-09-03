/* eslint-disable react/sort-comp */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import ReactEcharts from 'echarts-for-react';
import _ from 'lodash';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import {
  Tabs, Table, Icon, Tooltip, Spin,
} from 'choerodon-ui';
import { Form, Select, CheckBox } from 'choerodon-ui/pro';
// import pic from './no_epic.svg';
import { EmptyPage } from '@choerodon/components';
import STATUS from '@/constants/STATUS';
import to from '@/utils/to';
import LINK_URL, { LINK_URL_TO } from '@/constants/LINK_URL';

import pic from '../../../assets/image/NoData.svg';
// import finish from './legend/finish.svg';
import SwithChart from '../Component/switchChart';
import StatusTag from '../../../components/StatusTag';
import PriorityTag from '../../../components/PriorityTag';
import TypeTag from '../../../components/TypeTag';
import ES from '../../../stores/project/epicBurndown';
import seeChangeRange from './seeChangeRange.svg';
import seeProgress from './seeProgress.svg';
import speedIcon from './speedIcon.svg';
import sprintIcon from './sprintIcon.svg';
import storyPointIcon from './storyPointIcon.svg';
import completed from './completed.svg';
import BackBtn from '../back-btn';
import './EpicReport.less';
import { Loading } from '@/components';
import { LoadingHiddenWrap, LoadingProvider } from '@/components/Loading';

const { Option } = Select;
const { TabPane } = Tabs;

@observer
class EpicBurndown extends Component {
  constructor(props) {
    super(props);
    this.state = {
      checkbox: undefined,
      inverse: true,
      tabActiveKey: 'done',
      linkFromParamUrl: undefined,
    };
  }

  componentDidMount = () => {
    const { location: { search } } = this.props;
    const linkFromParamUrl = _.last(search.split('&')).split('=')[0] === 'paramUrl' ? _.last(search.split('&')).split('=')[1] : undefined;
    this.setState({
      linkFromParamUrl,
    });
    ES.loadEpicAndChartAndTableData();
  };

  getLegendData() {
    const arr = ['工作已完成', '工作剩余', '工作增加'];
    const legendData = [];
    arr.forEach((item) => {
      legendData.push({
        name: item,
        textStyle: { fontSize: 12 },
      });
    });
    return legendData;
  }

  getLabel(record) {
    if (ES.beforeCurrentUnit === 'story_point') {
      if (record.typeCode === 'story') {
        return record.storyPoints === null ? '' : record.storyPoints;
      }
      return '';
    }
    return record.remainTime === null ? '' : record.remainTime;
  }

  getOption() {
    const { checkbox, inverse } = this.state;
    const { chartDataOrigin } = ES;
    const option = {
      animation: false,
      grid: {
        top: 30,
        left: 40,
        right: 50,
        containLabel: true,
      },
      xAxis: [
        {
          type: 'category',
          data: _.map(ES.chartDataOrigin, 'name'),
          // data: xAxisData,
          itemStyle: {
            color: 'var(--text-color3)',
          },
          axisTick: { show: false },
          axisLine: {
            show: true,
            lineStyle: {
              color: '#eee',
              type: 'solid',
              width: 1,
            },
          },
          axisLabel: {
            // interval: chartDataOrigin.length <= 7 ? 0 : _.parseInt(chartDataOrigin.length / 7),
            interval: 0,
            show: true,
            showMinLabel: true,
            agile: 'right',
            textStyle: {
              color: 'var(--text-color3)',
            },
            formatter(value, index) {
              if (chartDataOrigin.length >= 7) {
                return value.length > 5 ? `${value.slice(0, 5)}...` : value;
              }
              if (chartDataOrigin.length >= 10) {
                return value.length > 3 ? `${value.slice(0, 3)}...` : value;
              }
              return value.length > 7 ? `${value.slice(0, 7)}...` : value;

              // if (chartDataOrigin.length >= 7) {
              //   return value.length > 5 ? (
              //     <Tooltip title={value}>
              //       <span>{`${value.slice(0, 5)}...`}</span>
              //     </Tooltip>
              //   ) : value;
              // }
              // if (chartDataOrigin.length >= 10) {
              //   return value.length > 3 ?`<Tooltip title={value}><span>
              // {(`${value.slice(0, 5)}...`)}</span></Tooltip>` : value;
              // }
              // return value.length > 7 ? (
              //   <Tooltip title={value}>
              //     <span>{`${value.slice(0, 5)}...`}</span>
              //   </Tooltip>
              // ) : value;
            },
          },
        },
      ],
      yAxis: [
        {
          inverse,
          type: 'value',
          position: 'left',
          axisTick: { show: false },
          axisLine: {
            show: true,
            lineStyle: {
              color: '#eee',
              type: 'solid',
              width: 1,
            },
          },
          axisLabel: {
            show: true,
            textStyle: {
              color: 'var(--text-color3)',
            },
            formatter(value, index) {
              return !value ? value : '';
            },
          },
          splitLine: {
            lineStyle: {
              color: '#eee',
            },
          },
        },
        {
          inverse,
          type: 'value',
          position: 'right',
          axisTick: { show: false },
          axisLine: {
            show: true,
            lineStyle: {
              color: '#eee',
              type: 'solid',
              width: 1,
            },
          },
          axisLabel: {
            show: true,
            textStyle: {
              color: 'var(--text-color3)',
            },
            formatter(value, index) {
              return !value ? value : '';
            },
          },
          splitLine: {
            lineStyle: {
              color: '#eee',
            },
          },
        },
      ],
      legend: {
        show: true,
        data: this.getLegendData(),
        right: 50,
        itemWidth: 14,
        itemHeight: 14,
        itemGap: 30,
        icon: 'rect',
      },
      tooltip: {
        show: true,
        trigger: 'axis',
        axisPointer: { // 坐标轴指示器，坐标轴触发有效
          type: 'shadow', // 默认为直线，可选为：'line' | 'shadow'
        },
        backgroundColor: '#fff',
        textStyle: {
          color: 'var(--text-color)',
          fontSize: 13,
        },
        borderColor: '#ddd',
        borderWidth: 1,
        extraCssText: 'box-shadow: 0 2px 4px 0 rgba(0,0,0,0.20);',
        formatter(params) {
          // eslint-disable-next-line no-param-reassign
          params[0].name = _.trim(params[0].name, '\n\n');
          const sprint = chartDataOrigin.filter((item) => item.name === params[0].name)[0];
          let res = `<span className="primary">${params[0].name}</span>`;
          res += `<span style="display:block; margin-top: 0px; margin-bottom: 2px; color: rgba(0,0,0,0.54); font-size: 11px;">${sprint.startDate && sprint.startDate.split(' ')[0].split('-').join('/')}-${sprint.endDate && sprint.endDate.split(' ')[0].split('-').join('/')}</span>`;
          res += `本迭代开始时故事点数：${sprint.start}`;
          res += `<br/>工作已完成: ${(params[1].value === '-' ? 0 : Number(params[1].value)) + (params[4].value === '-' ? 0 : Number(params[4].value))}`;
          res += `<br/>工作增加: ${sprint.add}`;
          res += `<br/>本迭代结束时剩余故事点数: ${(params[2].value === '-' ? 0 : Number(params[2].value)) + (params[3].value === '-' ? 0 : Number(params[3].value))}`;
          return res;
        },
      },
      series: [
        {
          name: '辅助',
          type: 'bar',
          stack: '总量',
          barWidth: 52,
          itemStyle: {
            normal: {
              barBorderColor: 'rgba(0,0,0,0)',
              color: 'rgba(0,0,0,0)',
            },
            emphasis: {
              barBorderColor: 'rgba(0,0,0,0)',
              color: 'rgba(0,0,0,0)',
            },
          },
          // data: [0, 0, 0, 16, 19],
          // data: ES.chartData[0],
          data: checkbox === 'checked' ? _.fill(Array(ES.chartData[0].length), 0) : ES.chartData[0],
        },
        {
          name: '工作已完成',
          type: 'bar',
          stack: '总量',
          barMinHeight: 15,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'inside',
                color: '#fff',
                formatter(param) {
                  return param.value === '-' ? null : `-${param.value}`;
                },
              },
              color: 'rgba(0,191,165,0.8)',
            },
          },
          // data: ['-', '-', 16, 3, '-'],
          data: ES.chartData[1],
        },
        {
          name: '工作剩余',
          type: 'bar',
          stack: '总量',
          barMinHeight: 15,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'inside',
                color: '#fff',
              },
              // color: 'rgb(0, 187, 255, 0.8)',
              color: 'rgba(69,163,252,0.80)',
            },
          },
          // data: [3, 3, '-', 13, 18],
          data: ES.chartData[2],
        },
        {
          name: '工作增加',
          type: 'bar',
          stack: '总量',
          barMinHeight: 15,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'inside',
                color: '#fff',
                formatter(param) {
                  return param.value === '-' ? null : `+${param.value}`;
                },
              },
              // color: 'rgba(27,128,255,0.8)',
              color: 'rgba(27,128,223,0.80)',
              opacity: 0.75,
            },
          },
          // data: ['-', 13, 16, 5, '-'],
          data: ES.chartData[3],
        },
        {
          name: 'compoleted again',
          type: 'bar',
          stack: '总量',
          barMinHeight: 15,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'inside',
                color: '#fff',
                formatter(param) {
                  return param.value === '-' ? null : `-${param.value}`;
                },
              },
              color: 'rgba(0,191,165,0.8)',
            },
          },
          // data: ['-', '-', 3, '-', '-'],
          data: ES.chartData[4],
        },
        {
          name: 'showZeroBottom',
          type: 'bar',
          stack: '总量',
          barMinHeight: 2,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'bottom',
                color: 'var(--text-color)',
                formatter(param) {
                  return 0;
                },
              },
              color: 'rgba(0,0,0,0.54)',
            },
          },
          // data: ['-', '-', 3, 3, '-'],
          data: inverse ? ES.chartData[5] : [],
        },
        {
          name: 'showZeroTop',
          type: 'bar',
          stack: '总量',
          barMinHeight: 2,
          itemStyle: {
            normal: {
              label: {
                show: true,
                position: 'top',
                color: 'var(--text-color)',
                formatter(param) {
                  return 0;
                },
              },
              color: 'rgba(0,0,0,0.54)',
            },
          },
          // data: ['-', '-', 3, 3, '-'],
          data: inverse ? ES.chartData[6] : ES.chartData[7],
        },
      ],
      dataZoom: [{
        bottom: 30,
        show: ES.chartData[4].length > 7,
        type: 'slider',
        height: 15,
        width: '80%',
        left: 60,
        startValue: 0,
        endValue: 7,
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
  }

  transformPlaceholder2Zero = (arr) => arr.map((v) => (v === '-' ? 0 : v))

  getSprintSpeed = () => {
    const { chartData, chartDataOrigin } = ES;
    if (chartDataOrigin.length > 3) {
      const lastThree = chartDataOrigin.slice(chartDataOrigin.length - 3, chartDataOrigin.length);
      const lastThreeDone = [];
      lastThree.forEach((item) => {
        lastThreeDone.push(item.done);
      });
      return _.floor(_.sum(lastThreeDone) / 3, 2);
    }
    return 0;
  }

  getStoryPoints = () => {
    const { chartData } = ES;
    // if (chartData[2].length > 3) {
    const lastRemain = _.last(this.transformPlaceholder2Zero(chartData[2]));
    const lastAdd = _.last(this.transformPlaceholder2Zero(chartData[3]));
    return Number(lastRemain) + Number(lastAdd);
    // }
    // return 0;
  }

  getColumn = (item) => {
    let totalStoryPoints = 0;
    if (item && item.length > 0) {
      totalStoryPoints = _.sum(_.map(_.filter(item, (o) => o.typeCode === 'story' && o.storyPoints !== null), 'storyPoints'));
      if (totalStoryPoints % 1 > 0) {
        totalStoryPoints = totalStoryPoints.toFixed(1);
      }
    }

    const column = [
      ...[
        {
          // width: '15%',
          title: '问题编号',
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
          // width: '30%',
          title: '概要',
          dataIndex: 'summary',
          render: (summary) => (
            <div style={{ width: '100%', overflow: 'hidden' }}>
              <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={`问题概要：${summary}`}>
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
          // width: '15%',
          title: '问题类型',
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
          // width: '15%',
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
          // width: '15%',
          title: '状态',
          dataIndex: 'statusCode',
          render: (statusCode, record) => (
            <div>
              <Tooltip mouseEnterDelay={0.5} title={`任务状态:${record.statusVO.name}`}>
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
          // width: '10%',
          title: ES.beforeCurrentUnit === 'story_point' ? `故事点 (${totalStoryPoints}点)` : '剩余时间(小时)',
          dataIndex: 'storyPoints',
          render: (storyPoints, record) => (
            <div style={{ minWidth: 15 }}>
              {this.getLabel(record)}
            </div>
          ),
        },
      ],
    ];
    return column;
  }

  getSprintCount() {
    return Math.ceil(this.getStoryPoints() / this.getSprintSpeed());
  }

  getTableDta(type) {
    if (type === 'compoleted') {
      // return ES.tableData.filter(v => v.completed === 1);
      return ES.tableData.sprintBurnDownReportVOS;
    }
    if (type === 'unFinish') {
      // return ES.tableData.filter(v => v.completed === 0);
      return ES.tableData.incompleteIssues;
    }
    return [];
  }

  refresh() {
    if (!ES.currentEpicId) {
      ES.loadEpicAndChartAndTableData();
    } else {
      ES.loadChartData();
      ES.loadTableData();
      // this.setInitialPagination();
    }
  }

  handleChangeCurrentEpic(epicId) {
    ES.setCurrentEpic(epicId);
    ES.loadChartData();
    ES.loadTableData();
    this.setState({
      tabActiveKey: 'done',
    });
  }

  handleChangeCheckbox(checkbox) {
    this.setState({
      checkbox,
      inverse: checkbox !== 'checked',
    });
  }

  handleIconMouseEnter = () => {
    const iconShowInfo = document.getElementsByClassName('icon-show-info')[0];
    iconShowInfo.style.display = 'flex';
  }

  handleIconMouseLeave = () => {
    const iconShowInfo = document.getElementsByClassName('icon-show-info')[0];
    iconShowInfo.style.display = 'none';
  }

  handleLinkToIssue(linkType, item) {
    if (JSON.stringify(item) !== '{}') {
      if (linkType === 'epic') {
        LINK_URL_TO.issueLinkTo(item.issueId, item.issueNum);
        return;
      }
      to(LINK_URL.workListIssue);
    }
  }

  renderChart = () => {
    if (!ES.chartDataOrigin.length) {
      return (
        <EmptyPage
          image={pic}
          style={{ paddingTop: '.2rem' }}
          description={(
            <div style={{ textAlign: 'left' }}>
              在此史诗中没有预估的故事，请在
              <a
                role="none"
                onClick={() => {
                  to(LINK_URL.workListBacklog);
                }}
              >
                待办事项
              </a>
              中创建故事并预估故事点。
            </div>
          )}
        />
      );
    }
    return (
      <div className="c7n-report">
        <div className="c7n-chart">
          {
            ES.reload ? null : (
              <div style={{ position: 'relative' }}>
                <div className="c7n-chart-yaxixName">
                  故事点
                </div>
                <ReactEcharts
                  ref={(e) => { this.echarts_react = e; }}
                  option={this.getOption()}
                  style={{ height: 400, left: -31 }}
                />
              </div>
            )
          }
        </div>
        <div className="c7n-toolbar">
          {this.renderToolbar()}
        </div>
      </div>
    );
  }

  renderTable = (type) => {
    const sprintBurnDownReportVOS = this.getTableDta('compoleted');
    let firstCompleteIssues = 0;

    if (type === 'unFinish') {
      return (
        <Table
          rowKey={(record) => record.issueId}
          dataSource={this.getTableDta(type)}
          filterBar={false}
          columns={this.getColumn(this.getTableDta('unFinish'))}
          scroll={{ x: true }}
          loading={ES.tableLoading}
          pagination={!!(this.getTableDta(type) && this.getTableDta(type).length > 10)}
        />
      );
    }
    if (sprintBurnDownReportVOS && sprintBurnDownReportVOS.length !== 0) {
      for (let i = 0; i < sprintBurnDownReportVOS.length; i += 1) {
        if (sprintBurnDownReportVOS[i].completeIssues.length !== 0) {
          firstCompleteIssues = i;
          break;
        }
        firstCompleteIssues += 1;
      }
      if (firstCompleteIssues !== sprintBurnDownReportVOS.length) {
        return (
          <div>
            {
              sprintBurnDownReportVOS.map((item) => {
                if (item.completeIssues.length !== 0) {
                  return (
                    <div
                      style={{ marginBottom: 22 }}
                      key={item.sprintId}
                    >
                      <p style={{
                        position: 'relative',
                        marginBottom: 12,
                        marginLeft: 15,
                      }}
                      >
                        <span
                          className="primary"
                          style={{
                            cursor: 'pointer',
                          }}
                          role="none"
                          onClick={() => {
                            if (item.statusCode === 'started') {
                              to(LINK_URL.workListBacklog);
                            } else {
                              to(LINK_URL.reportSprint, { params: { sprintId: item.sprintId, paramUrl: 'reporthost/EpicBurndown' } });
                            }
                          }}
                        >
                          {`${item.sprintName}`}
                        </span>
                        <span
                          style={{
                            color: 'var(--text-color3)',
                            fontSize: 12,
                            marginLeft: 12,
                          }}
                        >
                          {`${item.startDate && item.startDate.slice(0, 11).replace(/-/g, '.')}-${item.endDate && item.endDate.slice(0, 11).replace(/-/g, '.')}`}
                        </span>
                      </p>
                      <Table
                        rowKey={(record) => record.issueId}
                        dataSource={item.completeIssues}
                        filterBar={false}
                        columns={this.getColumn(item.completeIssues)}
                        scroll={{ x: true }}
                        loading={ES.tableLoading}
                        pagination={!!(item.completeIssues && item.completeIssues.length > 10)}
                      />
                    </div>
                  );
                }
                return '';
              })
              //  : <p>当前史诗下的冲刺没有已完成的问题</p>
            }
          </div>
        );
      }
      return <p style={{ color: 'var(--text-color)' }}>当前史诗下的冲刺没有已完成的问题</p>;
    }

    return <p style={{ color: 'var(--text-color)' }}>当前史诗下的冲刺没有已完成的问题</p>;
  }

  renderToolbarTitle = () => {
    const { chartDataOrigin } = ES;
    if (this.getSprintSpeed() === 0) {
      return `根据最近${chartDataOrigin.length}次冲刺的数据，无法预估迭代次数`;
    }
    return `根据最近${chartDataOrigin.length}次冲刺的数据，将花费${this.getSprintCount()}个迭代来完成此史诗。`;
  }

  renderToolbar = () => {
    const { chartDataOrigin } = ES;
    if (chartDataOrigin.length < 3) {
      return (
        <div className="toolbar-cannot-forcast">
          <h3 className="title">尚不可预测</h3>
          <div className="word">至少3个冲刺完成，才能显示预测</div>
        </div>
      );
    }
    if (this.getStoryPoints() === 0) {
      return (
        <div className="toolbar-complete">
          <div className="pic">
            <img src={completed} alt="所有预估的问题都已完成!" />
          </div>
          <div className="word">所有预估的问题都已完成！</div>
        </div>
      );
    }
    return (
      <div className="toolbar-forcast">
        <h3 className="title">{this.renderToolbarTitle()}</h3>
        <div className="toolbar-forcast-content">
          <div className="word">
            <div className="icon">
              <img src={sprintIcon} alt="冲刺迭代" />
            </div>
            <span>{`冲刺迭代：${!this.getSprintSpeed() ? '无法预估' : this.getSprintCount()}`}</span>
          </div>
          <div className="word">
            <div className="icon">
              <img src={speedIcon} alt="冲刺速度" />
            </div>
            <span>{`冲刺速度：${this.getSprintSpeed()}`}</span>
          </div>
          <div className="word">
            <div className="icon">
              <img src={storyPointIcon} alt="剩余故事点" />
            </div>
            <span>{`剩余故事点：${this.getStoryPoints()}`}</span>
          </div>
        </div>

      </div>
    );
  }

  renderEpicInfo() {
    if (ES.currentEpicId !== undefined) {
      const currentEpic = ES.epics.find((item) => item.issueId === ES.currentEpicId);
      return (
        <p className="c7n-epicInfo">
          <span
            className="primary"
            style={{
              cursor: 'pointer',
            }}
            role="none"
            onClick={this.handleLinkToIssue.bind(this, 'epic', ES.currentEpicId !== undefined ? ES.epics.filter((item) => item.issueId === ES.currentEpicId)[0] : {})}
          >
            {`${currentEpic ? currentEpic.issueNum : ''}`}
          </span>
          <span>{` ${currentEpic ? currentEpic.summary : ''}`}</span>
        </p>
      );
    }
    return '';
  }

  render() {
    const { checkbox, tabActiveKey, linkFromParamUrl } = this.state;
    return (
      <Page
        className="c7n-epicBurndown"
      >
        <Header
          title="史诗燃耗图"
        >
          <HeaderButtons
            items={[{
              name: '切换',
              element: <SwithChart
                current="epicBurndown"
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
        <Breadcrumb title="史诗燃耗图" />
        <Content style={{ paddingTop: 20 }}>
          <LoadingProvider>
            {
            !(!ES.epics.length && ES.epicFinishLoading) ? (
              <div>
                <div style={{ display: 'flex', alignItems: 'center' }}>
                  <Form style={{ width: 512, marginRight: 19 }}>
                    <Select
                      label="史诗"
                      value={ES.currentEpicId}
                      onChange={(epic) => this.handleChangeCurrentEpic(epic)}
                      getPopupContainer={((triggerNode) => triggerNode.parentNode)}
                      clearButton={false}
                    >
                      {
                        ES.epics.map((epic) => (
                          <Option key={epic.issueId} value={epic.issueId}>{epic.epicName}</Option>
                        ))
                      }
                    </Select>
                  </Form>
                  <div className="c7n-epicSelectHeader" style={{ marginTop: 5 }}>
                    <CheckBox
                      value="checked"
                      checked={checkbox === 'checked'}
                      onChange={(val) => this.handleChangeCheckbox(val)}
                    >
                      根据图表校准冲刺
                    </CheckBox>
                    <span className="icon-show" role="none" onMouseEnter={this.handleIconMouseEnter} onMouseLeave={this.handleIconMouseLeave}>
                      <Icon type="help icon" />
                    </span>

                    <div className="icon-show-info" onMouseEnter={this.handleIconMouseEnter} onMouseLeave={this.handleIconMouseLeave}>
                      <figure className="icon-show-progress">
                        <div className="icon-show-info-svg">
                          <img src={seeProgress} alt="查看进度" />
                        </div>
                        <figcaption className="icon-show-info-detail">
                          <p className="icon-show-info-detail-header">查看进度</p>
                          <p className="icon-show-info-detail-content">按照史诗查看冲刺进度</p>
                        </figcaption>
                      </figure>
                      <figure>
                        <div className="icon-show-info-svg">
                          <img src={seeChangeRange} alt="查看变更范围" />
                        </div>
                        <figcaption className="icon-show-info-detail">
                          <p className="icon-show-info-detail-header">查看变更范围</p>
                          <p className="icon-show-info-detail-content">跟踪范围的扩大和缩小，由底部条状信息显示。</p>
                        </figcaption>
                      </figure>
                    </div>
                  </div>
                </div>
                <div style={{ marginTop: -10 }}>
                  {this.renderEpicInfo()}
                </div>

                <Loading loading={ES.chartLoading}>
                  {
                      this.renderChart()
                    }
                </Loading>
                <Tabs
                  activeKey={tabActiveKey}
                  onChange={(key) => {
                    this.setState({
                      tabActiveKey: key,
                    });
                  }}
                >
                  <TabPane tab="已完成的问题" key="done">
                    {this.renderTable('compoleted')}
                  </TabPane>
                  <TabPane tab="未完成的问题" key="todo">
                    {this.renderTable('unFinish')}
                  </TabPane>
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
                        【问题管理】
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

export default EpicBurndown;
