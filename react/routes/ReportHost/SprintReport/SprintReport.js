/* eslint-disable react/destructuring-assignment */
/* eslint-disable prefer-destructuring */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Button, Spin, Icon, Select, Table, Checkbox, Tabs, Tooltip, Pagination,
} from 'choerodon-ui';
import {
  Page, Header, Content, stores, Breadcrumb,
} from '@choerodon/boot';
import ReactEcharts from 'echarts-for-react';
import _ from 'lodash';
import moment from 'moment';
import querystring from 'querystring';
import BurndownChartStore from '@/stores/project/burndownChart/BurndownChartStore';
import ReportStore from '@/stores/project/Report';
import { commonformatDate } from '@/utils/Date';
import epicSvg from '@/assets/image/emptyChart.svg';
import StatusTag from '@/components/StatusTag';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import STATUS from '@/constants/STATUS';
import { sprintApi, reportApi } from '@/api';
import to from '@/utils/to';
import LINK_URL, { LINK_URL_TO } from '@/constants/LINK_URL';
import NoDataComponent from '../Component/noData';
import SwithChart from '../Component/switchChart';
import './SprintReport.less';

const { AppState } = stores;
const { Option } = Select;
const { TabPane } = Tabs;

@observer
class SprintReport extends Component {
  constructor(props) {
    super(props);
    this.state = {
      xAxis: [],
      yAxis: [],
      select: 'issueCount',
      defaultSprint: '',
      loading: false,
      endDate: '',
      restDayShow: true,
      restDays: [],
      exportAxis: [],
      markAreaData: [],
    };
  }

  componentDidMount() {
    this.getDefaultSprintId();
    this.getSprintData();
    ReportStore.init();
  }

  getDefaultSprintId() {
    const { location: { search } } = this.props;
    if (search.lastIndexOf('sprintId') !== -1) {
      const searchNoExtra = search.substring(1);
      const { sprintId } = querystring.parse(searchNoExtra);
      this.setState({
        defaultSprint: sprintId,
      });
    }
  }

  getBetweenDateStr(start, end) {
    // 是否显示非工作日
    const { restDayShow, restDays } = this.state;
    const result = [];
    const rest = [];
    const beginDay = start.split('-');
    const endDay = end.split('-');
    const diffDay = new Date();
    const dateList = [];
    let i = 0;
    diffDay.setDate(beginDay[2]);
    diffDay.setMonth(beginDay[1] - 1);
    diffDay.setFullYear(beginDay[0]);
    while (i === 0) {
      const countDay = diffDay.getTime();
      if (restDays.includes(moment(diffDay).format('YYYY-MM-DD'))) {
        rest.push(moment(diffDay).format('YYYY-MM-DD'));
      }
      dateList[2] = diffDay.getDate();
      dateList[1] = diffDay.getMonth() + 1;
      dateList[0] = diffDay.getFullYear();
      if (String(dateList[1]).length === 1) {
        dateList[1] = `0${dateList[1]}`;
      }
      if (String(dateList[2]).length === 1) {
        dateList[2] = `0${dateList[2]}`;
      }
      if (restDayShow || !restDays.includes(moment(diffDay).format('YYYY-MM-DD'))) {
        result.push(`${dateList[0]}-${dateList[1]}-${dateList[2]}`);
      }
      diffDay.setTime(countDay + 24 * 60 * 60 * 1000);
      if (String(dateList[0]) === endDay[0]
        && String(dateList[1]) === endDay[1] && String(dateList[2]) === endDay[2]) {
        i = 1;
      }
    }
    return { result, rest };
  }

  getSprintData() {
    BurndownChartStore.axiosGetSprintList().then((res) => {
      const { defaultSprint } = this.state;
      BurndownChartStore.setSprintList(res);
      this.setState({
        defaultSprint: defaultSprint === '' ? res[0].sprintId : defaultSprint,
        endDate: defaultSprint === '' ? res[0].endDate : res.filter((item) => item.sprintId === defaultSprint)[0].endDate,
      }, () => {
        this.axiosGetRestDays();
      });
    }).catch((error) => {
    });
  }

  // axiosGetRestDays1 = () => {
  //   sprintApi.getRestDays(this.state.defaultSprint).then((res) => {
  //     this.setState({
  //       restDays: res.map((date) => moment(date).format('YYYY-MM-DD')),
  //     }, () => {
  //       this.getChartCoordinate();
  //     });
  //   });
  // };

  getChartCoordinate() {
    reportApi.loadBurnDownCoordinate(this.state.defaultSprint, this.state.select).then((res) => {
      const keys = Object.keys(res.coordinate);
      let [minDate, maxDate] = [keys[0], keys[0]];
      for (let a = 1, len = keys.length; a < len; a += 1) {
        if (moment(keys[a]).isAfter(maxDate)) {
          maxDate = keys[a];
        }
        if (moment(keys[a]).isBefore(minDate)) {
          minDate = keys[a];
        }
      }
      // 如果后端给的最大日期小于结束日期
      let allDate;
      let rest = [];
      if (moment(maxDate).isBefore(this.state.endDate.split(' ')[0])) {
        const result = this.getBetweenDateStr(minDate, this.state.endDate.split(' ')[0]);
        allDate = result.result;
        rest = result.rest;
      } else if (moment(minDate).isSame(maxDate)) {
        allDate = [minDate];
      } else {
        const result = this.getBetweenDateStr(minDate, maxDate);
        allDate = result.result;
        rest = result.rest;
      }
      // const allDate = this.getBetweenDateStr(minDate, maxDate);
      const allDateValues = [res.expectCount];
      const markAreaData = [];
      let exportAxisData = [res.expectCount];
      const { restDayShow } = this.state;
      // 如果展示非工作日，期望为一条连续斜线
      if (!restDayShow) {
        if (allDate.length) {
          exportAxisData = [
            ['', res.expectCount],
            [allDate[allDate.length - 1].split(' ')[0].slice(5).replace('-', '/'), 0],
          ];
        }
      }
      for (let b = 0, len = allDate.length; b < len; b += 1) {
        const nowKey = allDate[b];
        // 显示非工作日，则非工作日期望为水平线
        if (restDayShow) {
          // 工作日天数
          const countWorkDay = (allDate.length - rest.length) || 1;
          // 日工作量
          const dayAmount = res.expectCount / countWorkDay;
          if (rest.includes(allDate[b])) {
            // 非工作日
            if (b < len) {
              markAreaData.push([
                {
                  xAxis: b === 0 ? '' : allDate[b - 1].split(' ')[0].slice(5).replace('-', '/'),
                },
                {
                  xAxis: allDate[b].split(' ')[0].slice(5).replace('-', '/'),
                },
              ]);
            }
            exportAxisData[b + 1] = exportAxisData[b];
          } else {
            // 工作量取整
            exportAxisData[b + 1] = (exportAxisData[b] - dayAmount) < 0
              ? 0 : exportAxisData[b] - dayAmount;
          }
        }
        // eslint-disable-next-line no-prototype-builtins
        if (res.coordinate.hasOwnProperty(nowKey)) {
          allDateValues.push(res.coordinate[allDate[b]]);
        } else if (moment(nowKey).isAfter(moment())) {
          allDateValues.push(null);
        } else {
          const beforeKey = allDate[b - 1];
          allDateValues.push(res.coordinate[beforeKey]);
          res.coordinate[nowKey] = res.coordinate[beforeKey];
        }
      }
      const sliceDate = _.map(allDate, (item) => item.slice(5).replace('-', '/'));
      this.setState({
        xAxis: ['', ...sliceDate],
        yAxis: allDateValues,
        exportAxis: exportAxisData,
        markAreaData,
      });
    });
  }

  getMaxY() {
    const data = this.state.yAxis;
    let max = 0;
    for (let index = 0, len = data.length; index < len; index += 1) {
      if (data[index] > max) {
        max = data[index];
      }
    }
    return max;
  }

  getOption() {
    return {
      tooltip: {
        trigger: 'axis',
        backgroundColor: '#fff',
        textStyle: {
          color: '#000',
        },
        extraCssText:
          'box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2); border: 1px solid #ddd; border-radius: 0;',
        formatter(params) {
          let content = '';
          params.forEach((item) => {
            if (item.seriesName === '剩余值') {
              content = `${item.axisValue || '冲刺开启'}<br />${item.marker}${item.seriesName} : ${(item.value || item.value === 0) ? item.value : '-'}${item.value ? ' 个' : ''}`;
            }
          });
          return content;
        },
      },
      legend: {
        top: '24px',
        right: '0%',
        data: [{
          name: '期望值',
          icon: 'line',
        }, {
          name: '剩余值',
          icon: 'line',
        }],
      },
      grid: {
        y2: 30,
        top: '60',
        left: '20',
        right: '40',
        containLabel: true,
      },
      xAxis: {
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
          // eslint-disable-next-line radix
          interval: parseInt(this.state.xAxis.length / 7)
            ? parseInt(this.state.xAxis.length / 7, 10) - 1 : 0,
          textStyle: {
            color: 'rgba(0, 0, 0, 0.65)',
            fontSize: 12,
            fontStyle: 'normal',
          },
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
        type: 'category',
        boundaryGap: false,
        data: this.state.xAxis,
      },
      yAxis: {
        name: '问题计数',
        nameTextStyle: {
          color: '#000',
        },
        nameGap: 22,
        type: 'value',
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
            color: 'rgba(0, 0, 0, 0.65)',
            fontSize: 12,
            fontStyle: 'normal',
          },
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
      },
      series: [
        {
          symbol: 'none',
          name: '期望值',
          type: 'line',
          data: this.state.exportAxis,
          itemStyle: {
            color: 'rgba(0,0,0,0.65)',
          },
          lineStyle: {
            type: 'dotted',
            color: 'rgba(0,0,0,0.65)',
          },
          markArea: {
            itemStyle: {
              color: 'rgba(235,235,235,0.65)',
            },
            emphasis: {
              itemStyle: {
                color: 'rgba(220,220,220,0.65)',
              },
            },
            data: this.state.markAreaData,
          },
        },
        {
          symbol: 'none',
          name: '剩余值',
          type: 'line',
          itemStyle: {
            color: '#4f9bff',
          },
          // stack: '总量',
          data: this.state.yAxis,
        },
      ],
    };
  }

  callback = (key) => {
    ReportStore.setActiveKey(key);
    const ARRAY = {
      done: {
        func: 'loadDoneIssues',
        page: 0,
        size: 10,
      },
      todo: {
        func: 'loadTodoIssues',
        page: 0,
        size: 10,
      },
      remove: {
        func: 'loadRemoveIssues',
        page: 0,
        size: 10,
      },
    };
    if (ReportStore.currentSprint.sprintId) {
      ReportStore[ARRAY[key].func](ARRAY[key].page, ARRAY[key].size);
    }
  }

  onCheckChange = (e) => {
    this.setState({
      restDayShow: e.target.checked,
    }, () => {
      this.getChartCoordinate();
    });
  };

  axiosGetRestDays() {
    sprintApi.getRestDays(this.state.defaultSprint).then((res) => {
      this.setState({
        restDays: res.map((date) => moment(date).format('YYYY-MM-DD')),
      }, () => {
        this.getChartCoordinate();
      });
    });
  }

  renderTodoIssue(column) {
    return (
      <div>
        <Table
          rowKey={(record) => record.issueId}
          dataSource={ReportStore.todoIssues}
          pagination={ReportStore.todoPagination}
          columns={column}
          filterBar={false}
          scroll={{ x: true }}
          loading={ReportStore.loading}
          onChange={(pagination, filters, sorter) => {
            ReportStore.setTodoPagination(Pagination);
            ReportStore.loadTodoIssues(pagination.current - 1, pagination.pageSize);
          }}
        />
      </div>
    );
  }

  renderDoneIssue(column) {
    return (
      <div>
        <Table
          rowKey={(record) => record.issueId}
          dataSource={ReportStore.doneIssues}
          pagination={ReportStore.donePagination}
          columns={column}
          filterBar={false}
          scroll={{ x: true }}
          loading={ReportStore.loading}
          onChange={(pagination, filters, sorter) => {
            ReportStore.setDonePagination(pagination);
            ReportStore.loadDoneIssues(pagination.current - 1, pagination.pageSize);
          }}
        />
      </div>
    );
  }

  renderRemoveIssue(column) {
    return (
      <div>
        <Table
          rowKey={(record) => record.issueId}
          dataSource={ReportStore.removeIssues}
          pagination={ReportStore.removePagination}
          columns={column}
          filterBar={false}
          scroll={{ x: true }}
          loading={ReportStore.loading}
          onChange={(pagination, filters, sorter) => {
            ReportStore.setRemovePagination(Pagination);
            ReportStore.removePagination(pagination.current - 1, pagination.pageSize);
          }}
        />
      </div>
    );
  }

  render() {
    const column = [
      {
        width: '15%',
        title: '问题编号',
        dataIndex: 'issueNum',
        render: (issueNum, record) => (
          <Tooltip mouseEnterDelay={0.5} title={`问题编号：${issueNum}`}>
            <span
              className="primary"
              style={{
                display: 'block',
                minWidth: '85px',
                cursor: 'pointer',
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
          </Tooltip>
        ),
      }, {
        width: '30%',
        title: '概要',
        dataIndex: 'summary',
        render: (summary, record) => (
          <Tooltip MouseEnterDelay={0.5} title={`问题概要：${record.summary}`}>
            <div
              style={{
                overflow: 'hidden',
                maxWidth: 260,
                whiteSpace: 'nowrap',
                textOverflow: 'ellipsis',
              }}
            >
              {record.summary}
            </div>
          </Tooltip>
        ),
      }, {
        width: '15%',
        title: '问题类型',
        dataIndex: 'typeCode',
        render: (typeCode, record) => (
          <div>
            <Tooltip mouseEnterDelay={0.5} title={`任务类型： ${record.issueTypeVO.name}`}>
              <div>
                <TypeTag
                  style={{ minWidth: 90 }}
                  data={record.issueTypeVO}
                  showName
                />
              </div>
            </Tooltip>
          </div>
        ),
      }, {
        width: '15%',
        title: '优先级',
        dataIndex: 'priorityId',
        render: (priorityId, record) => (
          <div>
            <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`优先级： ${record.priorityVO.name}`}>
              <div style={{ marginRight: 12 }}>
                <PriorityTag
                  style={{ minWidth: 55 }}
                  priority={record.priorityVO}
                />
              </div>
            </Tooltip>
          </div>
        ),
      }, {
        width: '15%',
        title: '状态',
        dataIndex: 'statusCode',
        render: (statusCode, record) => (
          <div>
            <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`任务状态： ${record.statusVO.name}`}>
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
      }, {
        width: '10%',
        title: '故事点(个)',
        dataIndex: 'storyPoints',
        render: (storyPoints, record) => (
          <div style={{ minWidth: 15 }}>
            {record.typeCode === 'story' ? storyPoints || '0' : ''}
          </div>
        ),
      },
    ];

    const urlParams = AppState.currentMenuType;
    return (
      <Page className="c7n-report" service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.sprintreport']}>
        <Header
          title="冲刺报告图"
          backPath={`/charts?type=${urlParams.type}&id=${urlParams.id}&name=${encodeURIComponent(urlParams.name)}&organizationId=${urlParams.organizationId}&orgId=${urlParams.organizationId}`}

        >
          <SwithChart
            current="sprint"
          />
          <Button
            funcType="flat"
            onClick={() => {
              this.axiosGetRestDays();
              ReportStore.changeCurrentSprint(ReportStore.currentSprint.sprintId);
            }}
          >
            <Icon type="refresh icon" />
            <span>刷新</span>
          </Button>
        </Header>
        <Breadcrumb title="冲刺报告图" />
        <Content>
          <Spin spinning={this.state.loading}>
            {
              BurndownChartStore.getSprintList.length > 0 ? (
                <div>
                  <div>
                    <Select
                      getPopupContainer={(triggerNode) => triggerNode.parentNode}
                      style={{ width: 244 }}
                      label="迭代冲刺"
                      value={this.state.defaultSprint}
                      onChange={(value) => {
                        ReportStore.changeCurrentSprint(value);
                        ReportStore.setDonePagination({
                          current: 0,
                          pageSize: 10,
                          total: undefined,
                        });
                        ReportStore.setTodoPagination({
                          current: 0,
                          pageSize: 10,
                          total: undefined,
                        });
                        ReportStore.setRemovePagination({
                          current: 0,
                          pageSize: 10,
                          total: undefined,
                        });
                        let endDate;
                        for (let index = 0, len = BurndownChartStore.getSprintList.length;
                          index < len; index += 1) {
                          if (BurndownChartStore.getSprintList[index].sprintId === value) {
                            endDate = BurndownChartStore.getSprintList[index].endDate;
                          }
                        }
                        this.setState({
                          defaultSprint: value,
                          endDate,
                        }, () => {
                          this.axiosGetRestDays();
                        });
                      }}
                    >
                      {BurndownChartStore.getSprintList.length > 0
                        ? BurndownChartStore.getSprintList.map((item) => (
                          <Option value={item.sprintId}>{item.sprintName}</Option>
                        )) : ''}
                    </Select>
                    <Checkbox
                      style={{ marginLeft: 24 }}
                      checked={this.state.restDayShow}
                      onChange={this.onCheckChange}
                    >
                      显示非工作日
                    </Checkbox>
                    <div className="c7n-sprintMessage">
                      <div className="c7n-sprintContent">
                        <span>
                          {ReportStore.getCurrentSprintStatus.status}
                          {'冲刺,'}
                          {'共'}
                          {ReportStore.currentSprint.issueCount || 0}
                          {'个问题'}
                        </span>
                        <span>
                          {`${commonformatDate(ReportStore.currentSprint.startDate)} - ${commonformatDate(ReportStore.currentSprint.actualEndDate) || '至今'}`}
                        </span>
                      </div>
                      <p
                        className="primary"
                        style={{
                          cursor: 'pointer',
                        }}
                        role="none"
                        onClick={() => {
                          to(LINK_URL.workListIssue, {
                            params: {
                              paramType: 'sprint',
                              paramId: ReportStore.currentSprint.sprintId,
                              paramName: `${ReportStore.currentSprint.sprintName}下的问题`,
                            },
                          });
                        }}
                      >
                        在“问题管理中”查看
                        <Icon style={{ fontSize: 13, verticalAlign: -2 }} type="open_in_new" />
                      </p>
                    </div>
                  </div>
                  <ReactEcharts option={this.getOption()} />
                  <Tabs activeKey={ReportStore.activeKey} onChange={this.callback}>
                    <TabPane tab="已完成的问题" key="done">
                      {this.renderDoneIssue(column)}
                    </TabPane>
                    <TabPane tab="未完成的问题" key="todo">
                      {this.renderTodoIssue(column)}
                    </TabPane>
                    <TabPane tab="从Sprint中移除的问题" key="remove">
                      {this.renderRemoveIssue(column)}
                    </TabPane>
                  </Tabs>
                </div>
              ) : (
                <NoDataComponent title="冲刺" links={[{ name: '待办事项', link: LINK_URL.workListBacklog }]} img={epicSvg} />
              )
            }
          </Spin>
        </Content>
      </Page>
    );
  }
}

export default SprintReport;
