/* eslint-disable react/destructuring-assignment */
/* eslint-disable prefer-destructuring */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Spin, Icon, Table, Checkbox, Tabs, Tooltip, Pagination,
} from 'choerodon-ui';
import { Form, Select } from 'choerodon-ui/pro';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import moment from 'moment';
import querystring from 'querystring';
import BurndownChartStore from '@/stores/project/burndownChart/BurndownChartStore';
import ReportStore from '@/stores/project/Report';
import { commonformatDate } from '@/utils/Date';
import epicSvg from '@/assets/image/NoData.svg';
import StatusTag from '@/components/StatusTag';
import PriorityTag from '@/components/PriorityTag';
import TypeTag from '@/components/TypeTag';
import BurnDownChart from '@/components/charts/burn-down';
import STATUS from '@/constants/STATUS';
import { sprintApi, reportApi } from '@/api';
import to from '@/utils/to';
import LINK_URL, { LINK_URL_TO } from '@/constants/LINK_URL';
import SwithChart from '../Component/switchChart';
import NoDataComponent from '../Component/noData';
import BackBtn from '../back-btn';
import './SprintReport.less';
import Loading, { LoadingHiddenWrap, LoadingProvider } from '@/components/Loading';

const { Option } = Select;
const { TabPane } = Tabs;

@observer
class SprintReport extends Component {
  constructor(props) {
    super(props);
    this.state = {
      select: 'issueCount',
      defaultSprint: '',
      loading: false,
      endDate: '',
      restDayShow: true,
      restDays: [],
      chartData: null,
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

  getChartCoordinate() {
    reportApi.loadBurnDownCoordinate(this.state.defaultSprint, this.state.select).then((res) => {
      this.setState({
        chartData: res,
      });
    });
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
          loading={false}
          onChange={(pagination, filters, sorter) => {
            ReportStore.setTodoPagination(Pagination);
            ReportStore.loadTodoIssues(pagination.current, pagination.pageSize);
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
          loading={false}
          onChange={(pagination, filters, sorter) => {
            ReportStore.setDonePagination(pagination);
            ReportStore.loadDoneIssues(pagination.current, pagination.pageSize);
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
          loading={false}
          onChange={(pagination, filters, sorter) => {
            ReportStore.setRemovePagination(Pagination);
            ReportStore.loadRemoveIssues(pagination.current, pagination.pageSize);
          }}
        />
      </div>
    );
  }

  render() {
    const {
      select, chartData, endDate, restDayShow, restDays,
    } = this.state;
    const column = [
      {
        width: '15%',
        title: '工作项编号',
        dataIndex: 'issueNum',
        render: (issueNum, record) => (
          <Tooltip mouseEnterDelay={0.5} title={`工作项编号：${issueNum}`}>
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
          <Tooltip MouseEnterDelay={0.5} title={`工作项概要：${record.summary}`}>
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
        title: '工作项类型',
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
            <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`优先级： ${record.priorityVO?.name}`}>
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

    return (
      <Page className="c7n-report">
        <Header
          title="冲刺报告图"
        >
          <HeaderButtons
            items={[{
              name: '切换',
              element: <SwithChart
                current="sprint"
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
                this.axiosGetRestDays();
                ReportStore.changeCurrentSprint(ReportStore.currentSprint.sprintId);
              },
              display: true,
            }]}
          />
        </Header>
        <Breadcrumb title="冲刺报告图" />
        <Content style={{ paddingTop: 20 }}>
          <LoadingProvider loading={this.state.loading}>
            {
              BurndownChartStore.getSprintList.length > 0 ? (
                <div>
                  <div>
                    <div style={{ display: 'flex', alignItems: 'center' }}>
                      <Form style={{ width: 244, marginBottom: -20 }}>
                        <Select
                          clearButton={false}
                          
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
                            let newEndDate;
                            for (let index = 0, len = BurndownChartStore.getSprintList.length;
                              index < len; index += 1) {
                              if (BurndownChartStore.getSprintList[index].sprintId === value) {
                                newEndDate = BurndownChartStore.getSprintList[index].endDate;
                              }
                            }
                            this.setState({
                              defaultSprint: value,
                              endDate: newEndDate,
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
                      </Form>
                      <Checkbox
                        style={{ marginLeft: 24 }}
                        checked={this.state.restDayShow}
                        onChange={this.onCheckChange}
                      >
                        显示非工作日
                      </Checkbox>
                      <p
                        className="primary"
                        style={{
                          cursor: 'pointer',
                          marginLeft: 'auto',
                        }}
                        role="none"
                        onClick={() => {
                          to(LINK_URL.workListIssue, {
                            params: {
                              paramType: 'sprint',
                              paramId: ReportStore.currentSprint.sprintId,
                              paramName: `${ReportStore.currentSprint.sprintName}下的工作项`,
                            },
                          }, { blank: true });
                        }}
                      >
                        在“所有工作项中”查看
                        <Icon style={{ fontSize: 13, verticalAlign: -2 }} type="open_in_new" />
                      </p>
                    </div>
                    <div className="c7n-sprintMessage">
                      <div className="c7n-sprintContent">
                        <span style={{ marginRight: 50 }}>
                          {ReportStore.getCurrentSprintStatus.status}
                          {'冲刺,'}
                          {'共'}
                          {ReportStore.currentSprint.issueCount || 0}
                          {'个工作项'}
                        </span>
                        <span>
                          {`${commonformatDate(ReportStore.currentSprint.startDate)} - ${commonformatDate(ReportStore.currentSprint.actualEndDate) || '至今'}`}
                        </span>
                      </div>
                    </div>
                  </div>
                  <BurnDownChart
                    select={select}
                    loading={false}
                    data={chartData}
                    endDate={endDate}
                    restDayShow={restDayShow}
                    restDays={restDays}
                  />
                  <Loading loading={ReportStore.loading} />
                  <Tabs activeKey={ReportStore.activeKey} onChange={this.callback}>
                    <TabPane tab="已完成的工作项" key="done">
                      {this.renderDoneIssue(column)}
                    </TabPane>
                    <TabPane tab="未完成的工作项" key="todo">
                      {this.renderTodoIssue(column)}
                    </TabPane>
                    <TabPane tab="从Sprint中移除的工作项" key="remove">
                      {this.renderRemoveIssue(column)}
                    </TabPane>
                  </Tabs>
                </div>
              ) : (
                <LoadingHiddenWrap><NoDataComponent title="冲刺" links={[{ name: '待办事项', link: LINK_URL.workListBacklog }]} img={epicSvg} /></LoadingHiddenWrap>
              )
            }
          </LoadingProvider>
        </Content>
      </Page>
    );
  }
}

export default SprintReport;
