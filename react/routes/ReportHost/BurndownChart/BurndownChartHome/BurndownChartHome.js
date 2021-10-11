import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Checkbox,
} from 'choerodon-ui';
import {
  Select,
} from 'choerodon-ui/pro';
import {
  Page, Header, Content, Breadcrumb, stores, HeaderButtons,
} from '@choerodon/boot';
import { some } from 'lodash';
import Moment from 'moment';
import { extendMoment } from 'moment-range';
import { sprintApi, reportApi } from '@/api';
import LINK_URL from '@/constants/LINK_URL';
import BurnDownChart from '@/components/charts/burn-down';
import IssueSearch, { IssueSearchStore } from '@/components/issue-search';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import BurndownChartStore from '@/stores/project/burndownChart/BurndownChartStore';
import epicSvg from '@/assets/image/NoData.svg';
import NoDataComponent from '../../Component/noData';
import SwithChart from '../../Component/switchChart';
import BurndownTable from './components/burndown-table';
import './BurndownChartHome.less';
import BackBtn from '../../back-btn';
import { Loading } from '@/components';
import { LoadingHiddenWrap, LoadingProvider } from '@/components/Loading';

const { AppState } = stores;
const moment = extendMoment(Moment);
const { Option } = Select;

@observer
class BurndownChartHome extends Component {
  constructor(props) {
    super(props);
    this.state = {
      select: 'remainingEstimatedTime',
      defaultSprintId: '',
      chartLoading: false,
      tableLoading: false,
      endDate: '',
      restDayShow: true,
      restDays: [],
      chartData: null,
      quickFilter: {
        onlyMe: false,
        onlyStory: false,
        personalFilters: [],
        quickFilters: [],
      },
    };
    this.issueSearchStore = new IssueSearchStore({
      transformFilter,
      getSystemFields: () => getSystemFields().filter((f) => !['contents', 'sprint', 'quickFilterIds'].includes(f.code)),
    });
  }

  componentDidMount() {
    this.getSprintData();
  }

  getSprintData() {
    BurndownChartStore.axiosGetSprintList().then((res) => {
      BurndownChartStore.setSprintList(res);
      const defaultSprint = res.find((item) => item.statusCode === 'started') || res[0]; // 如果有活跃冲刺就展示活跃冲刺否则就展示第一个
      this.setState({
        defaultSprintId: defaultSprint.sprintId,
        endDate: defaultSprint.endDate,
      }, () => {
        if (this.state.defaultSprintId) {
          this.getTableData();
          this.axiosGetRestDays();
        }
      });
    }).catch((error) => {
    });
  }

  getChartCoordinate() {
    const {
      defaultSprintId, select, quickFilter,
    } = this.state;
    const searchVO = this.issueSearchStore.getCustomFieldFilters();
    this.setState({ chartLoading: true });
    reportApi.loadBurnDownCoordinate(defaultSprintId, select, {
      assigneeId: quickFilter.onlyMe ? AppState.getUserId : undefined,
      onlyStory: quickFilter.onlyStory,
      quickFilterIds: quickFilter.quickFilters,
      personalFilterIds: quickFilter.personalFilters,
    }, searchVO).then((res) => {
      this.setState({
        chartLoading: false,
        chartData: res,
      });
    });
  }

  getTableData() {
    const {
      defaultSprintId, select, quickFilter,
    } = this.state;
    const searchVO = this.issueSearchStore.getCustomFieldFilters();
    this.setState({
      tableLoading: true,
    });
    reportApi.loadSprintBurnDown(defaultSprintId, select, 'asc', {
      assigneeId: quickFilter.onlyMe ? AppState.getUserId : undefined,
      onlyStory: quickFilter.onlyStory,
      quickFilterIds: quickFilter.quickFilters,
      personalFilterIds: quickFilter.personalFilters,
    }, searchVO).then((res) => {
      const data = res;
      const newData = [];
      // 将操作日期相同的合并
      for (let index = 0, len = data.length; index < len; index += 1) {
        if (!some(newData, { date: data[index].date })) {
          newData.push({
            date: data[index].date,
            issues: [{
              issueId: data[index].issueId,
              issueNum: data[index].issueNum,
              newValue: data[index].newValue,
              oldValue: data[index].oldValue,
              statistical: data[index].statistical,
              parentIssueId: data[index].parentIssueId,
              parentIssueNum: data[index].parentIssueNum,
            }],
            type: data[index].type,
          });
        } else {
          let index2;
          for (let i = 0, len2 = newData.length; i < len2; i += 1) {
            if (newData[i].date === data[index].date) {
              index2 = i;
            }
          }
          if (newData[index2].type.indexOf(data[index].type) === -1) {
            newData.push({
              date: data[index].date,
              issues: [{
                issueId: data[index].issueId,
                issueNum: data[index].issueNum,
                newValue: data[index].newValue,
                oldValue: data[index].oldValue,
                statistical: data[index].statistical,
                parentIssueId: data[index].parentIssueId,
                parentIssueNum: data[index].parentIssueNum,
              }],
              type: data[index].type,
            });
          } else {
            newData[index2].issues = [...newData[index2].issues, {
              issueId: data[index].issueId,
              issueNum: data[index].issueNum,
              newValue: data[index].newValue,
              oldValue: data[index].oldValue,
              statistical: data[index].statistical,
              parentIssueId: data[index].parentIssueId,
              parentIssueNum: data[index].parentIssueNum,
            }];
          }
        }
      }
      // 计算剩余
      for (let index = 0, dataLen = newData.length; index < dataLen; index += 1) {
        let rest = 0;
        if (newData[index].type !== 'endSprint') {
          if (index > 0) {
            rest = newData[index - 1].rest;
          }
        }
        for (let i = 0, len = newData[index].issues.length; i < len; i += 1) {
          if (newData[index].issues[i].statistical) {
            rest += newData[index].issues[i].newValue - newData[index].issues[i].oldValue;
            if (rest % 1 > 0) {
              // 如果计算结果为小数，利用toFixed消除js计算精度bug
              rest = rest.toFixed(1) * 1;
            }
          }
        }
        newData[index].rest = rest;
      }
      // TODO: 优化合并逻辑
      // const grouped = groupBy(res, 'date');
      // const data2 = Object.keys(grouped).map((key) => {
      //   const issues = grouped[key];
      //   return {
      //     date: issues[0].date,
      //     issues,
      //     type: issues[0].type,
      //   };
      // });
      // console.log(newData, data2);
      BurndownChartStore.setBurndownList(newData);
      this.setState({
        tableLoading: false,
      });
    }).catch((error) => {
      console.log(error);
    });
  }

  axiosGetRestDays = () => {
    sprintApi.getRestDays(this.state.defaultSprintId).then((res) => {
      this.setState({
        restDays: res.map((date) => moment(date).format('YYYY-MM-DD')),
      }, () => {
        this.getChartCoordinate();
      });
    });
  };

  handleChangeSelect = (value) => {
    this.setState({
      select: value,
    }, () => {
      this.getTableData();
      this.getChartCoordinate();
    });
  }

  handleQuickSearchChange = (value) => {
    this.setState({
      quickFilter: value,
    }, () => {
      this.getTableData();
      this.getChartCoordinate();
    });
  }

  onCheckChange = (e) => {
    this.setState({
      restDayShow: e.target.checked,
    }, () => {
      this.getChartCoordinate();
    });
  };

  refresh = () => {
    this.getTableData();
    this.axiosGetRestDays();
  }

  render() {
    const {
      select, chartLoading, chartData, endDate, restDayShow, restDays, tableLoading,
    } = this.state;
    const sprints = BurndownChartStore.getSprintList;
    return (
      <Page>
        <Header
          title="燃尽图"
        >

          <Checkbox
            style={{ marginLeft: 24 }}
            checked={restDayShow}
            onChange={this.onCheckChange}
          >
            显示非工作日
          </Checkbox>
          <HeaderButtons
            items={[{
              name: '切换',
              element: <SwithChart
                current="burndownchart"
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
                this.getTableData();
                this.axiosGetRestDays();
              },
              display: true,
            }]}
          />
        </Header>
        <Breadcrumb title="燃尽图" />
        <Content>
          <LoadingProvider loading={chartLoading}>
            {
            sprints.length > 0 ? (
              <div style={{ width: '100%' }}>
                <div style={{
                  display: 'flex', alignItems: 'flex-start', width: '100%',
                }}
                >
                  <Select
                    
                    style={{
                      width: 150,
                      flexShrink: 0,
                      marginTop: 10,
                      marginRight: 5,
                    }}
                    label="迭代冲刺"
                    placeholder="迭代冲刺"
                    dropdownMatchSelectWidth={false}
                    value={this.state.defaultSprintId}
                    onChange={(value) => {
                      let newEndDate;
                      for (let index = 0, len = sprints.length; index < len; index += 1) {
                        if (sprints[index].sprintId === value) {
                          newEndDate = sprints[index].endDate;
                        }
                      }
                      this.setState({
                        defaultSprintId: value,
                        endDate: newEndDate,
                      }, () => {
                        this.getTableData();
                        this.axiosGetRestDays();
                      });
                    }}
                    clearButton={false}
                  >
                    {sprints.length > 0
                      ? sprints.map((item) => (
                        <Option key={item.sprintId} value={item.sprintId}>{item.sprintName}</Option>
                      ))
                      : ''}
                  </Select>
                  <Select
                    
                    style={{ marginLeft: 10, marginTop: 10, marginRight: 15 }}
                    label="单位"
                    value={this.state.select}
                    onChange={this.handleChangeSelect}
                    dropdownMatchSelectWidth={false}
                    clearButton={false}
                  >
                    <Option value="remainingEstimatedTime">剩余时间</Option>
                    <Option value="storyPoints">故事点</Option>
                    <Option value="issueCount">工作项计数</Option>
                  </Select>
                  <div
                    style={{ flexShrink: 1 }}
                  >
                    <IssueSearch
                      store={this.issueSearchStore}
                      onClear={this.refresh}
                      onChange={this.refresh}
                    />
                  </div>
                </div>
                <BurnDownChart
                  type={select}
                  loading={false}
                  data={chartData}
                  endDate={endDate}
                  restDayShow={restDayShow}
                  restDays={restDays}
                />
                <BurndownTable
                  data={BurndownChartStore.getBurndownList}
                  loading={false}
                  select={select}
                />
                <Loading loading={tableLoading} />
              </div>
            ) : (
              <LoadingHiddenWrap>
                <NoDataComponent title="冲刺" links={[{ name: '待办事项', link: LINK_URL.workListBacklog }]} img={epicSvg} />
              </LoadingHiddenWrap>
            )
          }
          </LoadingProvider>
        </Content>
      </Page>
    );
  }
}

export default BurndownChartHome;
