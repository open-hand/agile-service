/* eslint-disable react/destructuring-assignment */
/* eslint-disable react/sort-comp */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Button, Spin, Icon, Select, Table, Checkbox, Tooltip,
} from 'choerodon-ui';
import {
  Page, Header, Content, stores, Breadcrumb,
} from '@choerodon/boot';
import ReactEcharts from 'echarts-for-react';
import _ from 'lodash';
import Moment from 'moment';
import { extendMoment } from 'moment-range';
import { sprintApi, reportApi } from '@/api';
import LINK_URL, { LINK_URL_TO } from '@/constants/LINK_URL';
import QuickSearch from '@/components/quick-search';
import BurndownChartStore from '../../../../stores/project/burndownChart/BurndownChartStore';
import './BurndownChartHome.less';
import NoDataComponent from '../../Component/noData';
import epicSvg from '../../../../assets/image/emptyChart.svg';
import SwithChart from '../../Component/switchChart';

const moment = extendMoment(Moment);
const { AppState } = stores;
const { Option } = Select;

@observer
class BurndownChartHome extends Component {
  constructor(props) {
    super(props);
    this.state = {
      xAxis: [],
      yAxis: [],
      select: 'remainingEstimatedTime',
      defaultSprintId: '',
      chartLoading: true,
      tableLoading: true,
      endDate: '',
      linkFromParamUrl: undefined,
      restDayShow: true,
      restDays: [],
      exportAxis: [],
      markAreaData: [],
      dateSort: 'asc',
      quickFilter: {
        onlyMe: false,
        onlyStory: false,
        personalFilters: [],
        quickFilters: [],
      },
    };
  }

  componentDidMount() {
    const { location: { search } } = this.props;
    // const linkFromParamUrl = _.last(search.split('&')).split('=')[1];
    const linkFromParamUrl = _.last(search.split('&')).split('=')[0] === 'paramUrl' ? _.last(search.split('&')).split('=')[1] : undefined;
    this.setState({
      linkFromParamUrl,
    });
    this.getSprintData();
  }

  getBetweenDateStr(start, end) {
    // 是否显示非工作日
    const { restDays } = this.state;
    const range = moment.range(start, end);
    const days = Array.from(range.by('day'));
    const result = days.map((day) => day.format('YYYY-MM-DD'));
    const rest = days.filter((day) => restDays.includes(day.format('YYYY-MM-DD'))).map((day) => day.format('YYYY-MM-DD'));
    return { result, rest };
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
          this.getChartData();
          this.axiosGetRestDays();
        }
      });
    }).catch((error) => {
    });
  }

  getChartCoordinate() {
    this.setState({ chartLoading: true });
    reportApi.loadBurnDownCoordinate(this.state.defaultSprintId, this.state.select).then((res) => {
      this.setState({
        chartLoading: false,
      });
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
      /* eslint-disable */
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
            exportAxisData[b + 1] = (exportAxisData[b] - dayAmount) < 0 ? 0 : exportAxisData[b] - dayAmount;
          }
        }
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
      const sliceDate = _.map(allDate, item => item.slice(5).replace('-', '/'));
      this.setState({
        xAxis: ['', ...sliceDate],
        yAxis: allDateValues,
        exportAxis: exportAxisData,
        markAreaData,
      });
    });
  }

  getChartData() {
    this.setState({
      tableLoading: true,
    });
    reportApi.loadSprintBurnDown(this.state.defaultSprintId, this.state.select).then((res) => {
      const data = res;
      const newData = [];
      // 将操作日期相同的合并
      for (let index = 0, len = data.length; index < len; index += 1) {
        if (!_.some(newData, { date: data[index].date })) {
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
      BurndownChartStore.setBurndownList(newData);
      this.setState({
        tableLoading: false,
      });
    }).catch((error) => {
      console.log(error)
    });
  }

  getOption() {
    const { select } = this.state;
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
          let unit = '';
          params.forEach((item) => {
            if (item.seriesName === '剩余值') {
              if (item.value && select === 'remainingEstimatedTime') {
                unit = ' 小时';
              }
              if (item.value && select === 'storyPoints') {
                unit = ' 点';
              }
              if (item.value && select === 'issueCount') {
                unit = ' 个';
              }
              content = `${item.axisValue || '冲刺开启'}<br />${item.marker}${item.seriesName} : ${(item.value || item.value === 0) ? item.value : '-'}${unit && unit}`;
            }
          });
          return content;
        },
      },
      legend: {
        top: '24px',
        right: '3.2%',
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
        // left: '20',
        left: 0,
        right: '40',
        containLabel: true,
      },
      xAxis: {
        type: 'category',
        boundaryGap: false,
        data: this.state.xAxis,
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
          interval: parseInt(this.state.xAxis.length / 7) ? parseInt(this.state.xAxis.length / 7) - 1 : 0,
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
      yAxis: {
        name: this.renderChartTitle(),
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
          formatter(value, index) {
            if (select === 'remainingEstimatedTime' && value) {
              return `${value}h`;
            } else {
              return value;
            }
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
            color: '#4D90FE',
          },
          // stack: '总量',
          data: this.state.yAxis,
        },
      ],
    };
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
  handleChangeSelect(value) {
    this.setState({
      select: value,
    }, () => {
      this.getChartData();
      this.getChartCoordinate();
    });
  }

  handleQuickSearchChange = (value) => {
    this.setState({
      quickFilter: value
    }, () => {
      this.getChartData()
    })
  }
  renderChartTitle() {
    let result = '';
    if (this.state.select === 'remainingEstimatedTime') {
      result = '剩余时间';
    }
    if (this.state.select === 'storyPoints') {
      result = '故事点';
    }
    if (this.state.select === 'issueCount') {
      result = '问题计数';
    }
    return result;
  }

  renderDetail(item, record) {
    let result = '-';
    if (record.type !== 'startSprint' && record.type !== 'endSprint') {
      if (item.statistical) {
        if (item.oldValue !== item.newValue) {
          result = `由${item.oldValue}到${item.newValue}`;
        }
      }
    }
    return (
      <p style={{
        maxWidth: '100px',
        whiteSpace: 'nowrap',
        textOverflow: 'ellipsis',
        overflow: 'hidden',
      }}
      >
        {result}
      </p>
    );
  }

  renderUp(item) {
    let result = '-';
    if (item.newValue > item.oldValue) {
      if (item.statistical) {
        result = item.newValue - item.oldValue;
        if (result && result % 1 > 0) {
          result = result.toFixed(1);
        }
      }
    }
    return result;
  }

  renderDown(item) {
    let result = '-';
    if (item.newValue < item.oldValue) {
      if (item.statistical) {
        result = item.oldValue - item.newValue;
        if (result && result % 1 > 0) {
          result = result.toFixed(1);
        }
      }
    }
    return result;
  }

  judgeText(text) {
    let result = '';
    if (text === 'startSprint') {
      result = '开启冲刺';
    }
    if (text === 'addDuringSprint') {
      result = '在冲刺期间添加';
    }
    if (text === 'removeDoneDuringSprint') {
      result = '在冲刺期间从已完成到一个其他状态';
    }
    if (text === 'timespent') {
      result = '用户登记工作日志';
    }
    if (text === 'removeDuringSprint') {
      result = '冲刺中移除';
    }
    if (text === 'endSprint') {
      result = '关闭冲刺';
    }
    if (text === 'addDoneDuringSprint') {
      result = '在冲刺期间移动到已完成';
    }
    if (text === 'timeestimate') {
      result = '用户修改剩余时间';
    }
    if (text === 'valueChange') {
      if (this.state.select === 'remainingEstimatedTime') {
        result = '用户修改预估时间';
      } else if (this.state.select === 'storyPoints') {
        result = '用户修改故事点';
      } else {
        result = '用户修改问题计数';
      }
    }
    return result;
  }

  renderTypeText(text) {
    const splitArray = text.split('-');
    return (
      <div>
        {
          splitArray.map(item => (
            <Tooltip mouseEnterDelay={0.5} title={`事件类型：${this.judgeText(item)}`}>
              <p style={{
                maxWidth: 150, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
              }}
              >
                {this.judgeText(item)}

              </p>
            </Tooltip>
          ))
        }
      </div>
    );
  }

  onCheckChange = (e) => {
    this.setState({
      restDayShow: e.target.checked,
    }, () => {
      this.getChartCoordinate();
    });
  };

  // handleChangeSort = () => {
  //   const { dateSort } = this.state;
  //   this.setState({
  //     dateSort: dateSort === 'asc' ? 'desc' : 'asc',
  //   }, () => {
  //     this.getChartData();
  //   })
  // }

  render() {
    const { select, dateSort } = this.state;
    let unit = '';
    if (select === 'remainingEstimatedTime') {
      unit = '(小时)';
    }
    if (select === 'storyPoints') {
      unit = '(点)';
    }
    if (select === 'issueCount') {
      unit = '(个)';
    }
    const columns = [{
      // title: (
      //   <span>日期<Icon type={ dateSort === 'asc' ? 'arrow_upward' : 'arrow_downward' } style={{ cursor: 'pointer', marginTop: -4, marginLeft: 4 }} onClick={this.handleChangeSort} /></span>
      // ),
      title: '日期',
      dataIndex: 'date',
      key: 'date',
      width: '16%',
      render: text => (
        <Tooltip mouseEnterDelay={0.5} title={`日期：${text}`}>
          <p style={{
            maxWidth: '130px',
            whiteSpace: 'nowrap',
            textOverflow: 'ellipsis',
            overflow: 'hidden',
          }}
          >
            {text}
          </p>
        </Tooltip>
      ),
    }, {
      title: '问题',
      dataIndex: 'issues',
      key: 'issues',
      width: '22%',
      render: (text, record) => (
        <div>
          {
            text.map(item => (
              <p
                className="primary"
                style={{
                  maxWidth: '130px',
                  whiteSpace: 'nowrap',
                  textOverflow: 'ellipsis',
                  overflow: 'hidden',
                  cursor: 'pointer',
                }}
                role="none"
                onClick={() => {
                  const urlParams = AppState.currentMenuType;
                  if (item.parentIssueId) {
                    LINK_URL_TO.issueLinkTo(item.parentIssueId, item.issueNum, { paramOpenIssueId: item.issueId });
                  } else {
                    LINK_URL_TO.issueLinkTo(item.issueId, item.issueNum, { paramOpenIssueId: item.issueId });
                  }
                }}
              >
                <Tooltip mouseEnterDelay={0.5} title={`问题：${item.parentIssueId ? `${item.parentIssueNum}/${item.issueNum}` : item.issueNum}`}>
                  {item.parentIssueId ? `${item.parentIssueNum}/${item.issueNum}` : item.issueNum}
                </Tooltip>
              </p>
            ))
          }
        </div>
      ),
    }, {
      title: '事件类型',
      dataIndex: 'type',
      key: 'type',
      width: '17%',
      render: text => (
        <div>{this.renderTypeText(text)}</div>
      ),
    }, {
      title: '事件详情',
      dataIndex: 'detail',
      key: 'detail',
      width: '13.5%',
      render: (text, record) => (
        <div className="textDisplayOverflow">
          {
            record.issues.map(item => (
              <div>
                {this.renderDetail(item, record)}
              </div>
            ))
          }
        </div>
      ),
    }, {
      title: `升${unit}`,
      dataIndex: 'up',
      key: 'up',
      width: '10.5%',
      render: (text, record) => (
        <div>
          {
            record.issues.map(item => (
              <div style={{ minWidth: 15 }}>
                {this.renderUp(item)}
              </div>
            ))
          }
        </div>
      ),
    }, {
      title: `降${unit}`,
      dataIndex: 'down',
      key: 'down',
      width: '10.5%',
      render: (text, record) => (
        <div className="textDisplayOverflow">
          {
            record.issues.map(item => (
              <div style={{ minWidth: 15 }}>
                {this.renderDown(item)}
              </div>
            ))
          }
        </div>
      ),
    }, {
      title: `剩余${unit}`,
      dataIndex: 'rest',
      key: 'rest',
      width: '10.5%',
      render: text => (
        <p style={{ minWidth: 15 }}>{text}</p>
      ),
    }];
    let sprintName;
    for (let index = 0, len = BurndownChartStore.getSprintList.length; index < len; index += 1) {
      if (BurndownChartStore.getSprintList[index].sprintId === this.state.defaultSprintId) {
        sprintName = BurndownChartStore.getSprintList[index].sprintName;
      }
    }
    const urlParams = AppState.currentMenuType;
    const { linkFromParamUrl, quickFilter } = this.state;
    return (
      <Page service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.burndown']}>
        <Header
          title="燃尽图"
          // backPath={`/agile/${linkFromParamUrl || 'reporthost'}?type=${urlParams.type}&id=${urlParams.id}&name=${encodeURIComponent(urlParams.name)}&organizationId=${urlParams.organizationId}`}
          backPath={`/charts?type=${urlParams.type}&id=${urlParams.id}&name=${encodeURIComponent(urlParams.name)}&organizationId=${urlParams.organizationId}&orgId=${urlParams.organizationId}`}
        >
          <SwithChart
            current="burndownchart"
          />
          {/* <Button funcType="flat" onClick={this.getChartData.bind(this)}> */}
          <Button
            funcType="flat"
            onClick={() => {
              this.getChartData();
              // this.getChartCoordinate();
              this.axiosGetRestDays();
            }}
          >
            <Icon type="refresh icon" />
            <span>刷新</span>
          </Button>
        </Header>
        <Breadcrumb title="燃尽图" />
        <Content>
          {
            // this.state.chartLoading || this.state.tableLoading || BurndownChartStore.getSprintList.length > 0 ? (
            BurndownChartStore.getSprintList.length > 0 ? (
              <div>
                <div>
                  <Select
                    getPopupContainer={triggerNode => triggerNode.parentNode}
                    style={{ width: 244 }}
                    label="迭代冲刺"
                    value={this.state.defaultSprintId}
                    onChange={(value) => {
                      let endDate;
                      let startDate;
                      for (let index = 0, len = BurndownChartStore.getSprintList.length; index < len; index += 1) {
                        if (BurndownChartStore.getSprintList[index].sprintId === value) {
                          endDate = BurndownChartStore.getSprintList[index].endDate;
                          startDate = BurndownChartStore.getSprintList[index].startDate;
                        }
                      }
                      this.setState({
                        defaultSprintId: value,
                        endDate,
                        startDate,
                      }, () => {
                        this.getChartData();
                        this.getChartCoordinate();
                        this.axiosGetRestDays();
                      });
                    }}
                  >
                    {BurndownChartStore.getSprintList.length > 0
                      ? BurndownChartStore.getSprintList.map(item => (
                        <Option key={item.sprintId} value={item.sprintId}>{item.sprintName}</Option>
                      )) : ''}
                  </Select>
                  <Select
                    getPopupContainer={triggerNode => triggerNode.parentNode}
                    style={{ width: 244, marginLeft: 24 }}
                    label="单位"
                    defaultValue={this.state.select}
                    onChange={this.handleChangeSelect.bind(this)}
                  >
                    <Option value="remainingEstimatedTime">剩余时间</Option>
                    <Option value="storyPoints">故事点</Option>
                    <Option value="issueCount">问题计数</Option>
                  </Select>
                  <QuickSearch
                    style={{ marginLeft: 24, width: 244 }}
                    onChange={this.handleQuickSearchChange} 
                    value={quickFilter}
                  />
                  <Checkbox
                    style={{ marginLeft: 24 }}
                    checked={this.state.restDayShow}
                    onChange={this.onCheckChange}
                  >
                    显示非工作日
                  </Checkbox>
                </div>
                <Spin spinning={this.state.chartLoading}>
                  <ReactEcharts option={this.getOption()} />
                </Spin>
                <Table
                  dataSource={BurndownChartStore.getBurndownList}
                  loading={this.state.tableLoading}
                  columns={columns}
                  pagination={false}
                  rowKey={record => `${record.date}-${record.type}`}
                />
              </div>
            ) : (
                <NoDataComponent title="冲刺" links={[{ name: '待办事项', link: LINK_URL.workListBacklog }]} img={epicSvg} />
              )
          }
        </Content>
      </Page>
    );
  }
}

export default BurndownChartHome;
