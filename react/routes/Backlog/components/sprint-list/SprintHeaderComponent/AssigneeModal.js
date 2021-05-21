import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Modal, Table } from 'choerodon-ui';
import _ from 'lodash';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';

const { Sidebar } = Modal;

@observer
class AssigneeModal extends Component {
  constructor(props) {
    super(props);
    this.state = {};
  }

  dealDecimal = (value) => {
    if (value) {
      if (value % 1 > 0) {
        return value.toFixed(1);
      }
      return value;
    }
    return 0;
  };

  render() {
    const { data, visible, onCancel } = this.props;
    const columns = [{
      title: '经办人',
      dataIndex: 'assigneeName',
      key: 'assigneeName',
      width: 200,
      render: (text) => (text === '合计' ? (
        <span>{text}</span>
      ) : (
        <span>
          {text || '未分配'}
        </span>
      )),
    }, {
      title: '总问题数',
      dataIndex: 'issueCount',
      key: 'issueCount',
      render: (text) => (text || '无'),
    }, {
      title: '剩余问题数',
      dataIndex: 'remainingIssueCount',
      key: 'remainingIssueCount',
      render: (text) => (text || '无'),
    }, {
      title: '总故事点',
      dataIndex: 'totalStoryPoints',
      key: 'totalStoryPoints',
      render: (text) => (text || '无'),
    }, {
      title: '剩余故事点',
      dataIndex: 'remainingStoryPoints',
      key: 'remainingStoryPoints',
      render: (text) => (text || '无'),
    }, {
      title: '总任务工时',
      dataIndex: 'totalRemainingTime',
      key: 'totalRemainingTime',
      render: (text) => (text || '无'),
    }, {
      title: '剩余任务工时',
      dataIndex: 'remainingTime',
      index: 'remainingTime',
      render: (text) => (text || '无'),
    }];
    const assignData = data.assigneeIssues;
    let totalIssue = 0;
    let totalRemainIssueCount = 0;
    let totalStoryPoints = 0;
    let remainingStoryPoints = 0;
    let totalTime = 0;
    let totalRemainTime = 0;
    if (Array.isArray([...assignData])) {
      for (let index = 0, lens = assignData.length; index < lens; index += 1) {
        if (assignData[index].issueCount) {
          totalIssue += assignData[index].issueCount;
        }
        if (assignData[index].remainingIssueCount) {
          totalRemainIssueCount += assignData[index].remainingIssueCount;
        }
        if (assignData[index].totalStoryPoints) {
          totalStoryPoints += assignData[index].totalStoryPoints;
        }
        if (assignData[index].remainingStoryPoints) {
          remainingStoryPoints += assignData[index].remainingStoryPoints;
        }
        if (assignData[index].totalRemainingTime) {
          totalTime += assignData[index].totalRemainingTime;
        }
        if (assignData[index].remainingTime) {
          totalRemainTime += assignData[index].remainingTime;
        }
      }
    }
    const total = {
      totalIssue,
      totalRemainIssueCount,
      totalStoryPoints,
      remainingStoryPoints,
      totalTime,
      totalRemainTime,
    };
    let noAssign = [];
    let dataSource = [];
    if (data.assigneeIssues) {
      noAssign = data.assigneeIssues.filter((item) => !item.assigneeName);
      dataSource = data.assigneeIssues.filter((item) => item.assigneeName).concat(noAssign);
    }
    return (
      <Sidebar
        maskClosable
        title="经办人工作量"
        visible={visible}
        onOk={onCancel.bind(this)}
        okText="关闭"
        okCancel={false}
        width={MODAL_WIDTH.middle}
      >
        {data.assigneeIssues && (
          <Table
            pagination={false}
            dataSource={_.concat(dataSource, {
              assigneeName: '合计',
              issueCount: this.dealDecimal(total.totalIssue),
              remainingIssueCount: this.dealDecimal(total.totalRemainIssueCount),
              totalStoryPoints: this.dealDecimal(total.totalStoryPoints),
              remainingStoryPoints: this.dealDecimal(total.remainingStoryPoints),
              totalRemainingTime: this.dealDecimal(total.totalTime),
              remainingTime: this.dealDecimal(total.totalRemainTime),
            })}
            columns={columns}
            filterBar={false}
            rowKey="assigneeName"
          />
        )}
      </Sidebar>
    );
  }
}

export default AssigneeModal;
