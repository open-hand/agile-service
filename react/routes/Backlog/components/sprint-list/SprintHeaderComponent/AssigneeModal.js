import React, {useMemo} from 'react';
import {DataSet, Modal, Spin, Table} from 'choerodon-ui/pro';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

export const AssigneeModal = ({ data, loading }) => {
  const dataSet = useMemo(() => new DataSet({
    selection: false,
    paging: false,
    fields: [{
      name: 'assigneeName',
      label: '经办人',
    }, {
      name: 'issueCount',
      label: '总工作项数',
    }, {
      name: 'remainingIssueCount',
      label: '剩余工作项数',
    }, {
      name: 'totalStoryPoints',
      label: '总故事点',
    }, {
      name: 'remainingStoryPoints',
      label: '剩余故事点',
    }, {
      name: 'totalRemainingTime',
      label: '总任务工时',
    }, {
      name: 'remainingTime',
      label: '剩余任务工时',
    }],
  }), []);
  const dealDecimal = (value) => {
    if (value) {
      if (value % 1 > 0) {
        return value.toFixed(1);
      }
      return value;
    }
    return 0;
  };
  const columns = [{
    name: 'assigneeName',
    minWidth: 150,
    tooltip: 'overflow',
    renderer: ({ text }) => (text === '合计' ? (
      <span>{text}</span>
    ) : (
      <span>
        {text || '未分配'}
      </span>
    )),
  }, {
    name: 'issueCount',
    renderer: ({ text }) => (text || '无'),
  }, {
    name: 'remainingIssueCount',
    renderer: ({ text }) => (text || '无'),
  }, {
    name: 'totalStoryPoints',
    renderer: ({ text }) => (text || '无'),
  }, {
    name: 'remainingStoryPoints',
    renderer: ({ text }) => (text || '无'),
  }, {
    name: 'totalRemainingTime',
    renderer: ({ text }) => (text || '无'),
  }, {
    name: 'remainingTime',
    renderer: ({ text }) => (text || '无'),
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
  dataSet.loadData([...dataSource, {
    assigneeName: '合计',
    issueCount: dealDecimal(total.totalIssue),
    remainingIssueCount: dealDecimal(total.totalRemainIssueCount),
    totalStoryPoints: dealDecimal(total.totalStoryPoints),
    remainingStoryPoints: dealDecimal(total.remainingStoryPoints),
    totalRemainingTime: dealDecimal(total.totalTime),
    remainingTime: dealDecimal(total.totalRemainTime),
  }]);
  return data.assigneeIssues && (
    <Spin spinning={loading}>
      <Table
        dataSet={dataSet}
        columns={columns}
        queryBar="none"
      />
    </Spin>
  );
};

export function openAssigneeModal(openModalProps) {
  BacklogStore.setModalOpened(true);
  return Modal.open(openModalProps);
}
