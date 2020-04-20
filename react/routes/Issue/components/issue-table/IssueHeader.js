import React from 'react';
import { Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { set } from 'mobx';
import './IssueHeader.less';

const FIELD = {
  issueId: '概要',
  issueNum: '问题编号',
  priorityId: '优先级',
  reporterId: '报告人',
  assigneeId: '经办人',
  statusId: '状态',
  lastUpdateDate: '最后更新时间',
};

const IssueHeader = ({ fieldCode, dataSet }) => {
  const handleSortIssueByField = () => {
    set(dataSet, { sort: fieldCode, isAsc: dataSet.sort === fieldCode ? !dataSet.isAsc : true });
    dataSet.query();
  };

  let iconType = 'arrow_upward';
  if (dataSet.sort === fieldCode && !dataSet.isAsc) {
    iconType = 'arrow_downward';
  }
  return (
    <span className={`c7nagile-issue-table-header ${dataSet.sort === fieldCode ? 'c7nagile-issue-table-header-sortBy' : ''}`}>
      {FIELD[fieldCode]}
      <Icon 
        className="c7nagile-issue-table-header-icon"
        type={iconType}
        onClick={handleSortIssueByField}
      />
    </span>
  );
};

export default observer(IssueHeader);
