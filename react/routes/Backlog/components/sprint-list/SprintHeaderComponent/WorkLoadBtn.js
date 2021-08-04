import React from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui/pro';
import openAssigneeModal from './AssigneeModal';
import './WorkLoadBtn.less';

const WorkLoadBtn = ({ data }) => {
  const handleClickBtn = () => {
    openAssigneeModal({
      data,
    });
  };
  const { assigneeIssues } = data;

  return (
    <>
      <span
        role="none"
        onClick={handleClickBtn}
        className="c7n-agile-workloadBtn"
        style={{
          display: assigneeIssues && assigneeIssues.length > 0 ? 'flex' : 'none',
        }}
      >
        <Icon type="find_in_page-o" />
        <span>查看经办人工作量</span>
      </span>
    </>
  );
};

export default observer(WorkLoadBtn);
