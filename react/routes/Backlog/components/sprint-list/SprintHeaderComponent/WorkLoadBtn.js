import React from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui/pro';
import openAssigneeModal from './AssigneeModal';
import './WorkLoadBtn.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const WorkLoadBtn = ({ data }) => {
  const handleClickBtn = () => {
    openAssigneeModal({
      data,
    });
  };
  const { assigneeIssues } = data;
  const formatMessage = useFormatMessage('agile.backlog');
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
        <span>{formatMessage({ id: 'assignee.workload' })}</span>
      </span>
    </>
  );
};

export default observer(WorkLoadBtn);
