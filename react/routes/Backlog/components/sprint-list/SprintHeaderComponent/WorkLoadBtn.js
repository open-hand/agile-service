import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui';
import AssigneeModal from './AssigneeModal';
import './WorkLoadBtn.less';

const WorkLoadBtn = ({ data }) => {
  const [isOpen, setIsOpen] = useState(false);
  const handleClickBtn = () => {
    setIsOpen(true);
  };

  const onCloseModal = () => {
    setIsOpen(false);
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
      <AssigneeModal
        visible={isOpen}
        onCancel={onCloseModal}
        data={data}
      />
    </>
  );
};

export default observer(WorkLoadBtn);
