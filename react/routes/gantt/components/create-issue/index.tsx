import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import CreateIssue from '@/components/CreateIssue';
import { Issue } from '@/common/types';
import Context from '../../context';

interface Props {
  onCreate: (issue:Issue) => void
}
const CreateIssueModal: React.FC<Props> = ({ onCreate }) => {
  const { store } = useContext(Context);
  const { createIssueVisible } = store;
  const handleCancel = useCallback(() => {
    store.setCreateIssueVisible(false);
  }, [store]);
  const handleOk = useCallback((issue) => {
    store.setCreateIssueVisible(false);
    onCreate(issue);
  }, [onCreate, store]);
  return (
    createIssueVisible ? (
      <CreateIssue
        visible={createIssueVisible}
        onCancel={handleCancel}
        // chosenSprint={store.sprintId === '0' ? undefined : store.sprintId}
        onOk={handleOk}
      />
    ) : null
  );
};

export default observer(CreateIssueModal);
