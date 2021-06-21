import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import CreateIssue from '@/components/CreateIssue';
import Context from '../../context';

interface Props {
  refresh: () => void
}
const CreateIssueModal: React.FC<Props> = ({ refresh }) => {
  const { store } = useContext(Context);
  const { createIssueVisible } = store;
  const handleCancel = useCallback(() => {
    store.setCreateIssueVisible(false);
  }, [store]);
  const handleOk = useCallback(() => {
    store.setCreateIssueVisible(false);
    refresh();
  }, [refresh, store]);
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
