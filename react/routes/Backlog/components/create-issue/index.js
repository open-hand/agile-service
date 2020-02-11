import React from 'react';
import { observer } from 'mobx-react-lite';
import CreateIssue from '@/components/CreateIssue';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

export default observer(() => {
  const visible = BacklogStore.newIssueVisible;
  
  return (
    <CreateIssue
      visible={visible}
      onCancel={() => {
        BacklogStore.setNewIssueVisible(false);
      }}
      onOk={(res) => {
        BacklogStore.setNewIssueVisible(false);
        // 创建issue后刷新
        if (res) {
          BacklogStore.refresh(false, res);
        }      
      }}
    />
  );
});
