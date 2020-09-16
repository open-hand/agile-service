import React from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import CreateIssue from '@/components/CreateIssue';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

export default observer(() => {
  const {
    newIssueVisible, chosenEpic, chosenFeature, chosenVersion, featureList,
  } = BacklogStore;
  const chosenFeatureItem = featureList.find((feature) => feature.issueId === chosenFeature) || {};
  return (
    newIssueVisible ? (
      <CreateIssue
        visible={newIssueVisible}
        epicId={chosenEpic !== 'all' && chosenEpic !== 'unset' ? chosenEpic : undefined}
        chosenFeature={chosenFeature !== 'all' && chosenFeature !== 'unset' ? chosenFeature : undefined}
        chosenFeatureName={chosenFeatureItem.summary}
        chosenVersion={chosenVersion !== 'all' && chosenVersion !== 'unset' ? chosenVersion : undefined}
        onCancel={() => {
          BacklogStore.setNewIssueVisible(false);
        }}
        onOk={(res) => {
          BacklogStore.setNewIssueVisible(false);
          // 创建issue后刷新
          if (res) {
            BacklogStore.refresh(false, false);
          }
        }}
      />
    ) : null
  );
});
