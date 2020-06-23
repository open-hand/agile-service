
import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { Observer } from 'mobx-react-lite';
import IssueDetail, { useIssueDetailStore } from '@/components/IssueDetail';

const Test = () => {
  const store = useIssueDetailStore();
  const issueIds = [271250, 271246];
  return (
    <div>
      <Observer>
        {() => issueIds.map(issueId => (
          <Button
            key={issueId}
            style={store.selectedMap.get(issueId) ? { color: 'red' } : undefined}
            onClick={() => {
              store.select(issueId);
            }}
          >
            打开详情
          </Button>
        ))}
      </Observer>
      <IssueDetail />
    </div>
  );
};

export default Test;
