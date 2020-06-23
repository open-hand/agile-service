
import React from 'react';
import { Button } from 'choerodon-ui/pro';
import IssueDetail, { useIssueDetailStore } from '@/components/IssueDetail';

const Test = () => {
  const store = useIssueDetailStore();
  return (
    <div>
      <Button onClick={() => {
        store.select(271250);
      }}
      >
        打开详情
      </Button>
      <Button onClick={() => {
        store.select(271246);
      }}
      >
        打开详情2
      </Button>
      <IssueDetail />
    </div>
  );
};

export default Test;
