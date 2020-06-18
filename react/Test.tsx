import React from 'react';
import { TextArea } from 'choerodon-ui/pro';
import IssueDetail from '@/components/IssueDetail';
import TextEditToggle from '@/components/TextEditTogglePro';

const Test = () => (
  <div>
    {/* <IssueDetail issueId={5} /> */}
    <TextEditToggle initValue="ssss" editor={() => <TextArea />}>
      <span>ssss</span>
    </TextEditToggle>
  </div>
);
export default Test;
