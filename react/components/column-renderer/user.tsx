import React from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import UserTag from '@/components/tag/user-tag';

const renderStatus = ({ record }: { record: Record }) => (
  <div style={{
    display: 'inline-flex',
    overflow: 'hidden',
  }}
  >
    <UserTag
      data={{
        id: record.get('assigneeId'),
        tooltip: record.get('assigneeName'),
        loginName: record.get('assigneeLoginName'),
        realName: record.get('assigneeRealName'),
        imageUrl: record.get('assigneeImageUrl'),
      }}
    />
  </div>
);
export default renderStatus;
