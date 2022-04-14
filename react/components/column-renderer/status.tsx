import React from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Tooltip } from 'choerodon-ui/pro';
import StatusTag from '@/components/StatusTag';

const renderStatus = ({ record }: { record: Record }) => (
  <Tooltip title={record.get('statusVO').name}>
    <div style={{
      display: 'inline-flex',
      overflow: 'hidden',
      maxWidth: '100%',
    }}
    >
      <StatusTag
        data={record.get('statusVO')}
        style={{ display: 'inline-block' }}
      />
    </div>
  </Tooltip>
);
export default renderStatus;
