import React from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Tooltip } from 'choerodon-ui/pro';
import PriorityTag from '@/components/PriorityTag';

const renderPriority = ({ record }: { record: Record }) => (
  <Tooltip mouseEnterDelay={0.5} title={`优先级： ${record.get('priorityDTO') ? record.get('priorityDTO').name : ''}`}>
    <PriorityTag
      priority={record.get('priorityVO')}
      style={{ display: 'inline-flex' }}
    />
  </Tooltip>
);
export default renderPriority;
