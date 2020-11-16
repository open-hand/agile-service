import React from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Tooltip } from 'choerodon-ui/pro';
import TypeTag from '@/components/TypeTag';

const renderSummary = ({ record }: { record: Record }) => (
  <>
    <TypeTag featureType={record.get('featureType')} data={record.get('issueTypeVO')} style={{ marginRight: 5, marginTop: -2 }} />
    <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`问题概要： ${record.get('summary')}`}>
      <span className="c7n-agile-table-cell-click">
        {record.get('summary')}
      </span>
    </Tooltip>
  </>
);
export default renderSummary;
