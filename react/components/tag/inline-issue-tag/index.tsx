import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import TypeTag from '@/components/TypeTag';
import { Issue } from '@/common/types';

export interface InlineIssueTagProps {
  data: Issue
}
const InlineIssueTag = ({
  data,
}: InlineIssueTagProps) => (
  <div style={{
    display: 'inline-flex',
    width: 'calc(100% - 30px)',
    alignItems: 'center',
    verticalAlign: 'middle',
  }}
  >
    <TypeTag
      data={data.issueTypeVO}
    />
    <span style={{
      paddingLeft: 12, paddingRight: 12, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
    }}
    >
      {data.issueNum}
    </span>
    <div style={{ overflow: 'hidden', flex: 1 }}>
      <Tooltip title={data.summary}>
        <p style={{
          paddingRight: '25px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0, maxWidth: 'unset',
        }}
        >
          {data.summary}
        </p>
      </Tooltip>
    </div>
  </div>
);

InlineIssueTag.optionRenderer = (data:Issue) => <InlineIssueTag data={data} />;
InlineIssueTag.renderer = (data:Issue) => <span>{data.issueNum}</span>;

export default InlineIssueTag;
