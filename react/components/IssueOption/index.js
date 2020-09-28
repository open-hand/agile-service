import React from 'react';
import TypeTag from '@/components/tag/type';

const IssueOption = ({ issue }) => (
  <div style={{
    display: 'inline-flex',
    flex: 1,
    // width: 'calc(100% - 30px)',
    alignItems: 'center',
    verticalAlign: 'middle',
  }}
  >
    <TypeTag
      data={issue.issueTypeVO}
    />
    <span style={{
      paddingLeft: 12, paddingRight: 12, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
    }}
    >
      {issue.issueNum}
    </span>
    <div style={{ overflow: 'hidden', flex: 1 }}>
      <p style={{
        paddingRight: '25px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0, maxWidth: 'unset',
      }}
      >
        {issue.summary}
      </p>
    </div>
  </div>
);
export default IssueOption;
