import React from 'react';
import { Tag } from 'choerodon-ui';

export default function renderTags({ array, name }: { array: any[], name: string }) {
  if (array) {
    if (array.length > 0) {
      return (
        <div style={{
          display: 'inline-flex', overflow: 'hidden', textOverflow: 'ellipsis', width: '100%',
        }}
        >
          <Tag
            color="blue"
            style={{
              maxWidth: 160,
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              whiteSpace: 'nowrap',
              cursor: 'auto',
            }}
          >
            {name}
          </Tag>
          { array.length > 1 ? <Tag color="blue">...</Tag> : null}
        </div>
      );
    }
  }
  return null;
}
