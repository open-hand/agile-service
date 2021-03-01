import React from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Tooltip } from 'choerodon-ui/pro';
import { Tag } from 'choerodon-ui';
import { map } from 'lodash';

const renderTag = (listField: string, nameField: string) => ({ record }: { record: Record }) => {
  const list = record.get(listField);
  if (list) {
    if (list.length > 0) {
      return (
        <Tooltip title={<div>{map(list, (item) => item[nameField]).map((name) => <div>{name}</div>)}</div>}>
          <div style={{ display: 'inline-flex', maxWidth: '100%' }}>
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
              {list[0][nameField]}
            </Tag>
            {list.length > 1 ? <Tag color="blue">...</Tag> : null}
          </div>
        </Tooltip>
      );
    }
  }
  return null;
};
const renderSprint = ({ record }: { record: Record }) => (renderTag('issueSprintVOS', 'sprintName')({ record }));
export default renderSprint;
