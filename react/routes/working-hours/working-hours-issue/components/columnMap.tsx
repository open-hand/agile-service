import React from 'react';
import { Tooltip } from 'choerodon-ui';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import classNames from 'classnames';
import UserTag from '@/components/tag/user-tag';
import TypeTag from '@/components/TypeTag';
import styles from './columnMap.less';
import StatusTag from '@/components/StatusTag';
import ProjectTags from '@/components/tag/project-tags';

const renderWorkTime = ({ value }: { value: string}) => (
  value && `${value}h`
);

const renderRate = ({ value }: { value: number}) => (
  <span className={classNames({
    [styles.zero]: value === 0,
    [styles.gtZero]: value > 0,
    [styles.ltZero]: value < 0,
  })}
  >
    {value}
  </span>
);

const renderUser = ({ record }: { record: Record}, code: string) => {
  const user = record?.get(code);
  if (user) {
    const showText = user?.ldap ? `${user?.realName}(${user?.loginName})` : `${user?.realName}(${user?.email})`;
    return (
      <UserTag
        data={{
          ...user || {},
          textShow: showText,
        }}
      />
    );
  }
  return null;
};

const columnRenderMap = new Map([
  ['userId', {
    sortable: true,
    width: 450,
    lock: true,
    renderer: ({ record }: { record: Record }) => renderUser({ record }, 'user'),
  }],
  ['workTime', {
    sortable: true,
    renderer: renderWorkTime,
  }],
  ['historyWorkTime', {
    sortable: true,
    width: 160,
    renderer: renderWorkTime,
  }],
  ['estimatedWorkTime', {
    sortable: true,
    width: 160,
    renderer: renderWorkTime,
  }],
  ['rate', {
    sortable: true,
    width: 160,
    renderer: renderRate,
  }],
  ['summary', {
    sortable: true,
    width: 400,
    renderer: ({ record }: { record: Record}, onSummaryClick: (record: Record) => void) => (
      <>
        <TypeTag data={record.get('issueTypeVO')} style={{ marginRight: 5, marginTop: -2 }} />
        <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`工作项概要： ${record.get('summary')}`}>
          <span role="none" className="c7n-agile-table-cell-click" onClick={() => onSummaryClick(record)}>
            {record.get('summary')}
          </span>
        </Tooltip>
      </>
    ),
  }],
  [
    'statusId', {
      sortable: true,
      width: 160,
      renderer: ({ record }: { record: Record}) => (
        <StatusTag
          data={record.get('statusVO')}
          style={{ display: 'inline-block' }}
        />
      ),
    },
  ],
  [
    'assigneeId', {
      sortable: true,
      width: 160,
      renderer: ({ record }: { record: Record }) => renderUser({ record }, 'assignee'),
    },
  ],
  [
    'projectId', {
      sortable: true,
      width: 250,
      renderer: ({ record }: { record: Record }) => <ProjectTags data={record.get('projectVO')} />,
    },
  ],
]);
export default columnRenderMap;
