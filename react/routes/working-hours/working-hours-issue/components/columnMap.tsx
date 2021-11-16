import React from 'react';
import { Tooltip } from 'choerodon-ui';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import classNames from 'classnames';
import UserTag from '@/components/tag/user-tag';
import TypeTag from '@/components/TypeTag';
import styles from './columnMap.less';
import StatusTag from '@/components/StatusTag';
import ProjectTag from '@/components/tag/project-tag';
import { User } from '@/common/types';

const renderWorkTime = ({ value }: { value: string}) => (
  value && `${value}h`
);

const renderRate = ({ value }: { value: number}) => {
  const numberValue = Number(value);
  return (
    <span className={classNames({
      [styles.zero]: numberValue === 0,
      [styles.gtZero]: numberValue > 0,
      [styles.ltZero]: numberValue < 0,
    })}
    >
      {numberValue !== 0 ? `${numberValue * 100}%` : 0}
    </span>
  );
};

const renderUser = (user: User | null) => {
  if (user) {
    const showText = user.textShow || (user?.ldap ? `${user?.realName}(${user?.loginName})` : `${user?.realName}(${user?.email})`);
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
    renderer: ({ record }: { record: Record }) => renderUser(record.get('userVO')),
  }],
  ['workTime', {
    sortable: true,
    renderer: renderWorkTime,
  }],
  ['cumulativeWorkTime', {
    sortable: true,
    width: 160,
    renderer: renderWorkTime,
  }],
  ['estimateTime', {
    sortable: true,
    width: 160,
    renderer: renderWorkTime,
  }],
  ['deviationRate', {
    sortable: true,
    width: 160,
    renderer: renderRate,
  }],
  ['summary', {
    lock: true,
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
      renderer: ({ record }: { record: Record }) => renderUser({
        id: record.get('assigneeId'),
        tooltip: record.get('assigneeName'),
        loginName: record.get('assigneeLoginName'),
        realName: record.get('assigneeRealName'),
        imageUrl: record.get('assigneeImageUrl'),
        textShow: record.get('assigneeName'),
      } as unknown as User),
    },
  ],
  [
    'projectId', {
      sortable: true,
      width: 250,
      renderer: ({ record }: { record: Record }) => <ProjectTag data={record.get('projectVO')} showText />,
    },
  ],
]);
export default columnRenderMap;
