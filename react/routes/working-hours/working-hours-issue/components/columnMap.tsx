import React from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import classNames from 'classnames';
import UserTag from '@/components/tag/user-tag';
import styles from './columnMap.less';

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

const columnRenderMap = new Map([
  ['userId', {
    sortable: true,
    width: 450,
    lock: true,
    renderer: ({ record }: { record: Record}) => {
      const user = record?.get('user');
      const showText = user?.ldap ? `${user?.realName}(${user?.loginName})` : `${user?.realName}(${user?.email})`;
      return (
        <UserTag
          data={{
            ...user || {},
            textShow: showText,
          }}
        />
      );
    },
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
]);
export default columnRenderMap;
