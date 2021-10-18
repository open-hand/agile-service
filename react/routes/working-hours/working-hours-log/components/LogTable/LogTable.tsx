import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  ColumnAlign, TableColumnTooltip, TableQueryBarType,
} from 'choerodon-ui/pro/lib/table/enum';
import { Table } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { getIsOrganization } from '@/utils/common';
import UserTag from '@/components/tag/user-tag';
import StatusTag from '@/components/StatusTag';
import styles from './LogTable.less';
import { TypeTag } from '@/components';
import { useLogStore } from '../../stores';

const { Column } = Table;

const LogTable = () => {
  const { logDs } = useLogStore();
  const renderMember = useCallback(({ record }) => {
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
  }, []);

  const renderWorkTime = useCallback(({ value }) => (value && <span>{`${value}h`}</span>), []);

  const renderIssue = useCallback(({ record }) => (
    <Tooltip title={`${record.get('issueNum')} ${record?.get('summary')}`}>
      <div>
        <TypeTag data={record?.get('issueTypeVO')} />
        <span className={styles.issue} role="none" onClick={() => {}}>
          {`${record.get('issueNum')} ${record?.get('summary')}`}
        </span>
      </div>
    </Tooltip>
  ), []);

  const renderStatus = useCallback(({ record }) => (
    <Tooltip title={record.get('statusVO').name}>
      <div style={{
        display: 'inline-flex',
        overflow: 'hidden',
      }}
      >
        <StatusTag
          data={record.get('statusVO')}
          style={{ display: 'inline-block' }}
        />
      </div>
    </Tooltip>
  ), []);

  return (
    <Table dataSet={logDs} queryBar={'none' as TableQueryBarType}>
      <Column name="user" sortable renderer={renderMember} width={140} />
      <Column name="workTime" sortable renderer={renderWorkTime} align={'left' as ColumnAlign} width={110} />
      <Column name="startDate" sortable tooltip={'overflow' as TableColumnTooltip} width={150} />
      <Column name="issue" sortable renderer={renderIssue} />
      {
      getIsOrganization() && (
        <Column name="projectVO.name" sortable tooltip={'overflow' as TableColumnTooltip} />
      )
    }
      <Column name="statusVO" sortable renderer={renderStatus} width={100} />
    </Table>
  );
};

export default observer(LogTable);
