import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { Table } from 'choerodon-ui/pro';
import { StatusTag } from '@choerodon/components';
import { ColumnAlign } from 'choerodon-ui/pro/lib/table/enum';
import { useWorkGroupStore } from '@/routes/work-group/stores';
import Styles from './index.less';

const { Column } = Table;

const UserTable = () => {
  const {
    tableDs,
  } = useWorkGroupStore();

  const renderStatus = useCallback(({ value }) => (
    <StatusTag
      name={value ? '启用' : '停用'}
      colorCode={value ? 'success' : 'deleted'}
    />
  ), []);

  return (
    <Table dataSet={tableDs} className={Styles.userTable}>
      <Column name="realName" />
      <Column name="loginName" />
      <Column
        name="enabled"
        renderer={renderStatus}
        width={100}
      />
      <Column
        name="ldap"
        renderer={({ value }) => (value ? 'LDAP用户' : '非LDAP用户')}
        align={ColumnAlign.left}
        width={150}
      />
    </Table>
  );
};

export default observer(UserTable);
