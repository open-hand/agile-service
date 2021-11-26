import React, { useCallback, useMemo } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { HeaderButtons, Action } from '@choerodon/master';
import { Table, DataSet } from 'choerodon-ui/pro';
import { Divider, Icon } from 'choerodon-ui';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TableAutoHeightType, ColumnAlign } from 'choerodon-ui/pro/lib/table/enum';
import { statusTransformApiConfig, ITotalStatus } from '@/api';
import StatusTypeTag from '@/components/tag/status-type-tag';
import { IStatus } from '@/common/types';

import { TabComponentProps } from '../index';
import openCreateStatus from '../components/create-status';
import openDeleteStatus from './DeleteStatus';
import styles from './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const { Column } = Table;
const Status: React.FC<TabComponentProps> = ({ tab }) => {
  const globalFormatMessage = useFormatMessage();

  const formatMessage = useFormatMessage('agile.stateMachine');
  const dataSet = useMemo(() => new DataSet({
    primaryKey: 'id',
    name: 'status',
    autoQuery: true,
    selection: false,
    transport: {
      read: ({ params }) => statusTransformApiConfig.listStatus(params.page, params.size),
    },
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'name' }),
      },
      {
        name: 'type',
        type: 'intl' as FieldType,
        label: formatMessage({ id: 'stage' }),
      },
      {
        name: 'completed',
        type: 'boolean' as FieldType,
        label: formatMessage({ id: 'solve.status' }),
      },
      {
        name: 'usage',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'usage' }),
      },
    ],
    queryFields: [
      {
        name: 'name',
        label: formatMessage({ id: 'name' }),
      },
    ],

  }), [formatMessage]);
  const handleCreateStatusClick = () => {
    openCreateStatus({
      onSubmit: () => {
        dataSet.query();
      },
    });
  };

  const handleEditStatus = useCallback(({ record }) => {
    openCreateStatus({
      onSubmit: () => {
        dataSet.query();
      },
      record,
    });
  }, [dataSet]);

  return (
    <Page>
      <Header>
        <HeaderButtons items={[
          {
            name: formatMessage({ id: 'create.state' }),
            display: true,
            handler: handleCreateStatusClick,
            icon: 'playlist_add',
          },
        ]}
        />
      </Header>
      <Breadcrumb />
      <Divider className={styles.divider} style={{ margin: 0 }} />
      <Content>
        {tab}
        <Table
          key="user"
          dataSet={dataSet}
          className={styles.table}
          autoHeight={{
            type: 'maxHeight' as TableAutoHeightType,
            diff: 100,
          }}
          filterBarFieldName="param"
        >
          <Column
            name="name"
          />
          <Column
            width={60}
            name="action"
            renderer={({ record, value }) => (
              <Action data={[
                {
                  text: globalFormatMessage({ id: 'boot.edit' }),
                  action: () => {
                    handleEditStatus({ record });
                  },
                },
                {
                  text: globalFormatMessage({ id: 'boot.delete' }),
                  action: () => {
                    openDeleteStatus({
                      onSubmit: () => {
                        dataSet.query();
                      },
                      data: record?.toData() as ITotalStatus,
                    });
                  },
                },
              ]}
              />
            )}
          />
          <Column
            name="type"
            renderer={({ record }) => (
              <StatusTypeTag
                mode="tag"
                code={record?.get('type') as IStatus['valueCode']}
              />
            )}
          />
          <Column name="completed" align={'left' as ColumnAlign} renderer={({ value }) => <span className={styles.gray}>{value ? '是' : '否'}</span>} />
          <Column name="usage" renderer={({ value }) => <span className={styles.gray}>{value}</span>} />
        </Table>
      </Content>
    </Page>
  );
};
export default Status;
