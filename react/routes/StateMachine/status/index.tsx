import React, { useCallback, useMemo } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { Button, Table, DataSet } from 'choerodon-ui/pro';
import { Divider } from 'choerodon-ui';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusTransformApiConfig, ITotalStatus } from '@/api';
import StatusTypeTag from '@/components/tag/status-type-tag';
import { IStatus } from '@/common/types';

import { TableAutoHeightType, ColumnAlign } from 'choerodon-ui/pro/lib/table/enum';
import { TabComponentProps } from '../index';
import openCreateStatus from '../components/create-status';
import openDeleteStatus from './DeleteStatus';
import styles from './index.less';

const { Column } = Table;
const Status: React.FC<TabComponentProps> = ({ tab }) => {
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
        label: '名称',
      },
      {
        name: 'type',
        type: 'intl' as FieldType,
        label: '阶段',
      },
      {
        name: 'completed',
        type: 'boolean' as FieldType,
        label: '是否为已解决',
      },
      {
        name: 'usage',
        type: 'string' as FieldType,
        label: '使用情况',
      },
      {
        name: 'operate',
        type: 'boolean' as FieldType,
        label: '操作',
      },
    ],
    queryFields: [],

  }), []);
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
        <Button icon="playlist_add" onClick={handleCreateStatusClick}>创建状态</Button>
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
            renderer={({ record, value }) => (
              <span
                role="none"
                className={styles.cellClick}
                onClick={() => handleEditStatus({ record })}
              >
                {value}
              </span>
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
          <Column
            name="operate"
            renderer={({ record }) => (
              <Button
                icon="delete"
                onClick={() => {
                  openDeleteStatus({
                    onSubmit: () => {
                      dataSet.query();
                    },
                    data: record?.toData() as ITotalStatus,
                  });
                }}
              />
            )}
          />
        </Table>
      </Content>
    </Page>
  );
};
export default Status;
