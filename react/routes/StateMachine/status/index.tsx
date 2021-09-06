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
    ],
    queryFields: [
      {
        name: 'name',
        label: '名称',
      },
    ],

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
        <HeaderButtons items={[
          {
            name: '创建状态',
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
                  text: '编辑',
                  action: () => {
                    handleEditStatus({ record });
                  },
                },
                {
                  text: '删除',
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
