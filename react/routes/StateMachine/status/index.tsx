import React, { useCallback, useMemo } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { Button, Table, DataSet } from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusTransformApiConfig, ITotalStatus } from '@/api';
import StatusTypeTag from '@/components/tag/status-type-tag';
import { IStatus } from '@/common/types';
import { Divider } from 'choerodon-ui';
import { DragColumnAlign, TableAutoHeightType } from 'choerodon-ui/pro/lib/table/enum';
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

  const handleDragEnd = useCallback((ds, columns, resultDrag, provided) => {
    console.log(ds, columns, resultDrag, provided);
    const { source, destination } = resultDrag;
    if (!destination || source.index === destination.index) {
      return;
    }
    const sourceRecord = ds.get(source.index);
    let beforeDestinationIndex = destination.index;
    if (source.index < destination.index && destination.index !== ds.length - 1) {
      beforeDestinationIndex += 1;
    }
    const destinationRecord = ds.get(beforeDestinationIndex);
    // projectApi.sortSubProject({ projectId: sourceRecord.get('projectId'), before: beforeDestinationIndex !== dataSet.length - 1, outSetProjectId: destinationRecord.get('projectId') });
  }, []);

  return (
    <Page>
      <Header>
        <Button icon="playlist_add" onClick={handleCreateStatusClick}>创建状态</Button>
      </Header>
      <Breadcrumb />
      <Divider className={styles.divider} />
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
          dragRow
          dragColumnAlign={'left' as DragColumnAlign}
          onDragEndBefore={handleDragEnd}
        >
          <Column name="name" renderer={({ value }) => <span className={styles.gray}>{value}</span>} />
          <Column
            name="type"
            renderer={({ record }) => (
              <StatusTypeTag
                mode="tag"
                code={record?.get('type') as IStatus['valueCode']}
              />
            )}
          />
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
