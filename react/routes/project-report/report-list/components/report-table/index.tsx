import React, { useMemo, useCallback } from 'react';
import {
  DataSet,
  Table,
  Modal,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import { projectReportApiConfig, projectReportApi } from '@/api';
import UserHead from '@/components/UserHead';
import TableAction from '@/components/TableAction';
import { User } from '@/common/types';
import to from '@/utils/to';

const { Column } = Table;

const ReportTable = () => {
  const dataSet = useMemo(() => new DataSet({
    primaryKey: 'id',
    autoQuery: true,
    selection: false,
    pageSize: 10,
    transport: {
      read: () => projectReportApiConfig.load(),
    },
    fields: [
      {
        name: 'title',
        type: 'string' as FieldType,
        label: '标题',
      },
      {
        name: 'receiverList',
        type: 'array' as FieldType,
        label: '收件人',
      },
      {
        name: 'createdUser',
        type: 'object' as FieldType,
        label: '创建人',
      },
    ],
  }), []);
  const handleMenuClick = useCallback(async (key, record) => {
    switch (key) {
      case 'delete': {
        Modal.confirm({
          title: `确认删除报告“${record.get('title')}”`,
          onOk: async () => {
            await projectReportApi.delete(record.get('id'));
            dataSet.query();
          },
        });
      }
    }
  }, [dataSet]);
  return (
    <Table
      key="user"
      dataSet={dataSet}
      autoMaxWidth
    >
      <Column
        name="title"
        tooltip={'overflow' as TableColumnTooltip}
        renderer={({ record }) => (
          <TableAction
            onEditClick={() => to(`/agile/project-report/edit/${record?.get('id')}`)}
            onMenuClick={({ key }: { key: string }) => handleMenuClick(key, record)}
            menus={[{
              key: 'delete',
              text: '删除',
            }]}
            text={record?.get('title')}
          />
        )}
      />
      <Column
        name="receiverList"
        renderer={({ value: receiverList }) => {
          if (!receiverList) {
            return null;
          }
          return receiverList.map((user: User) => (
            <UserHead
              // @ts-ignore
              style={{ display: 'inline-block' }}
              hiddenText
              user={user}
            />
          ));
        }}
      />
      <Column
        name="createdUser"
        renderer={({ value: createdUser }) => {
          if (!createdUser) {
            return null;
          }
          return (
            <UserHead
              // @ts-ignore
              style={{ display: 'inline-block' }}
              hiddenText
              user={createdUser as User}
            />
          );
        }}
      />
    </Table>
  );
};
export default ReportTable;
