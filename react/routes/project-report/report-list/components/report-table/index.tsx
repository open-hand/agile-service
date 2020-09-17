import React, { useMemo } from 'react';
import {
  DataSet,
  Table,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import { projectReportApiConfig } from '@/api';
import { UserHead } from '@/components';
import { User } from '@/common/types';

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
    ],
  }), []);

  return (
    <Table
      key="user"
      dataSet={dataSet}
      autoMaxWidth
    >
      <Column
        name="title"
        tooltip={'overflow' as TableColumnTooltip}
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
    </Table>
  );
};
export default ReportTable;
