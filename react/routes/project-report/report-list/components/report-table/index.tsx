import React, { useMemo } from 'react';
import {
  DataSet,
  Table,
} from 'choerodon-ui/pro';
import { Link } from 'react-router-dom';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import { projectReportApiConfig } from '@/api';
import UserHead from '@/components/UserHead';
import { User } from '@/common/types';
import { linkUrl } from '@/utils/to';

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
        renderer={({ record }) => (
          <Link
            to={linkUrl(`/agile/project-report/edit/${record?.get('id')}`)}
          >
            {record?.get('title')}
          </Link>
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
    </Table>
  );
};
export default ReportTable;
