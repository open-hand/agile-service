import React, { useMemo } from 'react';
import {
  DataSet,
  Table,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';

const { Column } = Table;

const ReportTable = () => {
  const dataSet = useMemo(() => new DataSet({
    primaryKey: 'userid',
    name: 'user',
    autoQuery: false,
    pageSize: 10,
    fields: [
      {
        name: 'userid',
        type: 'string' as FieldType,
        label: '编号',
        required: true,
        unique: true,
      },
      {
        name: 'name',
        type: 'intl' as FieldType,
        label: '姓名',
      },
      {
        name: 'age',
        type: 'number' as FieldType,
        label: '年龄',
        unique: 'uniqueGroup',
        max: 100,
        step: 1,
        help: '用户年龄，可以排序',
      },
      {
        name: 'email',
        type: 'string' as FieldType,
        label: '邮箱',
        help: '用户邮箱，可以自动补全',
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
        name="userid"
        style={{ color: 'red' }}
        tooltip={'overflow' as TableColumnTooltip}
        editor
        width={200}
        minWidth={150}
        lock
      />
      <Column
        name="age"
        editor
        width={150}
      />
      <Column
        name="email"
      />

    </Table>
  );
};
export default ReportTable;
