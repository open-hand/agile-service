import React, { useMemo, ReactElement } from 'react';
import { observer } from 'mobx-react-lite';
import { Table, DataSet, Icon } from 'choerodon-ui/pro/lib';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import OldSortTable from '../../../page/components/SortTable';
import './index.less';

const { Column } = Table;
const SortTable: React.FC<{ type: string, disabled: boolean | undefined }> = (
  { type, disabled, children },
) => {
  const dataSet = useMemo(() => new DataSet({
    autoQuery: true,
    dataKey: 'content',
    paging: false,
    selection: undefined,
    fields: [
      { name: 'fieldName', label: '字段名称' },
      { name: 'defaultValue', label: '默认值' },
      { name: 'require', label: '是否必填' },
      { name: 'fieldName', label: '是否加入到编辑页' },
      { name: 'fieldName', label: '是否加入到创建页' },

    ],
    transport: {
      read: {
        url: '/agile/v1/projects/1528/page_field/list?organizationId=7&pageCode=agile_issue_edit',
        method: 'get',
      },
    },
  }), []);
  return (
    <div className="c7n-page-issue-detail">
      <Table
        dataSet={dataSet}
        queryBar={'none' as TableQueryBarType}
        dragRow
        border={false}
      >
        <Column name="fieldName" />
        <Column name="defaultValue" />
        <Column name="require" width={75} />
        <Column name="fieldName" width={135} />
        <Column name="fieldName" width={135} />
      </Table>
    </div>
  );
};
export default observer(SortTable);
