import React, { useMemo, ReactElement, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, DataSet, Icon, CheckBox,
} from 'choerodon-ui/pro/lib';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import OldSortTable from '../../../page/components/SortTable';
import './index.less';
import { usePageIssueTypeStore } from '../../stores';

const { Column } = Table;
const SortTable: React.FC<{ type: string, disabled: boolean | undefined }> = (
  { type, disabled, children },
) => {
  const { sortTableDataSet } = usePageIssueTypeStore();

  useEffect(() => {
    console.log('disabled:', disabled);
  }, [disabled]);
  const renderFieldName = ({ value }: RenderProps) => (
    <div>
      {!disabled && <Icon type="baseline-drag_indicator" />}
      <span>{value}</span>
    </div>
  );

  function renderCheckBox({ value, name, record }: RenderProps) {
    return (
      <CheckBox
        // disabled={disabled}
        defaultChecked={value}
        // value={value}
        onChange={(val) => {
          console.log('val', val, name);
          record?.set(name as String, val);
        }}
      />
    );
  }
  return (
    <div className="c7n-page-issue-detail">
      <Table
        dataSet={sortTableDataSet}
        queryBar={'none' as TableQueryBarType}
        dragRow={!disabled}
        border={false}
      >
        <Column name="fieldName" renderer={renderFieldName} />
        <Column name="defaultValue" />
        <Column name="require" width={75} renderer={renderCheckBox} />
        <Column name="edit" width={135} renderer={renderCheckBox} />
        <Column name="create" width={135} renderer={renderCheckBox} />
      </Table>
    </div>
  );
};
export default observer(SortTable);
