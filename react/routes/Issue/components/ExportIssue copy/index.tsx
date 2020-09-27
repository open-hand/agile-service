import React from 'react';
import { DataSet, Modal } from 'choerodon-ui/pro/lib';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import ExportIssue from './ExportIssue';
import ExportIssueContextProvider from './stores';
import './ExportIssue.less';
import { IExportIssueField, IExportIssueChosenField } from './types';

export default function Index(props: any) {
  return (
    <ExportIssueContextProvider {...props}>
      <ExportIssue />
    </ExportIssueContextProvider>
  );
}
function openExportIssueModal(fields: Array<IExportIssueField>, chosenFields: Array<any>, tableDataSet: DataSet, otherProps: any) {
  const checkOptions: Array<Field> = [...tableDataSet.fields.values()];
  console.log('openExportIssueModal', fields, chosenFields);
  Modal.open({
    key: Modal.key(),
    title: '导出问题',
    style: {
      width: 380,
    },
    className: 'c7n-agile-export-issue-modal',
    drawer: true,
    children: <Index
      fields={fields}
      chosenFields={chosenFields}
      checkOptions={checkOptions.map((option) => ({ value: option.props.name, label: option.props.label, order: option.order }))}
      {...otherProps}
    />,
    okText: '关闭',
    okCancel: false,
  });
}
export { openExportIssueModal };
