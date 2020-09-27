import React from 'react';
import { DataSet, Modal, Table } from 'choerodon-ui/pro/lib';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import classnames from 'classnames';
import ExportIssue from './ExportIssue';
import ExportIssueContextProvider from './stores';
import './ExportIssue.less';
import { IExportIssueField } from './types';
import IssueExportStore from './stores/store';

export default function Index(props: any) {
  return (
    <ExportIssueContextProvider {...props}>
      <ExportIssue />
    </ExportIssueContextProvider>
  );
}

function openExportIssueModal(fields: Array<IExportIssueField>, chosenFields: Array<any>,
  tableDataSet: DataSet, tableRef: React.RefObject<Table>, store: IssueExportStore, otherModalProps: any) {
  const checkOptions: Array<Field> = [...tableDataSet.fields.values()];
  const { className, ...otherProps } = otherModalProps || {};
  Modal.open({
    key: Modal.key(),
    title: '导出问题',
    style: {
      width: 380,
    },
    className: classnames('c7n-agile-export-issue-modal', className),
    drawer: true,
    children: <Index
      fields={fields}
      chosenFields={chosenFields}
      checkOptions={checkOptions.map((option) => ({ value: option.props.name, label: option.props.label, order: option.order }))}
      tableRef={tableRef}
      store={store}
    />,
    okText: '关闭',
    okCancel: false,
    ...otherProps,
  });
}
export { openExportIssueModal };
