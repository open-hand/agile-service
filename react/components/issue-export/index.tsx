import React from 'react';
import { DataSet, Modal, Table } from 'choerodon-ui/pro/lib';
import { ModalProps } from 'choerodon-ui/pro/lib/modal/Modal';
import classnames from 'classnames';
import ExportIssue from './ExportIssue';
import ExportIssueContextProvider from './stores';
import './ExportIssue.less';
import IssueExportStore from './stores/store';
import { IChosenFieldField } from '../chose-field/types';

interface IExportIssueProps {
  fields: IChosenFieldField[],
  chosenFields: IChosenFieldField[],
  checkOptions: Array<{ value: string, label: string, order?: string }>,
  tableRef: React.RefObject<Table>,
  store: IssueExportStore,
}
export { IExportIssueProps };
export default function Index(props: IExportIssueProps) {
  return (
    <ExportIssueContextProvider {...props}>
      <ExportIssue />
    </ExportIssueContextProvider>
  );
}

function openExportIssueModal(fields: Array<IChosenFieldField>, chosenFields: Array<any>,
  tableDataSet: DataSet, tableRef: React.RefObject<Table>, store: IssueExportStore, otherModalProps?: ModalProps) {
  const checkOptions = [...tableDataSet.fields.values()].map((option) => ({ value: option.props.name!, label: option.props.label as string, order: option.order }));
  const { className, ...otherProps } = otherModalProps || {};
  const key = Modal.key();
  Modal.open({
    key,
    title: '导出问题',
    style: {
      width: 380,
    },
    className: classnames('c7n-agile-export-issue-modal', className),
    drawer: true,
    children: <Index
      fields={fields}
      chosenFields={chosenFields}
      checkOptions={checkOptions}
      tableRef={tableRef}
      store={store}
    />,
    footer: (okBtn: any, cancelBtn: any) => cancelBtn,
    // okText: store.exportButtonConfig?.buttonChildren ?? '导出',
    // okProps: { ...store.exportButtonConfig?.buttonProps },
    cancelText: '关闭',
    cancelProps: {
      color: 'primary',
    },
    ...otherProps,
  });
}
export { openExportIssueModal };
