import React from 'react';
import { DataSet, Modal, Table } from 'choerodon-ui/pro';
import { ModalProps } from 'choerodon-ui/pro/lib/modal/Modal';
import classnames from 'classnames';
import { TemplateAction } from '@/api';
import ExportIssue from './ExportIssue';
import ExportIssueContextProvider from './stores';
import './ExportIssue.less';
import IssueExportStore from './stores/store';
import { IChosenFieldField } from '../chose-field/types';
import { removeCodeExtraPrefix } from './utils';

interface IExportIssueProps {
  fields: IChosenFieldField[],
  chosenFields: IChosenFieldField[],
  checkOptions: Array<{ value: string, label: string, order?: any }>,
  visibleColumns: Array<string>
  store: IssueExportStore,
  // eslint-disable-next-line react/require-default-props
  action?: TemplateAction
  // eslint-disable-next-line react/require-default-props
  exportBtnText?: string
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
  tableDataSet: DataSet, tableRef: React.RefObject<Table>, store: IssueExportStore, action?: TemplateAction, otherModalProps?: ModalProps, exportBtnText?: string) {
  const columns = tableRef.current
    ? tableRef.current.tableStore.columns.filter((column) => column.name && !column.hidden)
    : [];

  const checkOptions = [...tableDataSet.fields.values()].map((option) => ({ value: removeCodeExtraPrefix(option.pristineProps.name!), label: (option.pristineProps as any).label as string, order: option.order }));
  const { className, ...otherProps } = otherModalProps || {};
  const key = Modal.key();
  Modal.open({
    key,
    title: '导出问题',
    style: {
      width: 740,
    },
    className: classnames('c7n-agile-export-issue-modal', className),
    drawer: true,
    children: <Index
      fields={fields}
      chosenFields={chosenFields}
      checkOptions={checkOptions}
      visibleColumns={columns.map((item) => item.name!)}
      store={store}
      action={action}
      exportBtnText={exportBtnText}
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
