import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import { TemplateAction } from '@/api';
import IssueExport from '@/components/issue-export';
import IssueExportStore from '@/components/issue-export/stores/store';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';

function openExportWorkModal() {
  const store = new IssueExportStore();
  const fields = [] as any[];
  const chosenFields = [] as any[];
  const checkOptions = [] as any[];
  const visibleColumns = [] as any[];
  const action: TemplateAction |undefined = undefined;
  Modal.open({
    key: Modal.key(),
    title: '导出工作项工时',
    style: {
      width: MODAL_WIDTH.middle,
    },
    className: 'c7n-agile-export-issue-modal',
    drawer: true,
    children: <IssueExport
      fields={fields}
      chosenFields={chosenFields}
      checkOptions={checkOptions}
      visibleColumns={visibleColumns}
      store={store}
      action={action}
      exportBtnText="导出"
    />,
    footer: (okBtn: any, cancelBtn: any) => cancelBtn,
    cancelText: '关闭',
    cancelProps: {
      color: 'primary',
    },
  });
}
export default openExportWorkModal;
