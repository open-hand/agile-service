import React from 'react';
import { DataSet, Modal, Table } from 'choerodon-ui/pro';
import { ModalProps } from 'choerodon-ui/pro/lib/modal/Modal';
import classnames from 'classnames';
import { TemplateAction } from '@/api';
import { IChosenFieldField } from '@/components/chose-field/types';
import { removeCodeExtraPrefix } from '@/components/issue-export/utils';
import Index from '@/components/issue-export';
import IssueExportStore from '@/components/issue-export/stores/store';

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
      width: 740,
    },
    className: 'c7n-agile-export-issue-modal',
    drawer: true,
    children: <Index
      fields={fields}
      chosenFields={chosenFields}
      checkOptions={checkOptions}
      visibleColumns={visibleColumns}
      store={store}
      action={action}
      exportBtnText="导出"
    />,
    footer: (okBtn: any, cancelBtn: any) => cancelBtn,
    // okText: store.exportButtonConfig?.buttonChildren ?? '导出',
    // okProps: { ...store.exportButtonConfig?.buttonProps },
    cancelText: '关闭',
    cancelProps: {
      color: 'primary',
    },
  });
}
export default openExportWorkModal;
