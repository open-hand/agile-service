import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import { TemplateAction } from '@/api';
import IssueExport from '@/components/issue-export';
import IssueExportStore from '@/components/issue-export/stores/store';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IChosenFieldField } from '@/components/chose-field/types';
import { getTransformSystemFilter } from '@/routes/Issue/components/ExportIssue/utils';

interface IExportWorkHoursIssueModalProps {
  fields: IChosenFieldField[]
  chosenFields: IChosenFieldField[]
  columns: Array<{ code: string, title: string }>
  visibleColumns?: string[]
}
function openExportWorkHoursIssueModal({
  fields, columns, chosenFields, visibleColumns = [],
}: IExportWorkHoursIssueModalProps) {
  const store = new IssueExportStore({
    defaultInitFieldAction: (data, self) => {
      if (data.code === 'quickFilterIds' || data.code === 'starBeacon' || data.code === 'myAssigned') {
        data.value && self.setState(data.code, data);
        return false;
      }
      if (data.archive) {
        return false;
      }

      return data;
    },
    defaultInitFieldFinishAction: (data, self) => {
      const quickFilterIds = self.getState('quickFilterIds');
      const starBeacon = self.getState('starBeacon');
      const myAssigned = self.getState('myAssigned');
      const value = [];
      starBeacon && value.push('myStarBeacon');
      myAssigned && value.push('myAssigned');
      quickFilterIds && value.push(...quickFilterIds.value);
      self.addExtraField({ name: '快速筛选', code: 'quickFilterIds', value });
    },
    transformSystemFilter: getTransformSystemFilter,
    events: {
      exportAxios: (searchData, sort) => new Promise((r) => true),
    },
  });
  const checkOptions = columns.map((item) => ({ value: item.code, label: item.title }));
  const action: TemplateAction | undefined = undefined;
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
export default openExportWorkHoursIssueModal;
