import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { set } from 'lodash';
import ExportIssue, { openExportIssueModal as originOpenExportIssueModal } from '@/components/issue-export';
import IssueExportStore from '@/components/issue-export/stores/store';
import { issueApi, TemplateAction } from '@/api';
import { IChosenFieldField } from '@/components/chose-field/types';
import { IFoundationHeader } from '@/common/types';
import { isInProgram } from '@/utils/program';
import {
  getExportFieldCodes, getTransformSystemFilter, getFilterFormSystemFields, getReverseExportFieldCodes,
} from './utils';

const mapper = {
  summary: 'issueId',
  issueNum: 'issueNum',
  priority: 'priorityId',
  sprint: 'issueSprintVOS',
  reporter: 'reporterId',
  creationDate: 'creationDate',
  assignee: 'assigneeId',
  status: 'statusId',
  lastUpdateDate: 'lastUpdateDate',
  estimatedStartTime: 'estimatedStartTime',
  estimatedEndTime: 'estimatedEndTime',
  label: 'label',
  component: 'component',
  storyPoints: 'storyPoints',
  fixVersion: 'fixVersion',
  influenceVersion: 'influenceVersion',
  epic: 'epic',
  feature: 'feature',
  estimateTime: 'estimateTime',
  remainingTime: 'remainingTime',
};
function openExportIssueModal(fields: Array<IChosenFieldField>, chosenFields: Array<any>,
  tableFields: IFoundationHeader[], visibleColumns: string[], tableListMode: boolean, action?: TemplateAction) {
  const store = new IssueExportStore({
    defaultInitFieldAction: (data, self) => {
      if (data.code === 'sprint') {
        return ({ ...data, immutableCheck: true });
      }
      if (data.code === 'quickFilterIds' || data.code === 'starBeacon' || data.code === 'myAssigned') {
        data.value && self.setState(data.code, data);
        // self.addExtraField(data);
        return false;
      }
      // @ts-ignore
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
    dataSetSystemFields: getFilterFormSystemFields(),
    transformSystemFilter: getTransformSystemFilter,
    transformExportFieldCodes: getExportFieldCodes,
    reverseTransformExportFieldCodes: getReverseExportFieldCodes,
    events: {
      exportAxios: (searchData, sort) => {
        set(searchData, 'searchArgs.tree', !tableListMode);
        return issueApi.export(searchData, sort);
      },
      loadRecordAxios: () => issueApi.loadLastImportOrExport('download_file'),
    },
    defaultCheckedExportFields: ['issueTypeId', 'issueNum', 'issueId', 'description'],
    checkboxOptionsExtraConfig: new Map(['issueTypeId', 'issueNum', 'issueId'].map((item) => [item, { optionConfig: { disabled: true }, defaultChecked: true }])),
    defaultInitOptions: ({ options }) => {
      options.splice(3, 0, {
        label: '描述',
        value: 'description',
      });
      options.splice(12, 0, {
        label: '关联工作项',
        value: 'relatedIssue',
      });
      return !isInProgram() ? options.filter((item) => item.value !== 'feature') : options;
    },
  });

  const checkOptions = tableFields.map((option) => ({ value: mapper[option.code as keyof typeof mapper] || option.code, label: option.title as string, order: false }));
  const key = Modal.key();
  Modal.open({
    key,
    title: '导出工作项',
    style: {
      width: 740,
    },
    className: 'c7n-agile-export-issue-modal',
    drawer: true,
    children: <ExportIssue
      fields={fields}
      chosenFields={chosenFields}
      checkOptions={checkOptions}
      visibleColumns={visibleColumns.map((i) => mapper[i as keyof typeof mapper] || i)}
      store={store}
      action={action}
      exportBtnText="导出"
    />,
    footer: (okBtn: any, cancelBtn: any) => cancelBtn,
    okText: '导出',
    // okProps: { ...store.exportButtonConfig?.buttonProps },
    cancelText: '关闭',
    cancelProps: {
      color: 'primary',
    },
  });
  // originOpenExportIssueModal(fields, chosenFields, tableDataSet, tableRef, store, action);
}
export { openExportIssueModal };
