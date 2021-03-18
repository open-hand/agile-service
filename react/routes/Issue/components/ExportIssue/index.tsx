import React from 'react';
import { DataSet, Table } from 'choerodon-ui/pro/lib';
import { openExportIssueModal as originOpenExportIssueModal } from '@/components/issue-export';
import IssueExportStore from '@/components/issue-export/stores/store';
import { issueApi, TemplateAction } from '@/api';
import { IChosenFieldField } from '@/components/chose-field/types';
import { set } from 'lodash';
import { isInProgram } from '@/utils/program';
import {
  getExportFieldCodes, getTransformSystemFilter, getFilterFormSystemFields, getReverseExportFieldCodes,
} from './utils';

function openExportIssueModal(fields: Array<IChosenFieldField>, chosenFields: Array<any>,
  tableDataSet: DataSet, tableRef: React.RefObject<Table>, tableListMode: boolean, action?: TemplateAction) {
  const store = new IssueExportStore({
    defaultInitFieldAction: (data, self) => {
      if (data.code === 'sprint') {
        return ({ ...data, immutableCheck: true });
      }
      if (data.code === 'quickFilterIds' || data.code === 'starBeacon') {
        data.value && self.setState(data.code, data);
        // self.addExtraField(data);
        return false;
      }

      return data;
    },

    defaultInitFieldFinishAction: (data, self) => {
      const quickFilterIds = self.getState('quickFilterIds');
      const starBeacon = self.getState('starBeacon');
      const value = [];
      starBeacon && value.push('myStarBeacon');
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
      return !isInProgram() ? options.filter((item) => item.value !== 'feature') : options;
    },
  });

  originOpenExportIssueModal(fields, chosenFields, tableDataSet, tableRef, store, action);
}
export { openExportIssueModal };
