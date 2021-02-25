import React from 'react';
import { DataSet, Table } from 'choerodon-ui/pro/lib';
import { openExportIssueModal as originOpenExportIssueModal } from '@/components/issue-export';
import IssueExportStore from '@/components/issue-export/stores/store';
import { issueApi } from '@/api';
import { IChosenFieldField } from '@/components/chose-field/types';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { getExportFieldCodes, getTransformSystemFilter, getFilterFormSystemFields } from './utils';

function openExportIssueModal(fields: Array<IChosenFieldField>, chosenFields: Array<any>,
  tableDataSet: DataSet, tableRef: React.RefObject<Table>) {
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
    events: {
      exportAxios: (searchData, sort) => issueApi.export(searchData, sort),
      loadRecordAxios: () => issueApi.loadLastImportOrExport('download_file'),
    },
  });
  originOpenExportIssueModal(fields, chosenFields, tableDataSet, tableRef, store, undefined, (options) => {
    options.splice(3, 0, {
      label: '描述',
      value: 'description',
    });
    return options;
  });
}
export { openExportIssueModal };
