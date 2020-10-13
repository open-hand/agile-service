import React, { useMemo } from 'react';
import { DataSet, Modal, Table } from 'choerodon-ui/pro/lib';
import { openExportIssueModal as originOpenExportIssueModal } from '@/components/issue-export';
import IssueExportStore from '@/components/issue-export/stores/store';
import { issueApi } from '@/api';
import { IChosenFieldField } from '@/components/chose-field/types';
import { getExportFieldCodes, getTransformSystemFilter, getFilterFormSystemFields } from './utils';

function openExportIssueModal(fields: Array<IChosenFieldField>, chosenFields: Array<any>,
  tableDataSet: DataSet, tableRef: React.RefObject<Table>) {
  const store = new IssueExportStore({
    defaultInitFieldAction: (data, self) => {
      if (data.code === 'sprint') {
        return ({ ...data, immutableCheck: true });
      }
      if (data.code === 'quickFilterIds') {
        self.addExtraField(data);
        return false;
      }
      return data;
    },
    dataSetSystemFields: getFilterFormSystemFields(),
    transformSystemFilter: getTransformSystemFilter,
    transformExportFieldCodes: getExportFieldCodes,
    events: {
      exportAxios: (searchData, sort) => issueApi.export(searchData, sort),
      loadRecordAxios: () => issueApi.loadLastImportOrExport('download_file'),
    },
  });
  originOpenExportIssueModal(fields, chosenFields, tableDataSet, tableRef, store);
}
export { openExportIssueModal };
