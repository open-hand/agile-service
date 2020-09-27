import React, { useMemo } from 'react';
import { DataSet, Modal, Table } from 'choerodon-ui/pro/lib';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import ExportIssue from '@/components/issue-export';
import { IExportIssueField } from '@/components/issue-export/types';
import IssueExportStore from '@/components/issue-export/stores/store';
import { issueApi } from '@/api';
import { getExportFieldCodes, getTransformSystemFilter, getFilterFormSystemFields } from './utils';

function openExportIssueModal(fields: Array<IExportIssueField>, chosenFields: Array<any>,
  tableDataSet: DataSet, tableRef: React.RefObject<Table>) {
  const store = new IssueExportStore({
    defaultInitFieldAction: (data) => {
      if (data.code === 'sprint') {
        return ({ ...data, immutableCheck: true });
      }
      return data;
    },
    dataSetSystemFields: getFilterFormSystemFields(),
    transformSystemFilter: getTransformSystemFilter,
    transformExportFieldCodes: getExportFieldCodes,
    actions: {
      exportAxios: (searchData, sort) => issueApi.export(searchData, sort),
      loadRecordAxios: () => issueApi.loadLastImportOrExport('download_file'),
    },
  });

  const checkOptions: Array<Field> = [...tableDataSet.fields.values()];
  Modal.open({
    key: Modal.key(),
    title: '导出问题',
    style: {
      width: 380,
    },
    className: 'c7n-agile-export-issue-modal',
    drawer: true,
    children: <ExportIssue
      fields={fields}
      chosenFields={chosenFields}
      checkOptions={checkOptions.map((option) => ({ value: option.props.name, label: option.props.label, order: option.order }))}
      tableRef={tableRef}
      store={store}
    />,
    okText: '关闭',
    okCancel: false,
  });
}
export { openExportIssueModal };
