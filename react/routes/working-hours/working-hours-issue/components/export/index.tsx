import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import { merge, set, unset } from 'lodash';
import { issueApi, TemplateAction, workingHoursApi } from '@/api';
import IssueExport from '@/components/issue-export';
import IssueExportStore, { IssueExportStoreProps } from '@/components/issue-export/stores/store';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IChosenFieldField } from '@/components/chose-field/types';
import { getExportFieldCodes, getTransformSystemFilter } from '@/routes/Issue/components/ExportIssue/utils';
import getSearchWorkbenchFields from '@/components/field-pro/layouts/searchWorkbench';
import { getProjectId, getOrganizationId } from '@/utils/common';

interface IExportWorkHoursIssueModalProps {
  fields: IChosenFieldField[]
  chosenFields: IChosenFieldField[]
  attachSearchArgs: any
  columns: Array<{ code: string, title: string }>
  menuType?: 'project' | 'workbench'
  visibleColumns?: string[]
}
function openExportWorkHoursIssueModal({
  fields, columns, chosenFields, visibleColumns = [], menuType = 'project', attachSearchArgs,
}: IExportWorkHoursIssueModalProps) {
  function transformSystemFilter(data:any) {
    const search = getTransformSystemFilter(data);
    set(search, 'searchArgs.projectIds', [...(data.projectIds || [])]);
    return search;
  }
  const commonStoreProps = {
    events: {
      exportAxios: (searchData, sort) => {
        merge(searchData, { searchArgs: attachSearchArgs });
        searchData.exportFieldCodes && set(searchData, 'displayFields', searchData.exportFieldCodes?.map((code: string) => ({ code })));
        unset(searchData, 'exportFieldCodes');
        return workingHoursApi.exportHours(searchData);
      },
      loadRecordAxios: () => issueApi.loadLastImportOrExport('download_file_issue_work_hours'),
    },
    transformSystemFilter,
  } as IssueExportStoreProps;
  const store = new IssueExportStore({
    ...commonStoreProps,
    ...menuType === 'project' ? {
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
      transformExportFieldCodes: getExportFieldCodes,
      wsMessageKey: `agile-export-issue-work-hours-${getProjectId()}`,
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
    } : {
      wsMessageKey: `agile-export-issue-work-hours-org-${getOrganizationId()}`,
      renderField: (field, otherComponentProps) => getSearchWorkbenchFields([field], { [field.code]: { ...otherComponentProps, placeholder: undefined, flat: false } })[0] as React.ReactElement,
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
      visibleCheckField={menuType === 'project'}
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
