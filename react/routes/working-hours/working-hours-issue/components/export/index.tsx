import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import {
  find,
  merge, set, uniq, unset,
} from 'lodash';
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
  columns: Array<{ code: string, title: string, sortId?: string }>
  menuType?: 'project' | 'workbench'
  visibleColumns?: string[]
}
function openExportWorkHoursIssueModal({
  fields, columns, chosenFields, visibleColumns = [], menuType = 'project', attachSearchArgs,
}: IExportWorkHoursIssueModalProps) {
  function transformSystemFilter(data: any) {
    const search = getTransformSystemFilter(data);
    set(search, 'searchArgs.projectIds', [...(data.projectIds || [])]);
    return search;
  }
  const commonStoreProps = {
    events: {
      exportAxios: (searchData, sort) => {
        merge(searchData, { searchArgs: attachSearchArgs });
        if (menuType === 'project') {
          const checkCustomFieldMap = new Map(fields.map((item) => [item.code, item.id]));

          const { exportFieldCodes = [] } = searchData;
          (exportFieldCodes as any[]).splice(0, 0, 'typeName');
          set(searchData, 'displayFields', uniq(exportFieldCodes).map((code: string) => {
            const id = checkCustomFieldMap.get(code);
            return { id, code };
          }));
          unset(searchData, 'exportFieldCodes');
        } else {
          set(searchData, 'displayFields', [
            'typeName',
            'summary',
            'workTime',
            'cumulativeWorkTime',
            'estimateTime',
            'deviationRate'].map((code) => ({ code })));
        }

        return workingHoursApi.exportHours(searchData);
      },
      loadRecordAxios: () => issueApi.loadLastImportOrExport('download_file_issue_work_hours'),
    },
    defaultCheckedExportFields: ['typeName'],
    defaultInitOptions: ({ options }) => {
      options.splice(0, 0, {
        label: '问题类型',
        value: 'typeName',
      });
      return options;
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
        if (data.code === 'issueTypeId') {
          return { ...data, otherComponentProps: { excludeTypeCodes: ['issue_epic'] } as any };
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
      renderField: (field, otherComponentProps) => getSearchWorkbenchFields([field], {
        [field.code]: {
          ...otherComponentProps,
          name: field.code,
          placeholder: undefined,
          flat: false,
        },
      })[0] as React.ReactElement,
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
    />,
    footer: (okBtn: any, cancelBtn: any) => cancelBtn,
    cancelText: '关闭',
    cancelProps: {
      color: 'primary',
    },
  });
}
export default openExportWorkHoursIssueModal;
