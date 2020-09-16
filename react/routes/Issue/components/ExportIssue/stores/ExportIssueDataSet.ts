import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { AxiosRequestConfig } from 'axios';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { getProjectId } from '@/utils/common';
import { issueTypeApiConfig } from '@/api';
import { message } from 'choerodon-ui';

interface Props {
  userFields: Array<FieldProps>,
  selectedFields?: any,
  isInProgram: boolean,
}
const ExportIssueDataSet = ({ userFields, selectedFields, isInProgram }: Props): DataSetProps => ({
  autoQuery: false,
  autoCreate: true,
  // dataKey: 'content',
  paging: false,
  selection: undefined,
  fields: [
    {
      name: 'exportFieldCodes', multiple: true, label: '', defaultValue: selectedFields,
    },
    {
      name: 'statusId',
      label: '状态',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/schemes/query_status_by_project_id?apply_type=${'agile'}`,
        method: 'get',
      }),
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'sprint',
      label: '冲刺',
      required: true,
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/sprint/names`,
        method: 'post',
      }),
      valueField: 'sprintId',
      textField: 'sprintName',
    },
    {
      name: 'issueTypeId',
      label: '问题类型',
      lookupAxiosConfig: () => ({
        ...issueTypeApiConfig.loadAllWithStateMachineId() as AxiosRequestConfig,
        transformResponse: (data: string | Array<{ name: string, typeCode: string, id: string }>) => {
          try {
            const newData: Array<{ name: string, typeCode: string, id: string }> = JSON.parse(data as string);
            return newData.filter((item) => item.typeCode !== 'feature');
          } catch (error) {
            if (Array.isArray(data)) {
              return data;
            }
            message.error('数据加载错误');
            return undefined;
          }
        },
      }),
      valueField: 'id',
      textField: 'name',
    },
    ...isInProgram ? [{
      name: 'featureId',
      label: '所属特性',
      valueField: 'issueId',
      textField: 'summary',
    }] : [{
      name: 'epicId',
      label: '所属史诗',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/issues/epics/select_data`,
        method: 'get',
      }),
      valueField: 'issueId',
      textField: 'epicName',
    }], {
      name: 'priorityId',
      label: '优先级',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/priority/list_by_org`,
        method: 'get',
        transformResponse: (response) => {
          try {
            const data = JSON.parse(response);
            return data.filter((v: any) => v.enable);
          } catch (error) {
            return response;
          }
        },
      }),
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'labelIssueRelVOList',
      label: '标签',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/issue_labels`,
        method: 'get',
      }),
      valueField: 'labelId',
      textField: 'labelName',
    }, {
      name: 'componentIssueRelVOList',
      label: '模块',
      lookupAxiosConfig: ({ record, dataSet: ds, params }) => ({
        url: `/agile/v1/projects/${getProjectId()}/component/query_all`,
        method: 'post',
        data: {
          advancedSearchArgs: {},
          searchArgs: { name: params.name },
        },
        params: {
          size: 999,
          page: 1,
        },
        transformResponse: (response) => {
          try {
            const data = JSON.parse(response);
            return data.content;
          } catch (error) {
            return response;
          }
        },
      }),
      valueField: 'componentId',
      textField: 'name',
    }, {
      name: 'version',
      label: '版本',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
        method: 'post',
        data: [],
      }),
      valueField: 'versionId',
      textField: 'name',
    },
    ...userFields,
  ],

});
export default ExportIssueDataSet;
