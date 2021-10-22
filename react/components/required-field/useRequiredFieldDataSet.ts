import {
  useCallback, useMemo, useRef,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import {
  assign,
} from 'lodash';
import { getProjectId } from '@/utils/common';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IField } from '@/common/types';
import { formatFields, systemFields } from './util';
import { IFieldsValueVo } from './RequiredField';
import { epicConfigApi } from '@/api';

interface Props {
  issueId: string
  issueTypeId: string
  requiredFields: IField[]
}

export interface RequiredFieldDs {
  issueId: string
  dataSet: DataSet,
  getData: () => IFieldsValueVo,
}

const useRequiredFieldDataSet = (issuesFieldRequired: Props[], projectId?: string): RequiredFieldDs[] => {
  const { isInProgram } = useIsInProgram({ projectId });
  const dataSetMapRef = useRef<Map<string, DataSet>>();
  const dataSetMap = useMemo(() => new Map(), []);
  // @ts-ignore
  dataSetMapRef.current = dataSetMap;
  const getLookupFields = useCallback((issueTypeId) => (!issueTypeId ? [] : [{
    name: 'statusId',
    label: '状态',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${projectId || getProjectId()}/schemes/query_status_by_issue_type_id?apply_type=agile&issue_type_id=${issueTypeId}`,
      method: 'get',
    }),
    valueField: 'id',
    textField: 'name',
  }, {
    name: 'sprintId',
    label: '冲刺',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${projectId || getProjectId()}/sprint/names`,
      method: 'post',
      data: ['started', 'sprint_planning'],
    }),
    valueField: 'sprintId',
    textField: 'sprintName',
  },
  ...isInProgram ? [{
    name: 'featureId',
    label: '所属特性',
    valueField: 'issueId',
    textField: 'summary',
  }] : [{
    name: 'epicId',
    label: '所属史诗',
    lookupAxiosConfig: () => epicConfigApi.loadEpicsForSelect(projectId, { size: 0 }),
    valueField: 'issueId',
    textField: 'epicName',
  }], {
    name: 'priorityId',
    label: '优先级',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${projectId || getProjectId()}/priority/list_by_org`,
      method: 'get',
      transformResponse: (response: any) => {
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
      url: `/agile/v1/projects/${projectId || getProjectId()}/issue_labels`,
      method: 'get',
    }),
    valueField: 'labelId',
    textField: 'labelName',
  }, {
    name: 'componentIssueRelVOList',
    label: '模块',
    lookupAxiosConfig: ({ params }: { params: any }) => ({
      url: `/agile/v1/projects/${projectId || getProjectId()}/component/query_all`,
      method: 'post',
      data: {
        advancedSearchArgs: {},
        searchArgs: { name: params.name },
      },
      params: {
        size: 999,
        page: 1,
      },
      transformResponse: (response: any) => {
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
    name: 'fixVersion',
    label: '修复的版本',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${projectId || getProjectId()}/product_version/names`,
      method: 'post',
      data: ['version_planning'],
    }),
    valueField: 'versionId',
    textField: 'name',
  }, {
    name: 'influenceVersion',
    label: '影响的版本',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${projectId || getProjectId()}/product_version/names`,
      method: 'post',
      data: [],
    }),
    valueField: 'versionId',
    textField: 'name',
  }]), [isInProgram, projectId]);

  const getRequiredFieldDataSet = useCallback((issue: Props) => {
    const newDataSet = new DataSet({
      autoCreate: true,
      fields: [
        ...(issue.requiredFields.map((item) => {
          const key = item.fieldCode === 'epic' && isInProgram ? 'featureId' : (systemFields.get(item.fieldCode as string)?.id || item?.fieldCode);
          const lookupField = {};
          const lookupFields = getLookupFields(issue.issueTypeId);
          if (item.system && lookupFields.find((field) => field.name === key)) {
            assign(lookupField, lookupFields.find((field) => field.name === key));
          }
          return ({
            ...lookupField,
            name: item.fieldCode === 'epic' && isInProgram ? 'featureId' : item.fieldCode,
            label: item.fieldCode === 'epic' && isInProgram ? '特性' : item.fieldName,
            required: true,
          });
        }))],
    });

    const oldDataSet = dataSetMapRef.current?.get(issue.issueId);
    const newValue: { [key: string]: any } = {};
    // 从旧的dataSet拿值
    newDataSet.fields.forEach(({ name }) => {
      const oldValue = toJS(oldDataSet?.current?.get(name));
      if (oldValue) {
        newValue[name] = oldValue;
      }
    });
    const setValue = (name: string, value: any) => {
      // 没有值的时候再设置
      if (newValue[name] === null || newValue[name] === undefined) {
        newValue[name] = value;
      }
    };

    // 设置默认值
    issue.requiredFields?.forEach((field) => {
      const defaultValue = field.defaultValue === '' ? undefined : field.defaultValue;
      if (defaultValue !== null && defaultValue !== undefined) {
        setValue(field.fieldCode as string, defaultValue);
      }
    });

    newDataSet.create(newValue);
    dataSetMap.set(issue.issueId, newDataSet);
    return newDataSet;
  }, [dataSetMap, getLookupFields, isInProgram]);

  const getDsData = useCallback(({ requiredFields, ds }) => {
    const temp = ds.current ? ds.current.toData() : {};
    const obj = {};
    requiredFields.forEach((field: IField) => {
      if (field.fieldCode) {
        const key = field.fieldCode === 'epic' && isInProgram ? 'featureId' : field.fieldCode;
        assign(obj, { [key]: temp[key] });
      }
    });
    return obj;
  }, [isInProgram]);

  const getData = useCallback(({ issue, ds }) => ({
    issueIds: [issue.issueId],
    ...formatFields(issue.requiredFields, getDsData({ requiredFields: issue.requiredFields, ds }), ds),
  }), [getDsData]);

  const firstDs = useMemo(() => {
    const requiredFieldDs = getRequiredFieldDataSet(issuesFieldRequired[0]);
    if (!issuesFieldRequired[0].requiredFields.length) {
      return undefined;
    }
    return ({
      issueId: issuesFieldRequired[0].issueId,
      dataSet: requiredFieldDs,
      getData: () => getData({ issue: issuesFieldRequired[0], ds: requiredFieldDs }),
    });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [getData, getRequiredFieldDataSet, JSON.stringify(issuesFieldRequired[0])]);

  const otherDs = useMemo(() => issuesFieldRequired.slice(1, issuesFieldRequired.length).filter((item) => item.requiredFields?.length).map((issue) => {
    const requiredFieldDs = getRequiredFieldDataSet(issue);
    return ({
      issueId: issue.issueId,
      dataSet: requiredFieldDs,
      getData: () => getData({ issue, ds: requiredFieldDs }),
    });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }), [getData, getRequiredFieldDataSet, JSON.stringify(issuesFieldRequired)]);

  const requiredFieldDsArr = useMemo(() => [...firstDs ? [firstDs] : [], ...otherDs], [firstDs, otherDs]);

  return requiredFieldDsArr;
};

export default useRequiredFieldDataSet;
