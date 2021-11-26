import React, {
  useCallback, useEffect, useMemo, useRef, useState,
} from 'react';
import { stores } from '@choerodon/boot';
import {
  DataSet, Row, Col, Spin, Form, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import {
  castArray,
  every,
  filter,
  find, get, includes, map, merge, some, uniq,
} from 'lodash';
import { toJS } from 'mobx';
import { UploadFile } from 'choerodon-ui/lib/upload/interface';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import moment from 'moment';
import UploadButton from '@/components/CommonComponent/UploadButton';
import validateFile from '@/utils/File';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import {
  IIssueType, IModalProps, IssueCreateFields, Priority, User,
} from '@/common/types';
import useIssueCreateFields from '@/hooks/data/useIssueCreateFields';
import { fieldApi, issueApi } from '@/api';
import { getProjectId } from '@/utils/common';
import useIsInProgram from '@/hooks/useIsInProgram';
import { ICascadeLinkage } from '@/routes/page-config/components/setting-linkage/Linkage';
import useDeepMemo from '@/hooks/useDeepMemo';
import { MINUTE } from '@/constants/DATE_FORMAT';
import WSJF from './components/wsjf';
import IssueLink from './components/issue-link';
import hooks from './hooks';
// import getFieldConfig from './fields';
import getFieldConfig from '@/components/field-pro/layouts/create';
import { insertField } from './utils';
import useFormatMessage from '@/hooks/useFormatMessage';

const { AppState } = stores;
interface CreateIssueBaseCallbackData {
  data: {
    [key: string]: any
  }
  fieldList: {
    fieldType: string,
    value: any,
    fieldId: string,
    fieldCode: string,
  }[]
  fileList: UploadFile[]
}
export interface CreateIssueBaseProps {
  onSubmit: ({ data, fieldList }: CreateIssueBaseCallbackData) => void,
  /** 返回 boolean 类型则会阻止关闭或 关闭 弹窗 */
  onAfterSubmitError?: ({ data, fieldList }: CreateIssueBaseCallbackData, errorData: any) => void | Promise<boolean> | boolean,
  modal?: IModalProps,
  projectId?: string,
  menuType?: 'project' | 'org',
  defaultTypeCode?: string
  defaultTypeId?: string
  defaultAssignee?: {
    id: string,
    realName: string,
  }
  defaultFeature?: {
    summary: string
    issueId: string
  }
  parentIssue?: {
    summary: string
    issueId: string
  }
  defaultValues?: {
    [key: string]: any
  }
  /** 限定可选工作项类型 */
  typeCode?: string | string[]
  /** 是否在项目群创建 */
  isProgram?: boolean
  showSelectProject?: boolean,
  /** 额外的需要必填的字段建 */
  extendRequiredCodes?: string[]
}
const defaultDataSet = new DataSet({
  autoCreate: true,
  fields: [],
});
const presets = new Map([
  ['component', {
    type: 'object',
    valueField: 'componentId',
  }],
  ['label', {
    type: 'object',
    valueField: 'labelId',
  }],
  ['estimatedStartTime', {
    max: 'estimatedEndTime',
    format: MINUTE,
  }],
  ['estimatedEndTime', {
    min: 'estimatedStartTime',
    format: MINUTE,
  }],
  ['actualStartTime', {
    max: 'actualEndTime',
    format: MINUTE,
  }],
  ['actualEndTime', {
    min: 'actualStartTime',
    format: MINUTE,
  }],
]);
const afterLoadKeyMap = new Map([
  ['component', 'componentId'],
  ['priority', 'id'],
  ['fixVersion', 'versionId'],
  ['influceVersion', 'versionId'],
  ['subProject', 'projectId'],
]);

const lineField = ['summary', 'description'];
const reuseFields = ['issueType', 'summary', 'description'];
const pageCascadeFields = ['component', 'priority', 'fixVersion', 'influenceVersion'];

function isSelect(field: IssueCreateFields | { fieldType: string }) {
  return includes(['radio', 'multiple', 'checkbox', 'single'], field.fieldType);
}
function isMultiple(field: IssueCreateFields | { fieldType: string }) {
  return field.fieldType === 'multiple' || field.fieldType === 'checkbox' || field.fieldType === 'multiMember';
}

function isSingle(field: IssueCreateFields | { fieldType: string }) {
  return includes(['radio', 'single', 'member'], field.fieldType);
}

function getValue(dataSet: DataSet, fieldCode: string) {
  return toJS(dataSet.current?.get(fieldCode));
}

function getRuleDefaultValue(field: IssueCreateFields, rules: ICascadeLinkage[] = []) {
  const fieldRules = filter(rules, { cascadeFieldCode: field.fieldCode });
  if (filter(fieldRules, (rule) => rule.defaultValue || rule.defaultIds?.length).length === 1) { // 所有规则中只有1条设置了默认值
    const defaultValueRule = find(fieldRules, (rule) => rule.defaultValue || rule.defaultIds?.length) as ICascadeLinkage;
    return defaultValueRule.defaultValue || (isSingle({ fieldType: defaultValueRule.cascadeFieldType }) ? defaultValueRule.defaultIds?.length && defaultValueRule.defaultIds[0] : defaultValueRule.defaultIds);
  }
  return undefined;
}

function getRuleHidden(field?: Pick<IssueCreateFields, 'fieldType' | 'fieldCode' | 'defaultValueObj' | 'defaultValueObjs' | 'required'>, rules: ICascadeLinkage[] = []) {
  const fieldRules = filter(rules, { cascadeFieldCode: field?.fieldCode });
  if (field) {
    if (field.required || find(fieldRules, { required: true }) || every(fieldRules, (rule) => !rule.hidden)) {
      return false;
    }
    return true;
  }
  return true;
}

function getRuleRequired(field: IssueCreateFields, rules: ICascadeLinkage[] = []) {
  const fieldRules = filter(rules, { cascadeFieldCode: field.fieldCode });
  return !!find(fieldRules, { required: true });
}

function getOptionsData(rules: ICascadeLinkage[] = [], dataSet: DataSet, field: Pick<IssueCreateFields, 'fieldType' | 'fieldCode' | 'defaultValueObj' | 'defaultValueObjs' | 'required'>) {
  const fieldRules = filter(rules, { cascadeFieldCode: field.fieldCode });
  const ruleIds = fieldRules.length ? map(fieldRules, 'id') : undefined;
  return ({
    extendParams: field.fieldCode === 'fixVersion' ? ['sprint_planning', 'started'] : undefined,
    ruleIds,
    // eslint-disable-next-line no-nested-ternary
    selected: ruleIds?.length && getValue(dataSet, field.fieldCode) ? (isMultiple(field) ? getValue(dataSet, field.fieldCode) : [getValue(dataSet, field.fieldCode)]) : undefined,
  });
}

const hasValue = (dataSet: DataSet, field: IssueCreateFields) => (isMultiple(field) ? getValue(dataSet, field.fieldCode)?.length : getValue(dataSet, field.fieldCode));

function cascadeFieldAfterLoad(dataSet: DataSet, list: any[], field: IssueCreateFields, rules: ICascadeLinkage[] = []) {
  const key = afterLoadKeyMap.get(field.fieldCode) || 'id';
  const fieldCurrentValue = getValue(dataSet, field.fieldCode);
  const currentValueIsArr = Array.isArray(fieldCurrentValue);
  if (!hasValue(dataSet, field) || (!currentValueIsArr ? !find(list, { [key]: fieldCurrentValue }) : some(fieldCurrentValue, (id) => !find(list, { [key]: id })))) {
    const defaultValue = getRuleDefaultValue(field as IssueCreateFields, rules);
    if (defaultValue) {
      dataSet.current?.set(field.fieldCode, isSingle(field) ? defaultValue : uniq([...filter(fieldCurrentValue, (id) => find(list, { [key]: id })), ...defaultValue]));
    }
  }
}

function transformSubmitFieldValue(field: IssueCreateFields, value: any) {
  switch (field.fieldType) {
    case 'time':
    case 'date':
    case 'datetime': {
      return value && moment.isMoment(value) ? value.format('YYYY-MM-DD HH:mm:ss') : value && moment(value, ['YYYY-MM-DD HH:mm:ss', 'HH:mm:ss']).format('YYYY-MM-DD HH:mm:ss');
    }
    default: return value;
  }
}
const CreateIssueBase = observer(({
  modal,
  projectId,
  onSubmit,
  onAfterSubmitError,
  defaultTypeCode = 'story',
  defaultTypeId,
  defaultAssignee,
  defaultFeature,
  parentIssue,
  defaultValues,
  typeCode,
  isProgram,
  showSelectProject = false,
  extendRequiredCodes = [],
  menuType = 'project',
}: CreateIssueBaseProps) => {
  const formatMessage = useFormatMessage('agile.common');
  // formatMessage({}
  const [fileList, setFileList] = useState<UploadFile[]>([]);
  const dataSetRef = useRef(defaultDataSet);
  const currentTemplateSummary = useRef(defaultValues?.summary || '');
  const currentTemplateDescription = useRef(defaultValues?.description || '');
  const [templateSummary] = useState(new Map());
  const [dataSet, setDataSet] = useState(defaultDataSet);
  dataSetRef.current = dataSet;
  const issueTypeId = dataSet.current && dataSet.current.get('issueType');
  const setFieldValue = usePersistFn((name, value) => {
    dataSet.current?.set(name, value);
  });
  const { isFetching: isLoading, data: issueTypeList } = useProjectIssueTypes({
    projectId,
    typeCode,
    isProgram,
  }, {
    onSuccess: ((issueTypes) => {
      if (!issueTypeId) {
        setFieldValue('issueType', getDefaultIssueType(issueTypes));
      }
    }),
  });
  const getDefaultIssueType = (issueTypes: IIssueType[]) => {
    if (defaultTypeId && find(issueTypes, { id: defaultTypeId })) {
      return find(issueTypes, { id: defaultTypeId })?.id;
    }
    if (defaultTypeCode && find(issueTypes, { typeCode: defaultTypeCode })) {
      return find(issueTypes, { typeCode: defaultTypeCode })?.id;
    }
    if (issueTypes && issueTypes.length > 0) {
      return issueTypes[0].id;
    }
    return undefined;
  };

  const issueTypeCode = find(issueTypeList, {
    id: issueTypeId,
  })?.typeCode;
  const enableIssueLinks = issueTypeCode && !['sub_task', 'issue_epic', 'feature'].includes(issueTypeCode);
  const isSubIssue = issueTypeCode && ['sub_task', 'bug'].includes(issueTypeCode);

  const [{ data: fields, isFetching: isFieldsLoading }, {
    data: templateData,
  }, { data: cascadeRuleList = [] }] = useIssueCreateFields({ issueTypeId, projectId });
  const fieldValueArr = usePersistFn((field: IssueCreateFields) => {
    let value = castArray(getValue(dataSet, field.fieldCode));
    const preset = presets.get(field.fieldCode);
    if (preset) {
      if (preset.type === 'object') {
        value = value.map((v) => get(v, preset.valueField));
      }
    }
    return value;
  });
  const getAllRules = usePersistFn(() => {
    let allRules: ICascadeLinkage[] = [];
    fields?.forEach((field) => {
      if (((field.system && includes(pageCascadeFields, field.fieldCode)) || (!field.system && isSelect(field))) && hasValue(dataSet, field)) {
        allRules = [...allRules, ...filter(cascadeRuleList, (rule) => rule.fieldId === field.fieldId && includes(fieldValueArr(field), rule.fieldOptionId))];
      }
    });
    return allRules;
  });

  const rules = useDeepMemo(() => getAllRules());

  const {
    isInProgram,
    isShowFeature,
  } = useIsInProgram({ projectId });
  const showFeature = !!issueTypeCode && issueTypeCode === 'story' && !!isShowFeature;
  const getDefaultValue = usePersistFn((field: IssueCreateFields) => {
    const preset = presets.get(field.fieldCode);
    // defaultAssignee优先级更高
    if (field.fieldCode === 'assignee' && defaultAssignee) {
      return defaultAssignee.id;
    }
    if (field.fieldCode === 'reporter') {
      return AppState.userInfo.id;
    }
    // 通过外部设置的默认值优先
    if (defaultValues && defaultValues[field.fieldCode]) {
      return defaultValues[field.fieldCode];
    }
    // 页面字段级联rule的默认值
    const ruleDefaultValue = getRuleDefaultValue(field, rules);
    if (Array.isArray(ruleDefaultValue) ? ruleDefaultValue.length : ruleDefaultValue) {
      return ruleDefaultValue;
    }
    if (preset) {
      if (preset.type === 'object') {
        return isMultiple(field) ? field.defaultValueObjs : field.defaultValueObj;
      }
      // 格式化时间格式
      if (field.defaultValue && preset.format && field.fieldType === 'datetime') {
        return moment(field.defaultValue).format(preset.format);
      }
    }
    if (field.defaultValue === '') {
      return undefined;
    }
    if (field.fieldType === 'time') {
      return field.defaultValue?.split(' ')[1];
    }
    if (field.fieldType === 'date') {
      return field.defaultValue?.split(' ')[0];
    }
    return field.defaultValue;
  });

  const handleUpdate = usePersistFn(async ({
    name, value, oldValue, record,
  }) => {
    switch (name) {
      case 'parentIssueId': {
        if (value) {
          try {
            const res = await issueApi.load(value?.issueId, projectId);
            const { activeSprint } = res || {};
            record.set('sprint', activeSprint?.sprintId ?? undefined);
          } catch (e) {
            record.set('sprint', undefined);
          }
        }
        break;
      }
      case 'assignee': {
        // 标识经办人是否手动选择过，以便判断是否赋值为模块负责人
        record.setState('changeAssignee', !!value);
        break;
      }
      case 'component': {
        // 若经办人未手动选择，按照选中的模块顺序，经办人默认赋值为第一个模块负责人
        if (!record.getState('changeAssignee')) {
          if (value && value.length) {
            const hasManagerId = value.some((component: any) => {
              if (component.managerId) {
                record.init('assignee', component.managerId);
                return true;
              }
              return false;
            });
            if (!hasManagerId && record.get('assignee')) {
              record.init('assignee', null);
            }
          } else if (record.get('assignee')) {
            record.init('assignee', null);
          }
        }
        break;
      }
      case 'issueType': {
        if (!value) {
          // 工作项类型不能置空
          record.set('issueType', oldValue);
        }
      }
      default: {
        break;
      }
    }
  });

  // 设置概要默认前缀
  const setSummaryValue = useCallback(async (newDataSet) => {
    const newIssueTypeId = newDataSet.current?.get('issueType');
    const oldSummary = newDataSet.current?.get('summary');
    if (!newIssueTypeId) {
      return;
    }
    if (!oldSummary || currentTemplateSummary.current === oldSummary) {
      let prefix: string | undefined = '';
      if (templateSummary.has(newIssueTypeId)) {
        prefix = templateSummary.get(newIssueTypeId);
      } else {
        prefix = await fieldApi.project(projectId).getSummaryDefaultValue(newIssueTypeId);
        templateSummary.set(newIssueTypeId, prefix);
      }
      currentTemplateSummary.current = prefix as string;
      newDataSet.current?.init('summary', prefix);
    }
  }, [templateSummary, currentTemplateSummary.current]);

  useEffect(() => {
    const oldDataSet = dataSetRef.current;
    const newDataSet = new DataSet({
      autoCreate: false, // 子项目 过滤史诗
      fields: fields ? insertField([...fields.filter((field) => (isInProgram ? field.fieldCode !== 'epic' : true)).map((field) => {
        const preset = presets.get(field.fieldCode) ?? {};
        return merge(preset, {
          name: field.fieldCode,
          fieldId: field.fieldId,
          fieldType: field.fieldType,
          fieldCode: field.fieldCode,
          label: field.fieldName,
          required: field.required || getRuleRequired(field, rules) || extendRequiredCodes.includes(field.fieldCode),
          multiple: isMultiple(field),
        });
      })], [{
        insert: !!isSubIssue,
        after: 'issueType',
        field: {
          name: 'parentIssueId',
          type: 'object' as any,
          label: '关联父级任务',
          required: issueTypeCode === 'sub_task',
        },
      }, {
        insert: showFeature,
        after: 'issueType',
        field: {
          name: 'feature',
          label: '特性',
          required: false,
        },
      }]) : [],
      events: {
        update: handleUpdate,
      },
    });
    const newValue: { [key: string]: any } = {};
    // 优先保留之前的值
    reuseFields.forEach((name) => {
      const oldValue = oldDataSet.current?.get(name);
      if (oldValue) {
        newValue[name] = oldValue;
      }
    });
    newDataSet.fields.forEach(({ name }) => {
      const oldValue = toJS(oldDataSet.current?.get(name));
      if (oldValue) {
        newValue[name] = oldValue;
      }
    });
    const setValue = (name: string, value: any) => {
      if (newValue[name] === null || newValue[name] === undefined || (Array.isArray(newValue[name]) && !newValue[name].length)) {
        newValue[name] = value;
      }
    };

    // 设置默认值
    fields?.forEach((field) => {
      const defaultValue = getDefaultValue(field);
      if (defaultValue !== null && defaultValue !== undefined) {
        // 没有值的时候再设置
        setValue(field.fieldCode, defaultValue);
        if (field.fieldCode === 'summary' && !currentTemplateSummary.current) {
          currentTemplateSummary.current = defaultValue;
          templateSummary.set(issueTypeId, defaultValue);
        }
      }
    });
    // TODO: 将各种默认值的获取和设置逻辑合并
    // 设置描述默认值  和上一个模板相同 或默认值
    if (currentTemplateDescription.current === newValue.description) {
      newValue.description = templateData?.template;
    }
    currentTemplateDescription.current = templateData?.template;
    if (parentIssue) {
      setValue('parentIssueId', parentIssue);
    }
    if (showFeature && defaultFeature) {
      setValue('feature', defaultFeature.issueId);
    }
    if (issueTypeCode === 'feature') {
      setValue('jobSize', '1');
    }
    // 创建一个新的
    newDataSet.create(newValue);
    setDataSet(newDataSet);
    setSummaryValue(newDataSet);
  }, [defaultFeature, fields, getDefaultValue, handleUpdate, isInProgram, isShowFeature, isSubIssue, issueTypeCode, issueTypeId, parentIssue, rules, setSummaryValue, showFeature, templateData, templateSummary]);
  const getIssueLinks = usePersistFn(() => {
    const links = issueLinkDataSet.toData() as {
      issueIds: string[]
      linkType: string
    }[];
    const issueLinkCreateVOList: {
      linkTypeId: string,
      linkedIssueId: string,
      in: boolean
    }[] = [];
    links.forEach((link) => {
      const [linkTypeId, isIn] = link.linkType.split('+');
      const issueIds = link.issueIds as string[];
      if (issueIds) {
        issueIds.forEach((issueId) => {
          issueLinkCreateVOList.push({
            linkTypeId,
            linkedIssueId: issueId,
            in: isIn === 'true',
          });
        });
      }
    });
    return issueLinkCreateVOList;
  });
  const handleSubmit = usePersistFn(async () => {
    if (showSelectProject && !projectId) {
      return false;
    }
    if (await dataSet.validate()) {
      if (enableIssueLinks && !await issueLinkDataSet.validate()) {
        return false;
      }
      const data = dataSet.current?.toData();
      const customFields = fields?.filter((f) => !f.system).filter((f) => !getRuleHidden(f, rules));
      const systemFields = fields?.filter((f) => f.system).filter((f) => !getRuleHidden(f, rules));
      const fieldList = customFields?.map((field) => ({
        fieldType: field.fieldType,
        value: transformSubmitFieldValue(field, data[field.fieldCode]),
        fieldId: field.fieldId,
        fieldCode: field.fieldCode,
      })) ?? [];
      let values = systemFields?.reduce((res, field) => {
        const config = getFieldConfig(field);
        return {
          ...res,
          ...{
            [config.valueKey ?? field.fieldCode]: data[field.fieldCode],
          },
        };
      }, {}) ?? {};
      const issueType = find(issueTypeList, {
        id: data.issueType,
      });
      Object.assign(values, {
        typeCode: (issueType as IIssueType)?.typeCode,
        priorityCode: `priority-${data.priority || 0}`,
        parentIssueId: data.parentIssueId?.issueId,
        programId: projectId ?? getProjectId(),
        projectId: projectId ?? getProjectId(),
        featureId: data.feature,
        issueLinkCreateVOList: enableIssueLinks ? getIssueLinks() : undefined,
        componentIssueRelVOList: data.component ? data.component.map((item: { componentId: string }) => ({ componentId: item.componentId })) : [],
      });

      values = hooks.reduce((result, hook) => hook(result, data), values);
      try {
        await onSubmit({
          data: values, fieldList, fileList,
        });
      } catch (error) {
        const res = onAfterSubmitError && await onAfterSubmitError({
          data: values, fieldList, fileList,
        }, error);
        return typeof res === 'boolean' ? res : false;
      }

      return true;
    }
    return false;
  });
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  // hidden, 可见项
  const getFieldProps = usePersistFn((field?: Pick<IssueCreateFields, 'fieldType' | 'fieldCode' | 'defaultValueObj' | 'defaultValueObjs' | 'required'>): { [key: string]: any } => {
    if (!field) {
      return {};
    }
    switch (field.fieldCode) {
      case 'parentIssueId': {
        return {
          issueType: issueTypeCode,
          hidden: !!parentIssue,
        };
      }
      case 'status': {
        return {
          issueTypeId,
        };
      }
      case 'assignee': {
        return {
          extraOptions: [defaultAssignee].filter(Boolean),
          onAssigneeMe: (userInfo: User) => {
            setFieldValue('assignee', userInfo.id);
          },
          hidden: getRuleHidden(field, rules),
          selected: dataSet.current?.get('assignee'),
        };
      }
      case 'mainResponsible':
      case 'estimatedStartTime':
      case 'estimatedEndTime':
      case 'actualStartTime':
      case 'actualEndTime':
      case 'benfitHypothesis':
      case 'acceptanceCritera': {
        return {
          hidden: getRuleHidden(field, rules),
        };
      }
      case 'reporter': {
        return {
          hidden: getRuleHidden(field, rules),
          selected: dataSet.current?.get('reporter'),
        };
      }
      case 'participant': {
        return {
          hidden: getRuleHidden(field, rules),
          selected: dataSet.current?.get('participant'),
        };
      }
      case 'component':
      case 'fixVersion':
      case 'influenceVersion':
      case 'subProject': {
        return {
          afterLoad: (res: any) => cascadeFieldAfterLoad(dataSet, res, field as IssueCreateFields, rules),
          hidden: getRuleHidden(field, rules),
          ...getOptionsData(rules, dataSet, field),
        };
      }
      case 'priority': {
        return {
          afterLoad: (priorities: Priority[]) => {
            const defaultPriority = find(priorities, { default: true });
            if (defaultPriority && !hasValue(dataSet, field as IssueCreateFields)) {
              dataSet.current?.set('priority', defaultPriority.id);
            }
            cascadeFieldAfterLoad(dataSet, priorities, field as IssueCreateFields, rules);
          },
          hidden: getRuleHidden(field, rules),
          ...getOptionsData(rules, dataSet, field),
        };
      }
      case 'issueType': {
        return {
          isProgram,
          config: {
            typeCode,
            projectId,
            menuType: menuType ?? 'project',
          },
        };
      }
      case 'feature': {
        return defaultFeature ? {
          featureId: defaultFeature.issueId,
          featureName: defaultFeature.summary,
        } : {};
      }
      case 'tag': {
        return {
          mode: isProgram ? 'program' : 'project',
        };
      }
      case 'sprint': {
        return {
          disabled: issueTypeCode === 'sub_task',
        };
      }
      case 'summary': {
        return {
          maxLength: 44,
        };
      }
      default: break;
    }
    switch (field.fieldType) {
      case 'member': {
        return {
          extraOptions: [field.defaultValueObj].filter(Boolean),
          hidden: getRuleHidden(field, rules),
        };
      }
      case 'multiMember': {
        return {
          extraOptions: field.defaultValueObjs,
          hidden: getRuleHidden(field, rules),
        };
      }
      case 'single': case 'multiple': case 'checkbox': case 'radio': {
        return {
          ...getOptionsData(rules, dataSet, field),
          hidden: getRuleHidden(field, rules),
          afterLoad: (res: any) => cascadeFieldAfterLoad(dataSet, res, field as IssueCreateFields, rules),
        };
      }
      default: return {
        hidden: getRuleHidden(field, rules),
      };
    }
  });
  const renderFields = usePersistFn(() => (
    [...dataSet.fields.values()].filter((f) => f.get('display') !== false).map((dataSetField) => {
      const { name, required } = dataSetField;
      const fieldType = dataSetField.get('fieldType');
      const fieldId = dataSetField.get('fieldId');
      const config = getFieldConfig({
        fieldCode: name,
        fieldType,
      });
      const field = find(fields, { fieldCode: name });

      const extraProps = getFieldProps({
        required: required || false,
        fieldCode: name,
        fieldType,
        defaultValueObj: field?.defaultValueObj,
        defaultValueObjs: field?.defaultValueObjs,
      });
      if (extraProps.hidden) {
        return null;
      }
      return config
        ? ([
          config.renderFormItem({
            name,
            fieldId,
            projectId,
            clearButton: !required,
            multiple: dataSetField.get('multiple'),
            style: {
              width: '100%',
            },
            colSpan: lineField.includes(dataSetField.name) ? 2 : 1,
            newLine: lineField.includes(dataSetField.name),
            ...extraProps,
          }),
          name === 'description' ? (
            <div
              // @ts-ignore
              newLine
              colSpan={2}
            >
              <UploadButton
                fileList={fileList}
                onChange={({ fileList: files }) => {
                  if (validateFile(files)) {
                    setFileList(files);
                  }
                }}
              />
            </div>
          ) : null,
        ])
        : null;
    })
  ));
  const issueLinkDataSet = useMemo(() => new DataSet({
    // autoCreate: true,
    fields: [{
      name: 'linkType',
      label: '关系',
      required: true,
    }, {
      name: 'issueIds',
      label: '工作项',
      required: true,
      multiLine: true,
    }],
    events: {
      update: ({ name, record: current }: { name: string, record: Record }) => {
        if (name === 'linkType') {
          current.init('issueIds');
        }
      },
    },
  }), []);

  return (
    <Spin spinning={isLoading || isFieldsLoading}>
      <Form
        dataSet={dataSet}
        columns={2}
      >
        {parentIssue ? <TextField label="父任务概要" value={parentIssue.summary} disabled colSpan={2} /> : null}
        {renderFields()}
      </Form>
      {issueTypeCode === 'feature' ? <WSJF dataSet={dataSet} /> : null}
      {enableIssueLinks ? <IssueLink projectId={projectId} dataSet={issueLinkDataSet} /> : null}
    </Spin>
  );
});

export default CreateIssueBase;
