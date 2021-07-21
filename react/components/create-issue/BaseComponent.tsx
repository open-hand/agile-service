import React, {
  useEffect, useMemo, useRef, useState,
} from 'react';
import {
  DataSet, Row, Col, Spin, Form, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import {
  castArray,
  every,
  filter,
  find, get, groupBy, includes, map, merge,
} from 'lodash';
import { toJS } from 'mobx';
import { UploadFile } from 'choerodon-ui/lib/upload/interface';
import UploadButton from '@/components/CommonComponent/UploadButton';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import validateFile from '@/utils/File';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import {
  IIssueType, IModalProps, IssueCreateFields, Priority, User,
} from '@/common/types';
import useIssueCreateFields from '@/hooks/data/useIssueCreateFields';
import moment from 'moment';

import { getProjectId } from '@/utils/common';
import useIsInProgram from '@/hooks/useIsInProgram';
import { pageConfigApi } from '@/api';
import { ICascadeLinkage } from '@/routes/page-config/components/setting-linkage/Linkage';
import useDeepMemo from '@/hooks/useDeepMemo';
import WSJF from './components/wsjf';
import IssueLink from './components/issue-link';
import hooks from './hooks';
import getFieldConfig from './fields';
import { insertField } from './utils';

export interface CreateIssueBaseProps {
  onSubmit: ({ data, fieldList }: {
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
  }) => void,
  modal?: IModalProps,
  projectId?: string,
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
  /** 限定可选问题类型 */
  typeCode?: string | string[]
  /** 是否在项目群创建 */
  isProgram?: boolean
}
const defaultDataSet = new DataSet({
  autoCreate: true,
  fields: [],
});
const presets = new Map([
  ['component', {
    // type: 'object',
    valueField: 'componentId',
  }],
  ['label', {
    type: 'object',
    valueField: 'labelId',
  }],
  ['estimatedStartTime', {
    max: 'estimatedEndTime',
  }],
  ['estimatedEndTime', {
    min: 'estimatedStartTime',
  }],
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

function getRuleDefaultValue(field: IssueCreateFields, rules: ICascadeLinkage[] = []) {
  const fieldRules = filter(rules, { cascadeFieldCode: field.fieldCode });
  if (filter(fieldRules, (rule) => rule.defaultValue || rule.defaultIds?.length).length === 1) { // 所有规则中只有1条设置了默认值
    const defaultValueRule = find(fieldRules, (rule) => rule.defaultValue || rule.defaultIds?.length) as ICascadeLinkage;
    return defaultValueRule.defaultValue || isSingle({ fieldType: defaultValueRule.cascadeFieldType }) ? defaultValueRule.defaultIds?.length && defaultValueRule.defaultIds[0] : defaultValueRule.defaultIds;
  }
  return undefined;
}

function getRuleHidden(field?: Pick<IssueCreateFields, 'fieldType' | 'fieldCode' | 'defaultValueObj' | 'defaultValueObjs' | 'required'>, rules: ICascadeLinkage[] = []) {
  if (field) {
    if (field.required || find(rules, { required: true }) || every(rules, (rule) => !rule.hidden)) {
      return false;
    }
    return true;
  }
  return true;
}

function getRuleRequired(rules: ICascadeLinkage[] = []) {
  return !!find(rules, { required: true });
}

function getOptionsData(rules: ICascadeLinkage[] = [], dataSet: DataSet, field: Pick<IssueCreateFields, 'fieldType' | 'fieldCode' | 'defaultValueObj' | 'defaultValueObjs' | 'required'>) {
  const ruleIds = rules.length ? map(rules, 'id') : undefined;
  return ({
    ruleIds,
    // eslint-disable-next-line no-nested-ternary
    selected: ruleIds?.length ? (isMultiple(field) ? dataSet.current?.get(field.fieldCode) : [dataSet.current?.get(field.fieldCode)]) : undefined,
  });
}
function transformSubmitFieldValue(field: IssueCreateFields, value: any) {
  switch (field.fieldType) {
    case 'time':
    case 'date':
    case 'datetime': {
      return value && moment.isMoment(value) ? value.format('YYYY-MM-DD HH:mm:ss') : value;
    }
    default: return value;
  }
}
const CreateIssueBase = observer(({
  modal,
  projectId,
  onSubmit,
  defaultTypeCode = 'story',
  defaultTypeId,
  defaultAssignee,
  defaultFeature,
  parentIssue,
  defaultValues,
  typeCode,
  isProgram,
}: CreateIssueBaseProps) => {
  const [fileList, setFileList] = useState<UploadFile[]>([]);
  const dataSetRef = useRef(defaultDataSet);
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
  const isSubIssue = issueTypeCode && ['sub_task'].includes(issueTypeCode);

  const [{ data: fields, isFetching: isFieldsLoading }, {
    data: templateData,
  }, { data: cascadeRuleList = [] }] = useIssueCreateFields({ issueTypeId, projectId });

  const hasValue = usePersistFn((field: IssueCreateFields) => (isMultiple(field) ? dataSet.current?.get(field.fieldCode)?.length : dataSet.current?.get(field.fieldCode)));
  const fieldValueArr = usePersistFn((field: IssueCreateFields) => {
    let value = castArray(toJS(dataSet.current?.get(field.fieldCode)));
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
      if (((field.system && includes(pageCascadeFields, field.fieldCode)) || (!field.system && isSelect(field))) && hasValue(field)) {
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
    }
    if (field.defaultValue === '') {
      return undefined;
    }
    return field.defaultValue;
  });

  const handleUpdate = usePersistFn(({ name, value, record }) => {
    switch (name) {
      case 'issueType': {
        break;
      }
      default: {
        break;
      }
    }
  });

  useEffect(() => {
    const oldDataSet = dataSetRef.current;
    const newDataSet = new DataSet({
      autoCreate: false,
      fields: fields ? insertField([...fields.map((field) => {
        const preset = presets.get(field.fieldCode) ?? {};
        return merge(preset, {
          name: field.fieldCode,
          fieldId: field.fieldId,
          fieldType: field.fieldType,
          fieldCode: field.fieldCode,
          label: field.fieldName,
          required: field.required || getRuleRequired(rules),
          multiple: isMultiple(field),
        });
      })], [{
        insert: !!isSubIssue,
        after: 'issueType',
        field: {
          name: 'parentIssueId',
          label: '关联父级任务',
          required: true,
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
        // update: handleUpdate,
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
      if (newValue[name] === null || newValue[name] === undefined) {
        newValue[name] = value;
      }
    };

    // 设置默认值
    fields?.forEach((field) => {
      const defaultValue = getDefaultValue(field);
      if (defaultValue !== null && defaultValue !== undefined) {
        // 没有值的时候再设置
        setValue(field.fieldCode, defaultValue);
      }
    });
    // TODO: 将各种默认值的获取和设置逻辑合并
    // 设置描述默认值
    if (templateData && templateData.template) {
      setValue('description', templateData.template);
    }
    if (parentIssue) {
      setValue('parentIssueId', parentIssue.issueId);
    }
    if (showFeature && defaultFeature) {
      setValue('feature', defaultFeature.issueId);
    }
    // 创建一个新的
    newDataSet.create(newValue);
    setDataSet(newDataSet);
  }, [defaultFeature, fields, getDefaultValue, handleUpdate, isShowFeature, isSubIssue, issueTypeCode, parentIssue, rules, showFeature, templateData]);
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
    if (await dataSet.validate()) {
      if (enableIssueLinks && !await issueLinkDataSet.validate()) {
        return false;
      }
      const data = dataSet.current?.toData();
      const customFields = fields?.filter((f) => !f.system);
      const systemFields = fields?.filter((f) => f.system);
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
        parentIssueId: data.parentIssueId,
        programId: projectId ?? getProjectId(),
        projectId: projectId ?? getProjectId(),
        featureId: data.feature,
        issueLinkCreateVOList: enableIssueLinks ? getIssueLinks() : undefined,
        componentIssueRelVOList: data.component ? data.component.map((id: string) => ({ componentId: id })) : [],
      });

      values = hooks.reduce((result, hook) => hook(result, data), values);
      await onSubmit({
        data: values, fieldList, fileList,
      });
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
    console.log(rules, getOptionsData(rules, dataSet, field));
    switch (field.fieldCode) {
      case 'parentIssueId': {
        return {
          issueType: issueTypeCode,
          hidden: !!parentIssue,
        };
      }
      case 'assignee': {
        return {
          extraOptions: defaultAssignee,
          onAssigneeMe: (userInfo: User) => {
            setFieldValue('assignee', userInfo.id);
          },
          hidden: getRuleHidden(field, rules),
        };
      }
      case 'reporter':
      case 'mainResponsible':
      case 'environment':
      case 'component':
      case 'fixVersion':
      case 'influenceVersion':
      case 'estimatedStartTime':
      case 'estimatedEndTime':
      case 'benfitHypothesis':
      case 'acceptanceCritera':
      case 'subProject': {
        return {
          hidden: getRuleHidden(field, rules),
        };
      }
      case 'priority': {
        return {
          afterLoad: (priorities: Priority[]) => {
            const defaultPriority = find(priorities, { default: true });
            if (defaultPriority) {
              dataSet.current?.set('priority', defaultPriority.id);
            }
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
          },
        };
      }
      case 'feature': {
        return defaultFeature ? {
          featureId: defaultFeature.issueId,
          featureName: defaultFeature.summary,
        } : {};
      }
      default: break;
    }
    switch (field.fieldType) {
      case 'member': {
        return {
          extraOptions: field.defaultValueObj,
          hidden: getRuleHidden(field, rules),
        };
      }
      case 'multiMember': {
        return {
          extraOptions: field.defaultValueObjs,
          hidden: getRuleHidden(field, rules),
        };
      }
      default: return {
        hidden: getRuleHidden(field, rules),
      };
    }
  });
  const renderFields = usePersistFn(() => (
    <Row gutter={24}>
      {[...dataSet.fields.values()].filter((f) => f.get('display') !== false).map((dataSetField) => {
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
          ? (
            <Col
              key={name}
              style={{ marginBottom: 24 }}
              span={lineField.includes(dataSetField.name) ? 24 : 12}
            >
              {config.renderFormItem({
                name,
                fieldId,
                projectId,
                clearButton: !field?.required,
                multiple: dataSetField.get('multiple'),
                style: {
                  width: '100%',
                },
                ...extraProps,
              })}
            </Col>
          )
          : null;
      })}
    </Row>
  ));
  const issueLinkDataSet = useMemo(() => new DataSet({
    // autoCreate: true,
    fields: [{
      name: 'linkType',
      label: '关系',
      required: true,
    }, {
      name: 'issueIds',
      label: '问题',
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
      >
        {parentIssue ? <TextField label="父任务概要" value={parentIssue.summary} disabled /> : null}
        {renderFields()}
        <UploadButton
          fileList={fileList}
          onChange={({ fileList: files }) => {
            if (validateFile(files)) {
              setFileList(files);
            }
          }}
        />
        {issueTypeCode === 'feature' ? <WSJF /> : null}
      </Form>
      {enableIssueLinks ? <IssueLink projectId={projectId} dataSet={issueLinkDataSet} /> : null}
    </Spin>
  );
});

export default CreateIssueBase;
