import React from 'react';

import moment from 'moment';
import {
  merge, omit,
} from 'lodash';
import UserTag from '@/components/tag/user-tag';
import SelectCustomFieldBox from '@/components/select/select-custom-field-box';
import SelectFeature from '@/components/select/select-feature';
import SelectNumber from '@/components/select/select-number';
import SelectParentIssue from '@/components/select/select-parent-issue';
import SelectFeatureType from '@/components/select/select-feature-type';
import SelectTeam from '@/components/select/select-team';
import SelectUserWithAssigneeMe from '@/components/select/pro/select-user-with-assigneeme';
import SelectIssueType from '@/components/select/pro/select-issue-type';
import getFieldsInstance, { AgileComponentMap, CustomFieldMap, IFieldBaseConfig } from '../base';

type ProRenderFieldPropsType = {
  render?: ((
    text: any,
    props: any,
    dom: JSX.Element,
  ) => JSX.Element)
  renderFormItem?: ((props: any) => JSX.Element)
  props?: any
  /** 创建时提交至服务端的值和field的code的对应 */
  valueKey?: string
};
const valueTypeConfigMap: Record<string, ProRenderFieldPropsType> = {
  default: {
    render: (text: any,
      props: any,
      dom: JSX.Element) => text,
  },
  member: {
    render: (text) => <UserTag data={text} />,
  },
  multiMember: {
    render: (text) => <UserTag data={text} />,
  },
  multiple: {
    render: (text) => (Array.isArray(text) ? text.join('、') : text),
  },
};
const systemFieldConfigMap: Record<string, ProRenderFieldPropsType> = {
  issueType: {
    props: { showIcon: true },
    valueKey: 'issueTypeId',
  },
  sprint: {
    valueKey: 'sprintId',
  },
  estimatedEndTime: {
    render: (text) => text,
    props: {
      defaultPickerValue: moment().endOf('d'),
    },
  },
  component: {

    valueKey: 'componentIssueRelVOList',
  },
  label: {
    render: (text) => text,
    props: {
      combo: true,
      placeholder: undefined,
    },
    valueKey: 'labelIssueRelVOList',
  },
  epic: {
    valueKey: 'epicId',
  },
  assignee: {
    ...valueTypeConfigMap.member,
    valueKey: 'assigneeId',
  },
  reporter: {
    ...valueTypeConfigMap.member,
    valueKey: 'reporterId',
  },
  feature: {
    render: (text) => text,

    valueKey: 'featureId',
  },
  fixVersion: {
    render: (text) => text,
    props: {
      valueField: 'versionId',
      statusArr: ['version_planning'],
    },
  },
  mainResponsible: {
    ...valueTypeConfigMap.member,
    valueKey: 'mainResponsibleId',
  },
  storyPoints: {
    render: (text) => text,
  },
  priority: {
    render: (text) => text,

    valueKey: 'priorityId',
  },
  status: {
    valueKey: 'statusId',
  },
  subProject: {

    valueKey: 'teamProjectIds',
  },
  pi: {
    props: {
      openPermission: true,
      statusList: ['todo', 'doing'],
    },
    valueKey: 'piId',
  },

};

const fieldMap = {
  ...valueTypeConfigMap,
  ...systemFieldConfigMap,
};
const AgileComponentMapWithPro = {
  ...AgileComponentMap,
  issueType: SelectIssueType,
  assignee: SelectUserWithAssigneeMe,
  feature: SelectFeature,
  storyPoints: SelectNumber,
  remainingTime: SelectNumber,
  parentIssueId: SelectParentIssue,
  featureType: SelectFeatureType,
  subProject: SelectTeam,
};
const CreateCustomFieldMap = {
  ...CustomFieldMap,
  radio: SelectCustomFieldBox,
  checkbox: SelectCustomFieldBox,

};
/** 创建问题字段实例 */
const getCreateFields = getFieldsInstance<IFieldBaseConfig, typeof AgileComponentMapWithPro>({ SystemComponents: AgileComponentMapWithPro, CustomComponents: CreateCustomFieldMap });

// export { fieldMap };
/**
 * 获取创建的单字段配置
 * @param field
 */
const getCreateFieldConfig = (field: { fieldCode: string, fieldType: string }): Required<Pick<ProRenderFieldPropsType, 'render' | 'renderFormItem'>> & Pick<ProRenderFieldPropsType, 'valueKey'> => {
  const config = fieldMap[field.fieldCode as keyof typeof fieldMap] ?? fieldMap[field.fieldType as keyof typeof fieldMap];

  const createConfig = getCreateFields([{
    code: field.fieldCode,
    fieldType: field.fieldType as any,
    props: config?.props,
    outputs: ['config', 'function'],
  }]);
  const [elementConfig, fn] = createConfig[0] as [any, any];

  return {
    ...valueTypeConfigMap.default as any,
    ...omit(config || {}, 'props'),
    renderFormItem: (props: any) => fn(merge(elementConfig, { props })),
  };
};
export default getCreateFieldConfig;
