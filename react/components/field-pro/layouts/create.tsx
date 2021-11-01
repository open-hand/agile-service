import React from 'react';

import moment from 'moment';
import 'moment/locale/zh-cn';
import 'moment/locale/en-nz';
import {
  merge, omit,
} from 'lodash';
import { stores } from '@choerodon/boot';
import UserTag from '@/components/tag/user-tag';
import SelectCustomFieldBox from '@/components/select/select-custom-field-box';
import SelectFeature from '@/components/select/select-feature';
import SelectNumber from '@/components/select/select-number';
import SelectParentIssue from '@/components/select/select-parent-issue';
import SelectFeatureType from '@/components/select/select-feature-type';
import SelectTeam from '@/components/select/select-team';
import SelectUserWithAssigneeMe from '@/components/select/pro/select-user-with-assigneeme';
import SelectIssueType from '@/components/select/pro/select-issue-type';
import DateTimePickerWithDefault from '@/components/date-time-picker';
import type { DateTimePickerProps } from '@/components/date-time-picker/DateTimePicker';
import getFieldsInstance, { AgileComponentMap, CustomFieldMap, IFieldBaseConfig } from '../base';
import type { IAgileBaseFieldTypeComponentProps } from '../base/component';
import type {
  IComponentFCWithClassObject, IClassComponentType, IComponentFCWithClassObjectProps, IComponentFCWithClass,
} from '../base/type';
import SelectUser from '@/components/select/select-user';

const { AppState } = stores;

if (AppState.currentLanguage === 'zh_CN') {
  moment.locale('zh-cn');
}

type ProRenderFieldPropsType<P = any> = {
  render?: ((
    text: any,
    props: any,
    dom: JSX.Element,
  ) => JSX.Element)
  renderFormItem?: ((props: any) => JSX.Element)
  props?: P
  /** 创建时提交至服务端的值和field的code的对应 */
  valueKey?: string
};
const AgileComponentMapWithPro = {
  ...AgileComponentMap,
  issueType: SelectIssueType,
  assignee: SelectUserWithAssigneeMe,
  feature: SelectFeature,
  storyPoints: SelectNumber,
  remainingTime: SelectNumber,
  estimateTime: SelectNumber,
  parentIssueId: SelectParentIssue,
  featureType: SelectFeatureType,
  subProject: SelectTeam,
  participant: SelectUser,
  estimatedStartTime: DateTimePickerWithDefault as React.ComponentClass<DateTimePickerProps>,
  estimatedEndTime: DateTimePickerWithDefault as React.ComponentClass<DateTimePickerProps>,
  actualStartTime: DateTimePickerWithDefault as React.ComponentClass<DateTimePickerProps>,
  actualEndTime: DateTimePickerWithDefault as React.ComponentClass<DateTimePickerProps>,
};
const CreateCustomFieldMap = {
  ...CustomFieldMap,
  radio: SelectCustomFieldBox,
  checkbox: SelectCustomFieldBox,

};
export type ICreateComponentPropsDistributeProRender<T extends IComponentFCWithClassObject> = {
  [P in keyof T]?: ProRenderFieldPropsType<T[P] extends IComponentFCWithClass ? React.ComponentProps<T[P]> : T[P]> }
export type ICreateCustomFieldMapProps = IAgileBaseFieldTypeComponentProps & Pick<IComponentFCWithClassObjectProps<typeof CreateCustomFieldMap>, 'radio' | 'checkbox'>

const valueTypeConfigMap: ICreateComponentPropsDistributeProRender<ICreateCustomFieldMapProps & {
  default: ProRenderFieldPropsType
}> = {
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
    // @ts-ignore
    props: {
      menuType: 'project',
    },
  },
  single: {
    // @ts-ignore
    props: {
      menuType: 'project',
    },
  },
  checkbox: {
    // @ts-ignore
    props: {
      menuType: 'project',
    },
  },
  radio: {
    // @ts-ignore
    props: {
      menuType: 'project',
    },
  },

};

const systemFieldConfigMap: ICreateComponentPropsDistributeProRender<typeof AgileComponentMapWithPro & {
  // estimatedEndTime: ICreateCustomFieldMapProps['datetime']
  reporter: ICreateCustomFieldMapProps['member']
  mainResponsible: ICreateCustomFieldMapProps['member']
}> = {
  issueType: {
    props: { showIcon: true, clearButton: false },
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
  actualEndTime: {
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
  influenceVersion: {
    render: (text) => text,
    props: {
      valueField: 'versionId',
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
  participant: {
    valueKey: 'participantIds',
  },
};

const fieldMap = {
  ...systemFieldConfigMap,
};

/** 创建工作项字段实例 */
const getCreateFields = getFieldsInstance<IFieldBaseConfig, typeof AgileComponentMapWithPro>({ SystemComponents: AgileComponentMapWithPro, CustomComponents: CreateCustomFieldMap });

// export { fieldMap };
/**
 * 获取创建的单字段配置
 * @param field
 */
const getCreateFieldConfig = (field: { fieldCode: string, fieldType: string }): Required<Pick<ProRenderFieldPropsType, 'render' | 'renderFormItem'>> & Pick<ProRenderFieldPropsType, 'valueKey'> => {
  const config = fieldMap[field.fieldCode as keyof typeof fieldMap] ?? valueTypeConfigMap[field.fieldType as keyof typeof valueTypeConfigMap];

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
    renderFormItem: (props: any) => fn(merge(elementConfig, { props: { ...props, key: field.fieldCode } })),
  };
};
export default getCreateFieldConfig;
