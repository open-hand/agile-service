import { merge } from 'lodash';
import {
  IComponentFCWithClassObject, IFieldConfig, IFieldProcessConfig, IComponentFCWithClassObjectProps,
} from './type';
import type { CustomComponentMapProps, IAgileBaseComponentPartialProps, IAgileBaseFieldTypeComponentProps } from './component';
import { AgileComponentMap } from './component';
import { userApi } from '@/api';

const filterModeSelectCommonProps = {
  multiple: true,
  maxTagCount: 3,
  maxTagTextLength: 10,
  dropdownMatchSelectWidth: false,
  clearButton: true,
};
const filterModeProps = {
  input: {},
  time: {},
  datetime: {},
  date: {},
  number: {},
  text: {},
  url: {},
  radio: filterModeSelectCommonProps,
  single: filterModeSelectCommonProps,
  checkbox: filterModeSelectCommonProps,
  multiple: filterModeSelectCommonProps,
  multiMember: filterModeSelectCommonProps,
  member: filterModeSelectCommonProps,
};
type IAgileSystemFieldProps = IAgileBaseComponentPartialProps & {
  assignee?: IAgileBaseFieldTypeComponentProps['member'],
  reporter?: IAgileBaseFieldTypeComponentProps['member'],
}
type IComponentPropsToFn<T> = { [P in keyof T]: (fieldConfig: IFieldConfig<any, any>) => T[P] }
/**
 * 筛选模式通用的props （根据code匹配)
 */
const filterCodeModeProps: IComponentPropsToFn<IAgileSystemFieldProps> = {
  assignee: ({ props }) => ({
    extraOptions: [{ id: '0', realName: '未分配' }],
    selected: props?.value,
    request: ({ filter, page, requestArgs }: any) => userApi.project(props?.projectId).getAllInProjectIncludesLeaveUsers(filter, page, requestArgs?.selectedUserIds),
  }),
  sprint: ({ props }) => ({
    statusList: [],
    hasUnassign: true,
  }),
  issueType: ({ props }) => ({
    applyType: props?.applyType,
    projectId: props?.projectId,
  }),
  status: ({ props }) => ({
    applyType: props?.applyType,
    projectId: props?.projectId,
    selectedIds: props?.value,
    noIssueTypeIdQuery: true,
  }),
  influenceVersion: () => ({
    hasUnassign: true,
    valueField: 'versionId',
  }),
  fixVersion: () => ({
    hasUnassign: true,
    valueField: 'versionId',
  }),
  version: () => ({
    hasUnassign: true,
    valueField: 'versionId',
  }),
  epic: ({ props }) => ({
    unassignedEpic: true,
    onlyUnCompleted: false,
    selectIds: props?.value,
  }),
  label: () => ({
    placeholder: '标签',
  }),
};

/**
 * 获取处理后的配置
 * 最终确定字段配置
 */
function getProcessFieldConfig<T extends IComponentFCWithClassObject, C extends IComponentFCWithClassObject = CustomComponentMapProps>(fieldConfig: IFieldConfig<T, C>) {
  const config = {
    code: fieldConfig.code,
    fieldType: fieldConfig.fieldType,
    outputs: fieldConfig.outputs,
    display: fieldConfig.display === undefined || !!fieldConfig.display,
    props: {
      clearButton: true, name: fieldConfig.code, disabled: fieldConfig.archive, ...fieldConfig.props,
    },
  };
  const filedTypeConfigObj = {
    time: {},
    datetime: {},
    date: {},
    number: {},
    input: { props: { maxLength: 100, valueChangeAction: 'input' } },
    text: {
      props: {
        autoSize: true, rows: 3, maxLength: 255,
      },
    },
    url: {},
    radio: {},
    single: {},
    checkbox: { props: { multiple: true } },
    multiple: { props: { multiple: true } },
    multiMember: { props: { multiple: true } },
    member: {},
  };
  return merge(config.fieldType ? filedTypeConfigObj[config.fieldType] : {}, config) as unknown as IFieldProcessConfig<T, CustomComponentMapProps>;
}
/**
 * code 是否在系统组件内
 * @param code
 * @param components
 * @returns
 */
function isCodeInSystemComponents(code: string, components: { [x: string]: any } = AgileComponentMap) {
  return Object.keys(filterCodeModeProps).includes(code) || Object.keys(components).includes(code);
}
/**
 * 通过模式获取字段 `props` 配置
 * @param fieldConfig
 * @param props 外部传入的props
 * @param mode `filter` 待添加`flatFilter` `create` `edit`
 * @returns
 */
function getFieldPropsByMode(fieldConfig: IFieldConfig<any, any>, mode: 'filter' | string = 'filter') {
  switch (mode) {
    case 'filter': {
      const codeConfigFn = filterCodeModeProps[fieldConfig.code as keyof typeof filterCodeModeProps] || ((...args: any) => ({}));
      if (fieldConfig.fieldType) {
        return { ...filterModeProps[fieldConfig.fieldType], ...codeConfigFn(fieldConfig) };
      }
      return codeConfigFn(fieldConfig) as any;
    }
  }
  return {};
}

const IssueStoreSystemMapComponentCode = {
  issueTypeId: 'issueType',
  statusId: 'status',
  priorityId: 'priority',
  tags: 'tag',
  assigneeId: 'assignee',
  reporterIds: 'reporter',
};
/**
 * 对于本地前端写的code转换为组件code （针对IssueStore的)
 * @param code
 */
function getComponentCodeForLocalCode(code: string) {
  return IssueStoreSystemMapComponentCode[code as keyof typeof IssueStoreSystemMapComponentCode] || code;
}
// export type IFieldProcessConfigFn=ReturnType<(<T extends IFiledMapProps, K extends keyof T>(fieldConfig: IFieldConfig<T, K>) =>typeof getProcessFieldConfig)>
export {
  getProcessFieldConfig, isCodeInSystemComponents, getFieldPropsByMode, getComponentCodeForLocalCode,
};
