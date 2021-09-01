import {
  DataSet,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';

import { find, get } from 'lodash';
import type { IAgileBaseFieldTypeComponentProps, AgileComponentMapProps, CustomFCComponentMapProps } from '../base/component';
import { getAgileFields } from '../base';
import { IChosenFieldField } from '@/components/chose-field/types';
import { getFieldPropsByMode } from '../base/utils';
import { IFieldSystemConfig, IFieldCustomConfig } from '../base/type';

function getSystemFieldConfig({ code, value, defaultValue }: IChosenFieldField, dataSet?: DataSet): Partial<IFieldSystemConfig<AgileComponentMapProps>> {
  switch (code) {
    case 'sprint':
    case 'sprintList':
      return {
        code: 'sprint',
        props: {
          statusList: [], isProgram: code === 'sprintList', hasUnassign: true, selectSprints: value,
        },
      };
    case 'statusId':
    case 'statusList':
      return {
        code: 'status',
        props: {
          noIssueTypeIdQuery: true,
          issueTypeIds: (dataSet?.current?.get('issueTypeList') ?? dataSet?.current?.get('issueTypeId')) ?? undefined,
          selectedIds: dataSet?.current?.get('statusList') ?? dataSet?.current?.get('statusId'),
        },
      };
    case 'issueTypeId':
    case 'issueTypeList':
      return {
        code: 'issueType',
        props: {
          isProgram: code === 'issueTypeList',
          filterList: code === 'issueTypeList' ? [] : undefined,
        },

      };
    case 'epic':
    case 'epicList':
      return {
        code: 'epic',
        props: {
          onlyUnCompleted: false,
          isProgram: code === 'epicList',
          unassignedEpic: true,
          defaultSelectedIds: value,
        },
      };

    case 'label':
      return {
        code: 'label',
        props: {
          valueField: 'labelId',

        },
      };
    case 'component':
      return {
        props: {
          valueField: 'componentId',
        },
      };
    case 'version':
    case 'fixVersion':
    case 'influenceVersion':
      return {
        code: 'version',
        props: {
          valueField: 'versionId',
          hasUnassign: true,
        },
      };
    case 'feature': {
      return {
        code: 'feature',
        props: {
          featureIds: defaultValue,
        },
      };
    }
    case 'teamProjectList': {
      return {
        code: 'subProject',

      };
    }
    case 'piList': {
      return {
        code: 'pi',
        props: {
          afterLoad: (piList: any[]) => {
            if (!dataSet?.current?.getState(`init_${code}`) && !defaultValue && Array.isArray(piList) && piList.length > 0) {
              const data = find(piList, { statusCode: 'doing' }) ?? piList[0];
              dataSet?.current?.set(code, [data.id]);
              dataSet?.current?.setState(`init_${code}`, true);
            }
          },

        },

      };
    }
    case 'quickFilterIds':
    case 'myStarBeacon':
    case 'myAssigned':
    {
      return {
        code: 'quickFilter',
        fieldType: 'multiple',
        props: {
          disabledRequest: code === 'myStarBeacon' || code === 'myAssigned',

        },
      };
    }
    case 'tags': {
      return { code: 'tag' };
    }

    default:
      break;
  }
  return {};
}

/**
 * 获取自定义字段配置
 * @param field
 * @param dataSet
 * @returns
 */
function getCustomFieldConfig(field: IChosenFieldField, dataSet?: DataSet): Partial<IFieldCustomConfig<CustomFCComponentMapProps>> {
  const {
    code, fieldType, name, fieldOptions, value, id,
  } = field;
  const defaultValue = toJS(value);

  switch (fieldType) {
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return {
        props: {
          fieldId: field.id, selected: defaultValue ?? (field as any).valueBindValue, onlyEnabled: false,
        } as IAgileBaseFieldTypeComponentProps['multiple'],
      } as any; // valueBindValue 是快速筛选处的值 TODO 后续去掉
    case 'member':
    case 'multiMember':
      return {
        props: {
          selected: defaultValue ? defaultValue.map((item: any) => String(item)) : undefined,
          extraOptions: code === 'assigneeId' ? [{ id: '0', realName: '未分配' }] : undefined,
        } as IAgileBaseFieldTypeComponentProps['multiMember'],
      } as any;
    default:
      break;
  }
  return {};
}
function getFieldConfig(field: IChosenFieldField, dataSet?: DataSet) {
  const {
    value, id,
  } = field;
  const defaultValue = toJS(value);
  if (!id) {
    return getSystemFieldConfig({ ...field, defaultValue }, dataSet);
  }
  return getCustomFieldConfig({ ...field, defaultValue }, dataSet);
}
/**
 *  获取过滤的字段
 * @param fields
 * @param fieldCodeProps  兼容属性，key对应的是 `fields`中的code
 * @param instance 获取字段实例
 */
function getFilterFields(fields: any[], fieldCodeProps?: Record<string, any>, instance = getAgileFields) {
  const newFilters = fields.map(({ field, dataSet, otherComponentProps }) => {
    const config = getFieldConfig(field, dataSet);
    const { name, code, fieldType } = field;

    const { props, ...otherConfigProps } = config;
    return {
      code,
      fieldType,
      ...otherConfigProps,
      props: {
        name: code,
        label: name,
        style: { width: '100%' },
        key: code,
        ...getFieldPropsByMode({ code, fieldType, outputs: ['config', 'function'] }, 'filter'),
        ...props,
        ...otherComponentProps,
      },
      outputs: ['config', 'function'],
    };
  }) as any[];
  // const { system, custom } = groupBy(newFilters, (item) => (item.system ? 'system' : 'custom'));
  return instance(newFilters).map((i: [any, any]) => i[1]({ ...i[0], props: { ...i[0].props, ...get(fieldCodeProps, i[0].props.key) } }));
  // return getFields(newFilters).map((i, index) => React.createElement(i[0] as any,
  //   { ...getProps(fields[index].code, fieldCodeProps) })) as JSX.Element[];
}

export default getFilterFields;
