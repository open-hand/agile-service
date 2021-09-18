import type {
  DataSet,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';

import { find, get, merge } from 'lodash';
import type { IAgileBaseFieldTypeComponentProps, AgileComponentMapProps } from '../base/component';
import { getAgileFields } from '../base';
import type { IChosenFieldField } from '@/components/chose-field/types';
import { getComponentCodeForLocalCode, getFieldPropsByMode } from '../base/utils';
import { IFieldSystemConfig, IFieldCustomConfig } from '../base/type';

function getSystemFieldConfig(field: IChosenFieldField, dataSet?: DataSet): Partial<IFieldSystemConfig<AgileComponentMapProps>> {
  const { code, value, defaultValue } = field;
  switch (code) {
    case 'sprintList':
      return {
        code: 'sprint',
        props: {
          selectSprints: defaultValue,
          isProgram: true,
        },
      };
    case 'statusId':
    case 'statusList':
      return {
        code: 'status',
        props: {
          isProgram: code === 'statusList',
          issueTypeIds: (dataSet?.current?.get('issueTypeList') ?? dataSet?.current?.get('issueTypeId')) ?? undefined,
        },
      };
    case 'issueTypeList':
      return {
        code: 'issueType',
        props: {
          isProgram: true,
          filterList: [],
        },
      };
    case 'epic':
    case 'epicList': {
      return {
        code: 'epic',
        props: {
          defaultSelectedIds: defaultValue,
          isProgram: code === 'epicList',
        },
      };
    }
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
          addPi0: true,
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
          commonOptions: code === 'myStarBeacon' ? (options) => options.filter((i) => i.value === 'myStarBeacon') : undefined,
          disabledRequest: code === 'myStarBeacon' || code === 'myAssigned',

        },
      };
    }

    default:
      break;
  }
  return getCustomFieldConfig(field, dataSet) as any;
}

/**
 * 获取自定义字段配置
 * @param field
 * @param dataSet
 * @returns
 */
function getCustomFieldConfig(field: IChosenFieldField, dataSet?: DataSet): Partial<IFieldCustomConfig<IAgileBaseFieldTypeComponentProps>> {
  const {
    code, fieldType, name, fieldOptions, value, id, defaultValue,
  } = field;

  switch (fieldType) {
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return {
        props: {
          fieldId: field.id, selected: defaultValue ?? (field as any).valueBindValue, onlyEnabled: false, disabledRuleConfig: true,
        },
      }; // valueBindValue 是快速筛选处的值 TODO 后续去掉
    case 'member':
    case 'multiMember':
      return {
        props: {
          selected: defaultValue ? defaultValue.map((item: any) => String(item)) : undefined,
          extraOptions: code === 'assigneeId' ? [{ id: '0', realName: '未分配' }] : undefined,
        },
      };
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
 * @param fieldCodeProps  组件的props， key对应的是 `fields`中的code
 * @param instance 获取字段实例
 */
function getFilterFields(fields: any[], fieldCodeProps?: Record<string, any>, instance = getAgileFields) {
  const newFilters = fields.map(({ field, dataSet, otherComponentProps }) => {
    const { name, code, fieldType } = field;
    const config = merge({
      code: getComponentCodeForLocalCode(field.code),
      fieldType,
      props: {
        name: code,
        label: name,
        style: { width: '100%' },
        key: code,
        ...otherComponentProps,
      },
      outputs: ['config', 'function'],
    }, getFieldConfig(field, dataSet));
    return merge(config, { props: getFieldPropsByMode(config, 'filter') });
  }) as any[];
  // const { system, custom } = groupBy(newFilters, (item) => (item.system ? 'system' : 'custom'));
  return instance(newFilters).map((i: [any, any]) => i[1]({ ...i[0], props: { ...i[0].props, ...get(fieldCodeProps, i[0].props.key) } }));
  // return getFields(newFilters).map((i, index) => React.createElement(i[0] as any,
  //   { ...getProps(fields[index].code, fieldCodeProps) })) as JSX.Element[];
}

export default getFilterFields;
