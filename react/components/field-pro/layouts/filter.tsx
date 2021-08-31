import {
  DataSet,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { find, get, groupBy } from 'lodash';
import { getAgileFields } from '../base';
import { IChosenFieldField } from '@/components/chose-field/types';
import { statusApi } from '@/api';
import { getFieldPropsByMode } from '../base/utils';

function renderField(field: IChosenFieldField, dataSet?: DataSet) {
  const {
    code, fieldType, name, fieldOptions, value, id,
  } = field;
  const defaultValue = toJS(value);
  if (!id) {
    switch (code) {
      case 'sprint':
      case 'sprintList':
        return {
          code: 'sprint', fieldType, statusList: [], isProgram: code === 'sprintList', hasUnassign: true, selectSprints: value,
        };
      case 'statusId':
      case 'statusList':
        return {
          code: 'status',
          fieldType,
          noIssueTypeIdQuery: true,
          issueTypeIds: (dataSet?.current?.get('issueTypeList') ?? dataSet?.current?.get('issueTypeId')) ?? undefined,
          selectedIds: dataSet?.current?.get('statusList') ?? dataSet?.current?.get('statusId'),
        };
      case 'issueTypeId':
      case 'issueTypeList':
        return {
          code: 'issueType',
          isProgram: code === 'issueTypeList',
          filterList: code === 'issueTypeList' ? [] : undefined,

        };
      case 'epic':
      case 'epicList':
        return {
          code: 'epic',
          onlyUnCompleted: false,
          isProgram: code === 'epicList',
          unassignedEpic: true,
          defaultSelectedIds: value,
        };
      case 'priorityId':
        return {
          code: 'priority',

        };
      case 'label':
        return {
          code: 'label',

          valueField: 'labelId',
        };
      case 'component':
      case 'version':
      case 'fixVersion':
      case 'influenceVersion':
        return {
          code: 'version',

          valueField: 'versionId',
          hasUnassign: true,
        };
      case 'feature': {
        return {
          code: 'feature',

          featureIds: defaultValue,
        };
      }
      case 'teamProjectList': {
        return {
          code: 'subProject',
          name: 'teamProjectList',

        };
      }
      case 'piList': {
        return {
          code: 'pi',

          afterLoad: (piList: any[]) => {
            if (!dataSet?.current?.getState(`init_${code}`) && !defaultValue && Array.isArray(piList) && piList.length > 0) {
              const data = find(piList, { statusCode: 'doing' }) ?? piList[0];
              dataSet?.current?.set(field.code, [data.id]);
              dataSet?.current?.setState(`init_${code}`, true);
            }
          },

        };
      }
      case 'quickFilterIds':
      case 'myStarBeacon': {
        return {
          code: 'quickFilter',

          disabledRequest: code === 'myStarBeacon',
        };
      }
      case 'myAssigned': {
        return {
          code: 'quickFilter',

          disabledRequest: code === 'myAssigned',
        };
      }
      // case 'starBeacon': {
      //   return <CheckBox label="我的关注" name={code} />;
      // }

      case 'tags': {
        return { code: 'tag', multiple: true };
      }

      default:
        break;
    }
  }
  switch (fieldType) {
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return { fieldId: field.id, selected: defaultValue ?? (field as any).valueBindValue, onlyEnabled: false }; // valueBindValue 是快速筛选处的值 TODO 后续去掉
    case 'multiMember':
    case 'member':
    {
      return { selected: defaultValue ? defaultValue.map((item: any) => String(item)) : undefined, extraOptions: code === 'assigneeId' ? [{ id: '0', realName: '未分配' }] : undefined };
    }

    default:
      break;
  }
  return {};
}
/**
 *  获取过滤的字段
 * @param fields
 * @param fieldCodeProps  兼容属性，key对应的是 `fields`中的code
 * @param instance 获取字段实例
 */
function getFilterFields(fields: any[], fieldCodeProps?: Record<string, any>, instance = getAgileFields) {
  const newFilters = fields.map((field) => {
    const config = renderField(field.field, field.dataSet);
    const { code = field.field.code, name = field.field.code, ...otherProps } = config;

    return {
      code,
      fieldType: field.field.fieldType,
      props: {
        name,
        label: field.field.name,
        style: { width: '100%' },
        key: field.field.code,
        ...getFieldPropsByMode({ code, fieldType: field.field.fieldType, outputs: ['element'] }, 'filter'),
        ...otherProps,
        ...field.otherComponentProps,
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
