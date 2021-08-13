import {
  DataSet,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { find } from 'lodash';
import getFields from '../base';
import { IChosenFieldField } from '@/components/chose-field/types';

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
          code: 'sprint', name: code, fieldType, statusList: [], isProgram: code === 'sprintList', multiple: true, hasUnassign: true, selectSprints: value,
        };
      case 'statusId':
      case 'statusList':
        return {
          code: 'status',
          name: code,
          fieldType,
          isProgram: code === 'statusList',
          multiple: true,
          issueTypeIds: (dataSet?.current?.get('issueTypeList') ?? dataSet?.current?.get('issueTypeId')) ?? undefined,
          selectedIds: dataSet?.current?.get('statusList') ?? dataSet?.current?.get('statusId'),
        };
      case 'issueTypeId':
      case 'issueTypeList':
        return {
          code: 'issueType',
          name: code,
          isProgram: code === 'issueTypeList',
          filterList: code === 'issueTypeList' ? [] : undefined,
          multiple: true,
        };
      case 'epic':
      case 'epicList':
        return {
          code: 'epic',
          name: code,
          isProgram: code === 'epicList',
          unassignedEpic: true,
          multiple: true,
        };
      case 'priorityId':
        return {
          code: 'priority',
          name: code,
          multiple: true,
        };
      case 'label':
        return {
          code: 'label',
          name: code,
          multiple: true,
          valueField: 'labelId',
        };
      case 'component':
      case 'version':
      case 'fixVersion':
      case 'influenceVersion':
        return {
          code: 'version',
          name: code,
          valueField: 'versionId',
          hasUnassign: true,
        };
      case 'feature': {
        return {
          code: 'feature',
          name: code,
          multiple: true,
          featureIds: defaultValue,
        };
      }
      case 'teamProjectList': {
        return {
          code: 'subProject',
          name: 'teamProjectList',
          multiple: true,
        };
      }
      case 'piList': {
        return {
          code: 'pi',
          name: code,
          multiple: true,
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
          name: code,
          disabledRequest: code === 'myStarBeacon',
        };
      }
      case 'myAssigned': {
        return {
          code: 'quickFilter',
          name: code,
          disabledRequest: code === 'myAssigned',
        };
      }
      // case 'starBeacon': {
      //   return <CheckBox label="我的关注" name={code} />;
      // }
      case 'environment': {
        return { code: 'environment', name: code, multiple: true };
      }
      case 'programVersion': {
        return { code: 'programVersion', name: code, multiple: true };
      }
      case 'tags': {
        return { code: 'tag', name: code, multiple: true };
      }
      // case 'contents': {
      //   return <TextField name={code} clearButton {...otherComponentProps} />;
      // }
      default:
        break;
    }
  }
  switch (fieldType) {
    case 'input':
      return { maxLength: 100, valueChangeAction: 'input' as any };

    case 'text':
      return { rows: 3, maxLength: 255, valueChangeAction: 'input' as any };

    case 'radio': case 'single': case 'checkbox': case 'multiple':
      // @ts-ignore
      return { fieldId: field.id, selected: defaultValue ?? field.valueBindValue, onlyEnabled: false }; // valueBindValue 是快速筛选处的值 TODO 后续去掉

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
 * @param fieldCodeProps IFieldConfig<AgileComponentMapProps, CustomComponentMapProps>[]
 */
function getFilterFields(fields: any[], fieldCodeProps?: Record<string, any>) {
  const newFilters = fields.map((field) => {
    const config = renderField(field.field, field.dataSet);
    const { code = field.field.code, name = field.field.code, ...otherProps } = config;
    return {
      code,
      fieldType: field.field.fieldType,
      props: {
        name,
        label: field.field.name,
        multiple: true,
        style: { width: '100%' },
        key: field.field.code,
        ...otherProps,
        ...field.otherComponentProps,
      },
      outputs: ['element'],
    };
  }) as any[];
  return getFields(newFilters).map((i) => i[0]);
  // return getFields(newFilters).map((i, index) => React.createElement(i[0] as any,
  //   { ...getProps(fields[index].code, fieldCodeProps) })) as JSX.Element[];
}
export default getFilterFields;
