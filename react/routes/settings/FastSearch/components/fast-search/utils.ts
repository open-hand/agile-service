import { omit, pick, merge } from 'lodash';
// @ts-ignore
import JSONbig from 'json-bigint';
import { IFieldType } from '@/common/types';
import { IChosenFieldField } from '@/components/chose-field/types';
import { IFastSearchEditData } from './types';

const JSONbigString = JSONbig({ storeAsString: true });

type IFieldTypeWithSystemType = IFieldType | 'long' | 'decimal' | 'date'
interface FastSearchAttributeRelationItem {
  name: string
  value: string
  escapeCharacter: string
  excludeCode?: string[]
  excludeType?: string[]
  includeCode?: string[]
  includeType?: string[]

}

const CustomFieldType = {
  radio: 'option',
  checkbox: 'option',
  time: 'date_hms',
  datetime: 'date',
  number: 'number',
  input: 'string',
  text: 'text',
  single: 'option',
  multiple: 'option',
  member: 'option',
  multiMember: 'option',
  date: 'date',
};
/** 优先级excludeCode > excludeType > includeCode > includeType */

const FastSearchAttributeRelations: Array<FastSearchAttributeRelationItem> = [
  {
    name: '等于', value: 'equal', escapeCharacter: '=', excludeType: ['date', 'datetime', 'time'],
  },
  {
    name: '不等于', value: 'notEqual', escapeCharacter: '!=', excludeType: ['date', 'datetime', 'time', 'number', 'decimal'],
  },
  {
    name: '包含', value: 'include', escapeCharacter: 'in', excludeType: ['date', 'datetime', 'time', 'number', 'decimal', 'radio', 'single', 'text', 'input'],
  },
  {
    name: '不包含', value: 'exclude', escapeCharacter: 'not in', excludeType: ['date', 'datetime', 'time', 'number', 'decimal', 'radio', 'single', 'text', 'input'],
  },
  {
    name: '包含', value: 'like', escapeCharacter: 'like', includeType: ['text', 'input'], // 文字类专署
  },
  {
    name: '不包含', value: 'notLike', escapeCharacter: 'notLike', includeType: ['text', 'input'], // 文字类专署
  },
  {
    name: '是', value: 'is', escapeCharacter: 'is', excludeType: ['date', 'datetime', 'time', 'multiMember', 'text', 'input'], excludeCode: ['issue_type', 'priority', 'status'],
  },
  {
    name: '不是', value: 'notIs', escapeCharacter: 'is not', excludeType: ['date', 'datetime', 'time', 'multiMember', 'text', 'input'], excludeCode: ['issue_type', 'priority', 'status'],
  },
  {
    name: '大于', value: 'greater', escapeCharacter: '>', includeType: ['date', 'datetime', 'time', 'number', 'decimal'],
  },
  {
    name: '大于或等于', value: 'greaterOrEqual', escapeCharacter: '>=', includeType: ['date', 'datetime', 'time', 'number', 'decimal'],
  },
  {
    name: '小于', value: 'less', escapeCharacter: '<', includeType: ['date', 'datetime', 'time', 'number', 'decimal'],
  },
  {
    name: '小于或等于', value: 'lessOrEqual', escapeCharacter: '<=', includeType: ['date', 'datetime', 'time', 'number', 'decimal'],
  },
];
/**
 *   根据字段code与类型 加载关系列表
 * @param code
 * @param fieldType
 * @returns
 */
export function getAttributeRelation(code: string, fieldType: IFieldTypeWithSystemType) {
  return FastSearchAttributeRelations.filter((relation) => {
    const filterIncludeResult = relation.includeType?.includes(fieldType) ?? relation.includeCode?.includes(code);
    const filterExcludeResult = relation.excludeType?.includes(fieldType) ?? relation.excludeCode?.includes(code);
    let filterResult = typeof (filterIncludeResult) !== 'undefined' ? filterIncludeResult : true;
    filterResult = typeof (filterExcludeResult) !== 'undefined' ? !filterExcludeResult : filterResult;
    return filterResult;
  });
}
/**
 * 根据原字段得到加工后的属性对象
 * @param param0
 * @returns
 */
export function getFastSearchAttribute({
  code, fieldCode, fieldType, type, ...otherData
}: { code?: string, fieldCode?: string, fieldType?: string, type?: string, [propsName: string]: any }): { fieldCode: string, type: string, [propsName: string]: any } {
  if (code === 'story_point' || fieldCode === 'story_point') {
    return { fieldCode: 'story_point', type: 'number', ...otherData };
  }
  if (['last_updated_user', 'assignee', 'created_user', 'reporter'].includes(fieldCode || code!)) {
    return { fieldCode: code || fieldCode!, type: 'member', ...otherData };
  }
  return { fieldCode: code || fieldCode!, type: (fieldType || type)!, ...otherData };
}

const FieldTransformCodeRenderFieldObj = {
  fix_version: 'fixVersion',
  influence_version: 'influenceVersion',
  issue_type: 'issueTypeId',
  priority: 'priorityId',
  status: 'statusId',
};
interface FastFieldBaseProps {
  fieldCode: string
  fieldType: string
  [propsName: string]: any
}
/**
 * 将快速筛选的字段属性转换为renderField中所需的属性
 * @param param0
 * @returns
 */
export function transformFieldToRenderProps({ fieldCode, ...otherProps }: FastFieldBaseProps, omitProps: string[] = ['value']): IChosenFieldField {
  let value: any = otherProps.valueBindValue || otherProps.value;
  console.log('other', otherProps);
  const baseProps = {
    name: 'value',
    code: FieldTransformCodeRenderFieldObj[fieldCode as keyof typeof FieldTransformCodeRenderFieldObj] || fieldCode,
    ...omit(otherProps, 'defaultValue', 'value'),
  };
  // 人员有分页数据，需要value
  if (['member', 'multiMember'].includes(otherProps.fieldType)) {
    value = Array.isArray(otherProps.value) ? otherProps.value : [otherProps.value].filter(Boolean);
  } else {
    value = undefined;
  }
  return omit.apply(this, [merge(baseProps, { value }), ...omitProps]);
}
export function getTransformRelationRuleMap() {
  return new Map<string, string>(FastSearchAttributeRelations.map((relation) => [relation.value, relation.escapeCharacter]));
}
export function getTransformOperatorToRelationRuleMap() {
  return new Map<string, string>(FastSearchAttributeRelations.map((relation) => [relation.escapeCharacter, relation.value]));
}
/**
 *转换关系为对应的操作符
 * @param value
 * @param ruleMap
 * @returns
 */
export function transformRelationValueToOperation(value: string, ruleMap: Map<string, string> = getTransformRelationRuleMap()) {
  return ruleMap.get(value) || value;
}
export function transformDataToEditData(data: any): IFastSearchEditData | undefined {
  if (!data || !data.description) {
    return undefined;
  }
  const operatorRuleMap = getTransformOperatorToRelationRuleMap();
  const processData = pick(data, ['name', 'objectVersionNumber', 'projectId', 'filterId', 'childIncluded']);
  const description = data.description.split('+').slice(0, -3).join('+') || '';
  const { arr: searchConditionArr, o: bothRelationArr } = JSONbigString.parse(data.description.split('+').slice(-1));
  return {
    ...processData,
    description,
    searchConditionList: searchConditionArr.map((condition: { fieldCode: string, operation: string, predefined: boolean, value: string }, index: number) => {
      const relation = operatorRuleMap.get(condition.operation) || condition.operation;
      let { value }: { value: any } = condition;

      if (['include', 'exclude'].includes(relation)) {
        value = String(value).replace(/^\(|\)$/g, '').split(',');
      }
      const searchConditionItem = {
        fieldCode: condition.fieldCode,
        value,
        relation,
        bothRelation: bothRelationArr[index - 1],
        isCustomField: !condition.predefined,
        _editData: true,
      };
      return searchConditionItem;
    }),
  };
}
/**
 * 自定义字段根据类型得到反馈到后端类型
 * @param fieldType
 * @returns
 */
export function getCustomFieldType(fieldType: string) {
  return CustomFieldType[fieldType as keyof typeof CustomFieldType] || fieldType;
}
