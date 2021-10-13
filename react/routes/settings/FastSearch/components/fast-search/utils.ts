import { omit, pick, merge } from 'lodash';
// @ts-ignore
import JSONbig from 'json-bigint';
import { DataSet } from 'choerodon-ui/pro';
import moment from 'moment';
import { IChosenFieldField } from '@/components/chose-field/types';
import { getProjectId } from '@/utils/common';
import {
  IFastSearchCondition, IFastSearchEditData, IFastSearchEditConditionWithEditStatus, IFieldTypeWithSystemType,
} from './types';

const JSONbigString = JSONbig({ storeAsString: true });

interface FastSearchAttributeRelationItem {
  name: string
  value: string
  escapeCharacter: string
  excludeCode?: string[]
  excludeType?: string[]
  includeCode?: string[]
  includeType?: string[]

}
const DateFormatString = {
  datetime: 'YYYY-MM-DD HH:mm:ss',
  date: 'YYYY-MM-DD',
  time: 'HH:mm:ss',
};
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
    name: '等于', value: 'equal', escapeCharacter: '=', excludeType: ['date', 'datetime', 'time', 'multiMember'],
  },
  {
    name: '不等于', value: 'notEqual', escapeCharacter: '!=', excludeType: ['date', 'datetime', 'time', 'number', 'decimal', 'multiMember'],
  },
  {
    name: '包含', value: 'include', escapeCharacter: 'in', excludeType: ['date', 'datetime', 'time', 'number', 'decimal', 'radio', 'single', 'text', 'input'],
  },
  {
    name: '不包含', value: 'exclude', escapeCharacter: 'not in', excludeType: ['date', 'datetime', 'time', 'number', 'decimal', 'radio', 'single', 'text', 'input'],
  },
  {
    name: '包含', value: 'like', escapeCharacter: 'like', includeType: ['text', 'input'], // 文字类专属
  },
  {
    name: '不包含', value: 'notLike', escapeCharacter: 'notLike', includeType: ['text', 'input'], // 文字类专属
  },
  {
    name: '是', value: 'is', escapeCharacter: 'is', excludeType: ['date', 'datetime', 'time', 'text', 'input'], excludeCode: ['issue_type', 'priority', 'status'],
  },
  {
    name: '不是', value: 'notIs', escapeCharacter: 'is not', excludeType: ['date', 'datetime', 'time', 'text', 'input'], excludeCode: ['issue_type', 'priority', 'status'],
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
  code, fieldCode: propsFieldCode, fieldType, type, ...otherData
}: { code?: string, fieldCode?: string, fieldType?: string, type?: string, [propsName: string]: any }): { fieldCode: string, fieldType: IFieldTypeWithSystemType, [propsName: string]: any } {
  const fieldCode = (propsFieldCode || code)!;
  if (fieldCode === 'story_point') {
    return { fieldCode: 'story_point', fieldType: 'number', ...otherData };
  }
  if (['creation_date', 'last_update_date'].includes(fieldCode)) {
    return { fieldCode, fieldType: 'datetime', ...otherData };
  }
  if (['last_updated_user', 'assignee', 'created_user', 'reporter'].includes(fieldCode)) {
    return { fieldCode, fieldType: 'member', ...otherData };
  }
  if (['participant_id'].includes(fieldCode)) {
    return { fieldCode, fieldType: 'multiMember', ...otherData };
  }
  return { fieldCode, fieldType: (fieldType || type) as IFieldTypeWithSystemType, ...otherData };
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
  const baseProps = {
    name: 'value',
    code: FieldTransformCodeRenderFieldObj[fieldCode as keyof typeof FieldTransformCodeRenderFieldObj] || fieldCode,
    ...omit(otherProps, 'defaultValue', 'value'),
  };
  // 人员有分页数据，需要value feature是分页 需要value
  if (['member', 'multiMember'].includes(otherProps.fieldType) || baseProps.code === 'feature') {
    value = Array.isArray(otherProps.value) ? otherProps.value : [otherProps.value].filter(Boolean);
  } else {
    value = undefined;
  }
  return omit.apply(this, [merge(baseProps, { value }), ...omitProps]);
}
/**
 * 得到关系-->操作符 map集
 * @returns
 */
export function getTransformRelationRuleMap() {
  return new Map<string, string>(FastSearchAttributeRelations.map((relation) => [relation.value, relation.escapeCharacter]));
}
/**
 * 得到操作符-->关系 map集
 * @returns
 */
export function getTransformOperatorToRelationRuleMap() {
  return new Map<string, string>(FastSearchAttributeRelations.map((relation) => [relation.escapeCharacter, relation.value]));
}
/**
 * 处理值value 时间类型返回一个可靠的时间字符串,若不是时间类型value 则返回原值
 * @param value
 * @param condition
 * @returns
 */
export function processDataValue(value: any, condition: Pick<IFastSearchCondition, 'fieldCode' | 'fieldType' | 'isCustomField' | 'id'>): string {
  const newValue = value;
  if (['date', 'time', 'datetime'].includes(condition.fieldType)) {
    return moment(newValue, ['YYYY-MM-DD HH:mm:ss', 'HH:mm:ss']).format(DateFormatString[condition.fieldType as 'date' | 'time']);
  }
  return newValue;
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
/**
 * 将查询的详情转换为组件内部所需的数据格式
 * @param data
 * @returns
 */
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
        _editData: true, // 编辑数据标记，用以编辑数据加载对应select组件后 DataSet字段value 进行重新赋值
      };
      return searchConditionItem;
    }),
  };
}
/**
 * 将转换过的查询的详情数据中的筛选条件进行 的值 字段进行处理，保证再次保存的时候能够获取到显示值
 * @param searchConditionList
 * @param fieldData
 * @returns
 */
export function transformSearchConditionListToEditData(searchConditionList: IFastSearchEditConditionWithEditStatus[], fieldData: ReturnType<typeof getFastSearchAttribute>[]) {
  return searchConditionList.map((item) => {
    const attribute = fieldData.find((i) => i.fieldCode === item.fieldCode);
    let { value } = item;
    // 关系为 是/不是，  'null' 值 处理
    if (['is', 'notIs'].includes(item.relation)) {
      value = { value: "'null'", meaning: '空' };
    } else if (item.isCustomField && attribute?.fieldOptions) { // 含有选项的自定义字段处理
      const valueArr = Array.isArray(value) ? value : [value].filter(Boolean);
      const valueOptions = attribute.fieldOptions.filter((i: any) => valueArr.includes(i.id))
        .map((i: any) => ({ ...i, meaning: i.value, value: i.id }));
      value = typeof (value) === 'string' || valueOptions.length === 0 ? valueOptions[0] || value : valueOptions;
    }
    if (attribute) {
      value = processDataValue(value, { ...item, fieldType: attribute.fieldType! });
    }
    return ({
      ...item, attribute, value, _editDataCode: attribute?.fieldCode,
    });
  }).filter((item) => item.attribute);
}
/**
 * 自定义字段根据类型得到反馈到后端类型
 * @param fieldType
 * @returns
 */
export function getCustomFieldType(fieldType: string) {
  return CustomFieldType[fieldType as keyof typeof CustomFieldType] || fieldType;
}

/**
 * 处理待提交的数据
 * @param mainDataSet 主体DS
 * @param searchConditionDataSet 筛选条件DS
 * @returns
 */
export function processWaitSubmitData(mainDataSet: DataSet, searchConditionDataSet: DataSet) {
  const bothRelationArr: string[] = []; //  两个相邻筛选条件关系
  const expressQueryArr: any[] = [];
  const searchConditionArr = searchConditionDataSet.toJSONData().map((condition: IFastSearchCondition) => {
    const value = processDataValue(condition.valueBindValue || condition.value, condition);
    const operation = transformRelationValueToOperation(condition.relation);
    if (condition.bothRelation) {
      bothRelationArr.push(condition.bothRelation);
      expressQueryArr.push(condition.bothRelation.toUpperCase());
    }
    // 属性名
    expressQueryArr.push(condition.name);
    // 关系
    expressQueryArr.push(operation);
    // 显示的值
    expressQueryArr.push(Array.isArray(condition.valueText) ? `[${condition.valueText.join(',')}]` : condition.valueText || value);
    return {
      fieldCode: condition.fieldCode,
      operation,
      value: Array.isArray(value) ? `(${value.join(',')})` : `${value}`,
      predefined: !condition.id,
      customFieldType: condition.id ? getCustomFieldType(condition.fieldType) : undefined,
    };
  });
  const mainData = mainDataSet.current?.toData();
  const filterJson = JSON.stringify({
    arr: searchConditionArr,
    o: bothRelationArr,
  });
  return {
    ...pick(mainData, 'name'),
    childIncluded: true,
    description: `${mainData.description || ''}+++${filterJson}`,
    expressQuery: expressQueryArr.join(' '),
    projectId: getProjectId(),
    quickFilterValueVOList: searchConditionArr,
    relationOperations: bothRelationArr,
  };
}
