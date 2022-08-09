import {
  observable, action, computed, toJS,
} from 'mobx';
import {
  cloneDeep,
  debounce, find, isEmpty, isNull, isObject, omit, set, unset,
} from 'lodash';
import { fieldApi, personalFilterApi } from '@/api';
import { IField, ISearchVO } from '@/common/types';
import { getProjectId } from '@/utils/common';
import { IPersonalFilter } from '../quick-search';
import {
  flattenObject, isFilterSame, filterInvalidAttribute, getEmptyValue,
} from './utils';

export type ILocalField = {
  code: string,
  name: string,
  nameKey?: string
  fieldType: IField['fieldType'],
  defaultShow?: boolean
  noDisplay?: boolean

}
type IChosenField = (IField | ILocalField) & {
  value: any
  // 废弃的字段
  archive?: boolean
}
export type IChosenFields = Map<string, IChosenField>

export type IBatchAction = undefined | 'edit' | 'delete' | 'move'

export interface IssueSearchStoreProps {
  getSystemFields: () => ILocalField[]
  renderSearchFields?: (fields: any[], fieldCodeProps?: Record<string, any>) => any
  transformFilter: (chosenFields: IChosenFields) => any
  renderField?: (field: IChosenField, props: { onChange: (v: any) => void, value: any, projectId?: string }) => React.ReactElement | React.FunctionComponent | null | void | false
  defaultChosenFields?: IChosenFields,
  defaultSearchVO?: ISearchVO
  projectId?: string
  /** @default project */
  menuType?: 'project' | 'org' | 'workbench' | 'program' | 'programSubProject'
  issueTypeList?: 'agileIssueType' | 'programIssueType' | 'riskIssueType' | ''
  filterTypeCode?: 'agile_issue' | 'program_issue' | 'risk_issue' | ''
  /** program 模式使用 */
  programId?: string
  fieldConfigs?: { [key: string]: any }
  defaultTransformKeyMap?: Map<string, string>, // 将转换数据转为筛选可用字段时code映射
  customUpdateFilter?: (key: string, filter: any, fun: (code: string, value: any) => void) => any, // 更新筛选数据方法
  /**
   * 转换扁平筛选数据结构（目前仅用于还原我的筛选）
   */
  transformFlattenFilter?: (flattenFilter: Record<string, any>) => Record<string, any>
}
function isInvalidValue(value: any) {
  if (value === undefined || isNull(value) || (isObject(value) && isEmpty(value))) {
    return true;
  }
  return false;
}
/**
 * 过滤字段中空选项
 *
 */

function filterFieldWithoutNull<T extends { [x: string]: any } = any, K extends string = string>(data: T, maxDepth = 3) {
  if (!maxDepth) {
    return;
  }
  const keepKeys = ['advancedSearchArgs', 'otherArgs', 'searchArgs', 'customField'];
  (Object.keys(data) as Array<K>).forEach((key) => {
    if (isInvalidValue(data[key]) && !keepKeys.includes(key)) {
      unset(data, key);
    } else if (key === 'customField') {
      // 自定义字段单独处理
      const customField = data[key];
      Object.keys(customField).forEach((fieldTypeKey) => {
        const fieldTypeArr = customField[fieldTypeKey].filter((item: any) => {
          const itemWithoutFiledId = omit(item, ['fieldId']);
          return Object.values(itemWithoutFiledId).some((i) => !isEmpty(i));
        });
        set(data, `${key}.${fieldTypeKey}`, fieldTypeArr);
      });
      // 不处理系统字段中的数组
    } else if (isObject(data[key]) && !Array.isArray(data[key])) {
      filterFieldWithoutNull(data[key], maxDepth - 1);
    }
  });
}
class IssueSearchStore {
  query: () => void = () => { }

  getSystemFields: () => ILocalField[] = () => []

  renderSearchFields: IssueSearchStoreProps['renderSearchFields']

  renderField: IssueSearchStoreProps['renderField']

  transformFilter: (chosenFields: IChosenFields) => any = () => { }

  transformFlattenFilter: NonNullable<IssueSearchStoreProps['transformFlattenFilter']>;

  defaultChosenFields?: IChosenFields;

  defaultSearchVO?: ISearchVO

  projectId?: string

  programId?: string;

  issueTypeList?: IssueSearchStoreProps['issueTypeList']

  filterTypeCode?: IssueSearchStoreProps['filterTypeCode']

  fieldConfigs: { [key: string]: any } = {}

  // 只有项目层加载个人筛选和自定义字段
  menuType: IssueSearchStoreProps['menuType'] = 'project';

  defaultTransformKeyMap: IssueSearchStoreProps['defaultTransformKeyMap'];

  customUpdateFilter: IssueSearchStoreProps['customUpdateFilter'];

  constructor({
    getSystemFields,
    transformFilter,
    renderSearchFields,
    renderField,
    defaultChosenFields,
    defaultSearchVO,
    projectId,
    programId,
    menuType,
    fieldConfigs,
    issueTypeList,
    filterTypeCode,
    defaultTransformKeyMap,
    customUpdateFilter,
    transformFlattenFilter,
  }: IssueSearchStoreProps) {
    this.getSystemFields = getSystemFields;
    this.transformFilter = transformFilter;
    this.renderSearchFields = renderSearchFields;
    this.renderField = renderField || (() => null);
    this.defaultChosenFields = defaultChosenFields;
    this.defaultSearchVO = defaultSearchVO;
    this.projectId = projectId;
    this.programId = programId;
    this.menuType = menuType || 'project';
    this.fieldConfigs = fieldConfigs || {};
    this.issueTypeList = issueTypeList;
    this.filterTypeCode = filterTypeCode;
    this.defaultTransformKeyMap = defaultTransformKeyMap;
    this.customUpdateFilter = customUpdateFilter;
    this.transformFlattenFilter = transformFlattenFilter ?? ((f) => f);
  }

  setQuery(query: () => void) {
    this.query = debounce(query, 300);
  }

  // 我的筛选列表
  @observable myFilters: IPersonalFilter[] = [];

  @computed get getMyFilters(): IPersonalFilter[] {
    return toJS(this.myFilters);
  }

  setProgramId(data: string) {
    this.programId = data;
  }

  @action setMyFilters(data: IPersonalFilter[]) {
    this.myFilters = data;
  }

  loadMyFilterList = async () => {
    const data = this.menuType && ['project', 'program'].includes(this.menuType) ? await personalFilterApi.menu('project').project(this.projectId || getProjectId()).loadAll(undefined, this.filterTypeCode) : [];
    this.setMyFilters(data);
  };

  // 自定义字段，供选择
  @observable fields: IField[] = []

  async loadCustomFields() {
    const fields = this.menuType && ['project', 'program'].includes(this.menuType) ? await fieldApi.program(this.programId).project(this.projectId || getProjectId()).getCustomFields(this.issueTypeList) : [];
    this.setFields(fields);
    // 自定义字段加载完，设置默认searchVO的内容
    if (this.defaultSearchVO) {
      this.transformChosenFields();
    }
  }

  @action
  transformChosenFields() {
    const filter = flattenObject(this.defaultSearchVO || {});
    this.updateFilter(this.transformFlattenFilter ? this.transformFlattenFilter(filter) : filter);
  }

  @action updateFilter(filter: { [key: string]: any }) {
    for (const [key, value] of Object.entries(filter)) {
      if (value !== undefined) {
        // 自定义字段保存的时候只保存了id，这里要找到code
        // @ts-ignore
        if (value && isObject(value) && value.isCustom) {
          const code = this.getFieldCodeById(key);
          if (code) {
            // @ts-ignore
            this.handleFilterChange(code, value.value);
          }
        } else if (key === 'createEndDate' || key === 'createStartDate') {
          this.handleFilterChange('createDate', [filter.createStartDate, filter.createEndDate]);
        } else if (key === 'updateEndDate' || key === 'updateStartDate') {
          this.handleFilterChange('updateDate', [filter.updateStartDate, filter.updateEndDate]);
        } else if (key === 'estimatedStartTimeScopeStart' || key === 'estimatedStartTimeScopeEnd') {
          this.handleFilterChange('estimatedStartTime', [filter.estimatedStartTimeScopeStart, filter.estimatedStartTimeScopeEnd]);
        } else if (key === 'estimatedEndTimeScopeStart' || key === 'estimatedEndTimeScopeEnd') {
          this.handleFilterChange('estimatedEndTime', [filter.estimatedEndTimeScopeStart, filter.estimatedEndTimeScopeEnd]);
        } else if (key === 'actualStartTimeScopeStart' || key === 'actualStartTimeScopeEnd') {
          this.handleFilterChange('actualStartTime', [filter.actualStartTimeScopeStart, filter.actualStartTimeScopeEnd]);
        } else if (key === 'actualEndTimeScopeStart' || key === 'actualEndTimeScopeEnd') {
          this.handleFilterChange('actualEndTime', [filter.actualEndTimeScopeStart, filter.actualEndTimeScopeEnd]);
        } else {
          this.handleFilterChange(key, value);
        }
        if (this.customUpdateFilter) {
          this.customUpdateFilter(key, filter, this.handleFilterChange);
        }
      }
    }
  }

  @action setFields(fields: IField[]) {
    this.fields = fields;
  }

  @observable chosenFields: IChosenFields = new Map();

  @computed get chosenNotDisplayField() {
    const noDisplayChosenFields: IChosenField[] = [];
    this.chosenFields.forEach((value, key) => {
      if (!((value as ILocalField).noDisplay || (value as ILocalField).defaultShow)) {
        noDisplayChosenFields.push(value);
      }
    });
    return noDisplayChosenFields;
  }

  @computed get chosenDefaultDisplaySet() {
    return new Set([...this.chosenFields.values()].filter((field: ILocalField) => field.defaultShow || !field.noDisplay).map((i) => i.code));
  }

  @action initChosenFields() {
    const chosenFields = new Map(this.chosenFields);

    this.getSystemFields()
      .filter((f) => f.defaultShow)
      .forEach((f) => {
        if (!chosenFields.has(f.code)) {
          chosenFields.set(f.code, observable({ ...f, value: getEmptyValue(f) }));
        }
      });
    this.chosenFields = chosenFields;
    if (this.defaultChosenFields) {
      this.defaultChosenFields.forEach((value, key) => {
        this.chosenFields.set(key, value);
      });
    }
  }

  @action handleChosenFieldChange = (select: boolean, field: IField) => {
    const { code } = field;
    if (select) {
      this.chosenFields.set(code, observable({ ...field, value: getEmptyValue(field) }));
    } else {
      const { value } = this.chosenFields.get(code) as IChosenField;
      this.chosenFields.delete(code);
      if (value !== null && value !== undefined) {
        this.query();
      }
    }
  }

  getFilterValueByCode(code: string) {
    return this.chosenFields.has(code) ? toJS(this.chosenFields.get(code)?.value) : undefined;
  }

  handleFilterChange = (code: string, value: any) => {
    const realCode = this.defaultTransformKeyMap?.get(code) || code;
    this.setFieldFilter(realCode, value);
    this.query();
  }

  // 获取所有字段 包括系统字段
  @computed get getAllFields() {
    const allField = [...this.fields, ...this.getSystemFields()];
    return allField;
  }

  @action setFieldFilter = (code: string, v: any) => {
    const field = this.chosenFields.get(code);
    // 这里如果select清空，那么置为空数据
    const value = v ?? getEmptyValue(field);
    // 说明这时候没有被选择，那么要自动选上
    if (!field) {
      const unSelectField = find([...this.fields, ...this.getSystemFields()], { code });
      if (unSelectField) {
        this.chosenFields.set(code, observable({ ...unSelectField, value }));
      }
    } else {
      field.value = value;
    }
  }

  @action setChosenFields(chosenFields: IChosenFields) {
    this.chosenFields = chosenFields;
  }

  @action chooseAll(filteredFields: IField[]) {
    filteredFields.forEach((field) => {
      this.chosenFields.set(field.code, observable({ ...field, value: getEmptyValue(field) }));
    });
  }

  @action unChooseAll() {
    let hasValue = false;
    for (const [, field] of this.chosenFields) {
      if (!isEmpty(field.value)) {
        hasValue = true;
        break;
      }
    }
    this.chosenFields = new Map(this.getSystemFields().filter((f) => f.defaultShow)
      .map((f) => ([f.code, this.chosenFields.get(f.code)]))) as IChosenFields;
    // 取消全选之前如果有筛选就查一次
    if (hasValue) {
      this.query();
    }
  }

  @action
  clearAllFilter() {
    this.chosenFields.clear();
    this.getSystemFields()
      .filter((f) => f.defaultShow)
      .forEach((f) => {
        if (!this.chosenFields.has(f.code)) {
          this.chosenFields.set(f.code, observable({ ...f, value: getEmptyValue(f) }));
        }
      });
  }

  getFieldCodeById(id: string) {
    const field = find(this.fields, { id });
    return field ? field.code : undefined;
  }

  @computed
  get currentFilter() {
    const filter: IChosenField[] = [];
    for (const [, field] of this.chosenFields) {
      const value = toJS(field.value);
      if (value === undefined || value === null || value === '' || (Array.isArray(value) && !value.length)) {
        // eslint-disable-next-line no-continue
        continue;
      }
      filter.push(field);
    }
    return filter;
  }

  getCustomFieldFilters = (filterNull = false) => {
    if (!filterNull) {
      return this.transformFilter(this.chosenFields);
    }
    const filter = cloneDeep(toJS(this.transformFilter(this.chosenFields)));
    // filterFieldWithoutNull(filter);
    return filter;
  }

  @computed
  get isHasFilter() {
    const currentFilterDTO = this.getCustomFieldFilters()
      ? flattenObject(this.getCustomFieldFilters()) : {};
    Object.keys(currentFilterDTO).forEach((code) => {
      if (Array.isArray(currentFilterDTO[code]) && !currentFilterDTO[code].length && this.chosenDefaultDisplaySet.has(code)) {
        unset(currentFilterDTO, code);
      }
    });
    return !isFilterSame({}, currentFilterDTO);
  }

  @computed
  get currentFlatFilter() {
    const currentFilterDTO = this.getCustomFieldFilters()
      ? flattenObject(this.getCustomFieldFilters()) : {};
    return filterInvalidAttribute(currentFilterDTO);
  }

  @observable batchAction: IBatchAction;

  @action setBatchAction = (data: IBatchAction) => {
    this.batchAction = data;
  }

  @observable overflowLine: boolean = false;

  @action setOverflowLine = (data: boolean) => {
    this.overflowLine = data;
  }

  @observable folded: boolean | undefined;

  @action setFolded = (data: boolean) => {
    this.folded = data;
  }
}
export default IssueSearchStore;
