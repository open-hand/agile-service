import {
  observable, action, computed, toJS,
} from 'mobx';
import { find, isEmpty } from 'lodash';
import { fieldApi, personalFilterApi } from '@/api';
import { IField, ISearchVO } from '@/common/types';
import { getProjectId } from '@/utils/common';
import { IPersonalFilter } from '../quick-search';
import {
  flattenObject, isFilterSame, SearchVOToFilter, filterInvalidAttribute, getEmptyValue,
} from './utils';

export type ILocalField = {
  code: string,
  name: string,
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

export type IBatchAction = undefined | 'edit' | 'delete'

export interface IssueSearchStoreProps {
  getSystemFields: () => ILocalField[]
  transformFilter: (chosenFields: IChosenFields) => any
  renderField?: (field: IChosenField, props: { onChange: (v: any) => void, value: any, projectId?: string }) => React.ReactElement | React.FunctionComponent | null | void | false
  defaultChosenFields?: IChosenFields,
  defaultSearchVO?: ISearchVO
  projectId?: string
}
class IssueSearchStore {
  query: () => void = () => { }

  getSystemFields: () => ILocalField[] = () => []

  renderField: IssueSearchStoreProps['renderField']

  transformFilter: (chosenFields: IChosenFields) => any = () => { }

  defaultChosenFields?: IChosenFields;

  defaultSearchVO?: ISearchVO

  projectId?: string

  constructor({
    getSystemFields,
    transformFilter,
    renderField,
    defaultChosenFields,
    defaultSearchVO,
    projectId,
  }: IssueSearchStoreProps) {
    this.getSystemFields = getSystemFields;
    this.transformFilter = transformFilter;
    this.renderField = renderField || (() => null);
    this.defaultChosenFields = defaultChosenFields;
    this.defaultSearchVO = defaultSearchVO;
    this.projectId = projectId;
  }

  setQuery(query: () => void) {
    this.query = query;
  }

  // 我的筛选列表
  @observable myFilters: IPersonalFilter[] = [];

  @computed get getMyFilters(): IPersonalFilter[] {
    return toJS(this.myFilters);
  }

  @action setMyFilters(data: IPersonalFilter[]) {
    this.myFilters = data;
  }

  loadMyFilterList = async () => {
    const data = await personalFilterApi.project(this.projectId || getProjectId()).loadAll();
    this.setMyFilters(data);
  };

  // 自定义字段，供选择
  @observable fields: IField[] = []

  async loadCustomFields() {
    const fields = await fieldApi.project(this.projectId || getProjectId()).getCustomFields();
    this.setFields(fields);
    // 自定义字段加载完，设置默认searchVO的内容
    if (this.defaultSearchVO) {
      this.transformChosenFields();
    }
  }

  @action
  transformChosenFields() {
    const filter = SearchVOToFilter(this.defaultSearchVO || {});
    Object.keys(filter).filter((key) => filter[key] !== undefined).forEach((key) => {
      const systemFields = this.getSystemFields();
      const isSystemField = find(systemFields, { code: key });
      const field = isSystemField || find(this.fields, { id: key });

      if (!field) {
        return;
      }
      this.chosenFields.set(isSystemField ? key : field.code, { ...field, value: filter[key] });
    });
  }

  @action setFields(fields: IField[]) {
    this.fields = fields;
  }

  @observable chosenFields: IChosenFields = new Map();

  @action initChosenFields() {
    const chosenFields = new Map(this.chosenFields);

    this.getSystemFields()
      .filter((f) => f.defaultShow)
      .forEach((f) => {
        if (!chosenFields.has(f.code)) {
          chosenFields.set(f.code, observable({ ...f, value: undefined }));
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
      console.log(field, getEmptyValue(field.fieldType));
      this.chosenFields.set(code, observable({ ...field, value: getEmptyValue(field.fieldType) }));
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
    this.setFieldFilter(code, value);
    this.query();
  }

  // 获取所有字段 包括系统字段
  @computed get getAllFields() {
    const allField = [...this.fields, ...this.getSystemFields()];
    return allField;
  }

  @action setFieldFilter = (code: string, value: any) => {
    const field = this.chosenFields.get(code);
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
      this.chosenFields.set(field.code, observable({ ...field, value: undefined }));
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
    for (const [, field] of this.chosenFields) {
      if (field.archive) {
        this.handleChosenFieldChange(false, field as IField);
      } else if (field.value) {
        field.value = undefined;
      }
    }
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
      if (value === undefined || value === null || value === '') {
        // eslint-disable-next-line no-continue
        continue;
      }
      filter.push(field);
    }
    return filter;
  }

  getCustomFieldFilters = () => this.transformFilter(this.chosenFields)

  @computed
  get isHasFilter() {
    const currentFilterDTO = this.getCustomFieldFilters()
      ? flattenObject(this.getCustomFieldFilters()) : {};
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
