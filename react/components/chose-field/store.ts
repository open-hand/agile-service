import {
  observable, action, runInAction, computed, ObservableMap, toJS,
} from 'mobx';
import { IChosenFieldField, IUseChoseFieldProps } from './types';

type IChosenSpecialFieldField = IChosenFieldField & Required<Pick<IChosenFieldField, 'immutableCheck'>>
class ChoseFieldStore {
  constructor({
    systemFields, customFields, chosenFields, addFieldCallback,
  }: IUseChoseFieldProps) {
    this.fields = observable.map();
    this.fields.set('system', systemFields);
    this.fields.set('custom', customFields);
    chosenFields?.forEach((field) => {
      if (typeof (field.immutableCheck) === 'undefined') {
        if (typeof (field.value) !== 'undefined') {
          this.chosenFields.set(field.code, field);
        }
      } else {
        // 设置特殊值 不可更改
        this.addSpecialFields(field.code, field as IChosenSpecialFieldField);
      }
    });
    this.addFieldCallback = addFieldCallback;
  }

  addFieldCallback: undefined | ((key: string) => void)

  @observable currentOptionStatus: 'ALL' | 'PART' | 'NONE' = 'NONE';

  @observable fields: ObservableMap<'system' | 'custom', Array<IChosenFieldField>>;;

  @observable chosenFields: ObservableMap<string, IChosenFieldField> = observable.map();

  @observable specialFields: ObservableMap<string, IChosenSpecialFieldField> = observable.map();

  @observable searchVal?: string;

  @action setSearchVal(data: string) {
    this.searchVal = data;
  }

  @computed get getSearchVal() {
    return this.searchVal;
  }

  @computed get getChosenField() {
    return this.chosenFields;
  }

  @computed get getSpecialFields() {
    return this.specialFields;
  }

  @computed get getAllChosenField() {
    return [...this.specialFields.values(), ...this.chosenFields.values()];
  }

  getChosenByCode(code: string) {
    return this.chosenFields.get(code);
  }

  filterFieldBySearchVal = (field: IChosenFieldField) => field.name.indexOf(this.searchVal!) > -1

  @computed get getOriginalField() {
    return this.fields;
  }

  @computed get fieldMaps() {
    return observable.map([...this.fields.get('system')!, ...this.fields.get('custom')!].map((field) => [field.code, field]));
  }

  @computed get fieldCount() {
    return (this.fields.get('system')?.length || 0) + (this.fields.get('custom')?.length || 0);
  }

  @computed get getFields() {
    if (this.searchVal && this.searchVal !== '') {
      return [this.fields.get('system')!.filter(this.filterFieldBySearchVal), this.fields.get('custom')!.filter(this.filterFieldBySearchVal)];
    }
    return [this.fields.get('system')!, this.fields.get('custom')!];
  }

  @computed get getCurrentOptionStatus() {
    return this.currentOptionStatus;
  }

  @action('状态值自更新')
  selfUpdateCurrentOptionStatus() {
    let nextOptionStatus: any = this.chosenFields.size ? 'PART' : 'NONE';
    if (this.chosenFields.size === (this.fields.get('system')!.length + this.fields.get('custom')!.length)) {
      nextOptionStatus = 'ALL';
    }
    this.currentOptionStatus = nextOptionStatus;
  }

  /**
   * @deprecated 将在后续迁移到新方法
   * @param key
   * @param data
   * @param value
   */
  @action('增添选择字段') addChosenFields(key: string, data: IChosenFieldField, value?: any) {
    const field = this.fieldMaps.get(data.code || key) || data;
    this.chosenFields.set(key, { ...field, value: value || undefined });
    if (this.addFieldCallback) {
      this.addFieldCallback(key);
    }
    this.selfUpdateCurrentOptionStatus();
  }

  @action('增添特殊选择字段') addSpecialFields(key: string, data: IChosenSpecialFieldField) {
    this.specialFields.set(key, data);
  }

  @action('删除选择字段') delChosenFields(key: string) {
    this.chosenFields.delete(key);
    this.selfUpdateCurrentOptionStatus();
  }

  @action('增添全部字段') addAllChosenFields() {
    //   this.chosenFields.set(key, data);
    const currentChosenFields: IChosenFieldField[] = [];
    const [systemFiled, customFiled] = this.getFields;
    [...systemFiled, ...customFiled].forEach((field) => {
      if (!this.chosenFields.has(field.code) && !this.specialFields.has(field.code)) {
        this.chosenFields.set(field.code, { ...field, value: undefined });
        if (this.addFieldCallback) {
          this.addFieldCallback(field.code);
        }
        currentChosenFields.push(field);
      }
    });
    this.currentOptionStatus = 'ALL';
    return currentChosenFields;
  }

  @action('取消选择全部字段') cancelAllChosenFields() {
    //   this.chosenFields.set(key, data);
    const data = [...this.chosenFields.values()];
    this.chosenFields.clear();
    this.currentOptionStatus = 'NONE';
    return data;
  }
}

export default ChoseFieldStore;
