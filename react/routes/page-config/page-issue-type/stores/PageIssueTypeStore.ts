import {
  observable, action, runInAction, computed, ObservableMap,
} from 'mobx';
import { findIndex } from 'lodash';
import moment from 'moment';
import {
  PageConfigIssueType, pageConfigApi, IFiledProps, IFiledListItemProps, IFieldOptionProps,
} from '@/api';
import { DataSet } from 'choerodon-ui/pro/lib';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { IFieldPostDataProps } from '../../components/create-field/CreateField';

export enum PageIssueTypeStoreStatusCode {
  del = 'delete',
  add = 'add',
  null = '',
}

interface IDescriptionTempleProps {
  id: string | undefined,
  template: undefined | string | Array<any>,
  originTemplate: undefined | string,
  objectVersionNumber: undefined | number,
  dirty: boolean,
}
export type PageIFieldPostDataProps = IFieldPostDataProps & {
  local?: boolean, fieldName: string, localSource: string, // 'add' | 'created'
  localDefaultValue?: any,
  edited: boolean, created: boolean, required: boolean, rank?: string,
  dataSetRecord?: Record,
};
interface IAddPostData {
  fieldId: string,
  rank: string,
}
class PageIssueTypeStore {
  constructor(props: { addUnselectedDataSet: DataSet, sortTableDataSet: DataSet }) {
    this.addUnselectedDataSet = props.addUnselectedDataSet;
    this.sortTableDataSet = props.sortTableDataSet;
  }

  @observable sortTableDataSet: DataSet;

  @observable addUnselectedDataSet: DataSet;

  @observable loading: boolean = false;

  @observable allFieldData: ObservableMap<any, IFiledListItemProps> = observable.map();

  @observable currentIssueType: PageConfigIssueType = PageConfigIssueType.null;

  @observable dataStatusCode: PageIssueTypeStoreStatusCode = PageIssueTypeStoreStatusCode.null;

  @observable deleteIds: Array<string> = [];

  @observable deleteRecords: Array<Record> = []; // 删除列表中初始时所拥有的字段

  @observable addFields: Array<IAddPostData> = [];

  @observable createdFields: Array<PageIFieldPostDataProps> = [];

  @observable descriptionObj: IDescriptionTempleProps = {
    id: undefined,
    template: undefined,
    originTemplate: undefined,
    objectVersionNumber: undefined,
    dirty: false,
  };

  @action init(issueType: PageConfigIssueType) {
    this.currentIssueType = issueType;
  }

  @action('清空全部数据') destroy() {
    this.clear();
    this.currentIssueType = PageConfigIssueType.null;
  }

  @action('清空编辑数据') clear() {
    this.dataStatusCode = PageIssueTypeStoreStatusCode.null;
    this.deleteIds.length = 0;
    this.deleteRecords.length = 0;
    this.descriptionObj = {
      id: undefined,
      template: undefined,
      originTemplate: undefined,
      objectVersionNumber: undefined,
      dirty: false,
    };
    this.addFields.length = 0;
    this.createdFields.length = 0;
  }

  @action addDeleteRecord(record: Record) {
    this.deleteRecords.push(record);
  }

  @computed get getDeleteRecords() {
    return this.deleteRecords.slice();
  }

  @action('增添删除字段') addDeleteId(id: string) {
    this.deleteIds.push(id);
  }

  @action('移除删除字段') removeDeleteId(id: string) {
    const index = findIndex(this.deleteIds, (deleteId: string) => deleteId === id);
    index !== -1 && this.deleteIds.splice(index, 1);
  }

  @action('移除删除缓存记录') removeDeleteRecord(RecordId: number) {
    const index = findIndex(this.deleteRecords, (record: Record) => record.id === RecordId);
    index !== -1 && this.deleteRecords.splice(index, 1);
  }

  @action('删除本地字段') deleteLocalField(code: string, id?: string) {
    let index = -1;
    if (id) { // id 存在 则删除已有字段集合
      index = this.addFields.findIndex((item) => item.fieldId === id);
      index !== -1 && this.addFields.splice(index, 1);
      return;
    }
    index = this.createdFields.findIndex((item) => item.code === code);
    index !== -1 && this.createdFields.splice(index, 1);
  }

  @action('增加已有字段') addNewLocalField(data: IAddPostData) {
    this.addFields.push(data);
  }

  @action('增添新字段') addCreatedField(data: PageIFieldPostDataProps) {
    this.createdFields.push(data);
  }

  @action('最新创建的新字段绑定record') bindRecordForCreated(record: Record) {
    this.createdFields[this.createdFields.length - 1].dataSetRecord = record;
  }

  @computed get getAddFields() {
    return this.addFields.slice();
  }

  @computed get getCreatedFields() {
    return this.createdFields.slice();
  }

  @action setLoading(data: boolean) {
    this.loading = data;
  }

  @computed get getLoading() {
    return this.loading;
  }

  @action setDescriptionObj(data: IDescriptionTempleProps) {
    this.descriptionObj = data;
  }

  @action changeTemplate(data: Array<any>) {
    const dataStr = JSON.stringify(data);
    if (dataStr === this.descriptionObj.originTemplate) {
      this.descriptionObj.dirty = false;
    } else {
      this.descriptionObj.dirty = !this.descriptionObj.id ? dataStr !== JSON.stringify([{ insert: '\n' }]) : true;
    }
    this.descriptionObj.template = data;
  }

  @computed get getDescriptionObj() {
    return this.descriptionObj;
  }

  @computed get getDeleteIds() {
    return this.deleteIds.slice();
  }

  @action setDataStatusCode(code: PageIssueTypeStoreStatusCode) {
    this.dataStatusCode = code;
  }

  @action('设置数据状态') changeDataStatusCode(code: PageIssueTypeStoreStatusCode) {
    if (this.dataStatusCode === PageIssueTypeStoreStatusCode.del) {
      return;
    }
    if (this.dataStatusCode === PageIssueTypeStoreStatusCode.add) {
      this.dataStatusCode = code;
    }
  }

  @computed get getDirty() {
    return this.getDataStatusCode !== PageIssueTypeStoreStatusCode.null
      || this.getDescriptionObj.dirty || this.sortTableDataSet.dirty;
  }

  @computed get getDataStatusCode() {
    return this.dataStatusCode;
  }

  @action setCurrentIssueType(issueType: PageConfigIssueType) {
    this.currentIssueType = issueType;
  }

  @computed get getCurrentIssueType() {
    return this.currentIssueType;
  }

  @action loadAllField = () => {
    pageConfigApi.load().then((res: any) => {
      res?.content?.map((item: any) => {
        this.allFieldData.set(item.id, item);
      });
    });
  }

  transformDefaultValue = (fieldType: string, defaultValue: any, defaultValueObj?: any, fieldOptions?: Array<IFieldOptionProps> | null, optionKey: 'code' | 'id' = 'id') => {
    if (!defaultValue && !defaultValueObj) {
      return defaultValue;
    }
    switch (fieldType) {
      case 'datetime':
        return moment(defaultValue).format('YYYY-MM-DD HH:mm:ss');
      case 'time':
        return moment(defaultValue).format('HH:mm:ss');
      case 'date':
        return moment(defaultValue).format('YYYY-MM-DD');
      case 'multiple':
      case 'checkbox':
      case 'single':
      case 'radio': {
        const valueArr = String(defaultValue).split(',');
        return fieldOptions?.filter((option) => valueArr.some((v) => v === option[optionKey])).map((item) => item.value).join(',') || defaultValue;
      }
      case 'member': {
        const { realName } = defaultValueObj || {};
        return realName || defaultValue;
      }
      default:
        return defaultValue;
    }
  };

  loadData = () => {
    this.clear();
    this.addUnselectedDataSet.clear();
    this.setLoading(true);
    pageConfigApi.loadByIssueType(this.getCurrentIssueType).then((res) => {
      this.sortTableDataSet.loadData(res.fields.map((field) => ({ ...field, defaultValue: this.transformDefaultValue(field.fieldType, field.defaultValue, field.defaultValueObj, field.fieldOptions) })));
      if (res.issueTypeFieldVO) {
        this.setDescriptionObj({
          ...res.issueTypeFieldVO,
          originTemplate: res.issueTypeFieldVO.template,
          dirty: false,
        });
      }
      this.setLoading(false);
    });
  }
}
export default PageIssueTypeStore;
