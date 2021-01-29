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
import { transformDefaultValue } from '../utils';

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
  showDefaultValueText?: any, // 默认值展示字段
  edited: boolean, created: boolean, required: boolean, rank?: string,
  dataSetRecord?: Record,
};
interface IAddPostData {
  fieldId: string,
  rank: string,
  created: boolean,
  edited: boolean,
  required: boolean,
  localRecordIndexId: number,
}
interface IPageIssueTypeStoreIssueType {
  id: string,
  enabled: boolean,
  [x: string]: any
}
class PageIssueTypeStore {
  constructor(props: { addUnselectedDataSet: DataSet, sortTableDataSet: DataSet }) {
    this.addUnselectedDataSet = props.addUnselectedDataSet;
    this.sortTableDataSet = props.sortTableDataSet;
  }

  @observable sortTableDataSet: DataSet;

  @observable addUnselectedDataSet: DataSet;

  @observable loading: boolean = false;

  @observable currentTypeAllFieldData: ObservableMap<any, IFiledListItemProps> = observable.map();

  @observable currentIssueType: IPageIssueTypeStoreIssueType = {} as IPageIssueTypeStoreIssueType;

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

  @action init(issueType: IPageIssueTypeStoreIssueType) {
    this.currentIssueType = issueType;
  }

  @action('清空全部数据') destroy() {
    this.clear();
    this.currentIssueType = {} as IPageIssueTypeStoreIssueType;
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
      if (index !== -1) {
        this.addFields.splice(index, 1);
        this.addUnselectedDataSet.splice(index, 1);
      }
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
      || this.getDescriptionObj.dirty || this.sortTableDataSet.dirty || this.createdFields.length > 0;
  }

  @computed get getDataStatusCode() {
    return this.dataStatusCode;
  }

  @action setCurrentIssueType(issueType: IPageIssueTypeStoreIssueType) {
    this.currentIssueType = issueType;
  }

  @computed get getCurrentIssueType() {
    return this.currentIssueType?.id;
  }

  @action loadCurrentTypeAllField = (data: Array<IFiledListItemProps>) => {
    data.forEach((item) => {
      this.currentTypeAllFieldData.set(item.id, item);
    });
  }

  loadData = () => {
    this.clear();
    this.addUnselectedDataSet.clear();
    this.setLoading(true);
    pageConfigApi.loadByIssueType(this.getCurrentIssueType).then((res) => {
      this.sortTableDataSet.loadData(res.fields.map((field) => ({
        ...field,
        showDefaultValueText: transformDefaultValue({ ...field, defaultValue: field.defaultValueObjs && field.defaultValueObjs.length > 0 ? field.defaultValue : undefined, fieldOptions: field.fieldOptions || field.defaultValueObjs || [field.defaultValueObj].filter((item) => item) }),
      })));
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
