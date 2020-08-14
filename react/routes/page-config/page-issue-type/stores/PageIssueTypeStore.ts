import {
  observable, action, runInAction, computed,
} from 'mobx';

import { PageConfigIssueType, pageConfigApi } from '@/api';
import { IFieldPostDataProps } from '../../components/create-field/CreateField';

export enum PageIssueTypeStoreStatusCode {
  update = 'update',
  drag = 'drag_update',
  desc = 'description_update',
  none = 'none',
  null = '',
}
interface IDescriptionTempleProps {
  id: string | undefined,
  template: undefined | string,
  objectVersionNumber: undefined | number,
}
export type PageIFieldPostDataProps = IFieldPostDataProps & { local?: boolean, fieldName: string,
   edited: boolean, created:boolean, required:boolean };
class PageIssueTypeStore {
  @observable loading: boolean = false;

  @observable allFieldData = observable.map();

  @observable currentIssueType: PageConfigIssueType = PageConfigIssueType.feature;

  @observable dataStatusCode: PageIssueTypeStoreStatusCode = PageIssueTypeStoreStatusCode.null;

  @observable deleteIds: Array<string> = [];

  @observable addIds: Array<string> = [];

  @observable createdFields: Array<PageIFieldPostDataProps> = [];

  @observable descriptionObj: IDescriptionTempleProps = {
    id: undefined,
    template: undefined,
    objectVersionNumber: undefined,
  };

  @action('清空全部数据') destroy() {
    this.currentIssueType = PageConfigIssueType.feature;
    this.dataStatusCode = PageIssueTypeStoreStatusCode.null;
    this.deleteIds.length = 0;
    this.descriptionObj = {
      id: undefined,
      template: undefined,
      objectVersionNumber: undefined,
    };
  }

  @action('清空编辑数据') clear() {
    this.dataStatusCode = PageIssueTypeStoreStatusCode.null;
    this.deleteIds.length = 0;
    this.descriptionObj = {
      id: undefined,
      template: undefined,
      objectVersionNumber: undefined,
    };
  }

  @action('增添删除字段') addDeleteId(id: string) {
    this.deleteIds.push(id);
  }

  @action('删除本地字段') deleteLocalField(code: string) {
    const index = this.createdFields.findIndex((item) => item.code === code);
    index !== -1 && this.createdFields.splice(index, 1);
  }

  @action('增添新字段') addNewField(data: PageIFieldPostDataProps) {
    this.createdFields.push(data);
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

  @computed get getDescriptionObj() {
    return this.descriptionObj;
  }

  @computed get getDeleteIds() {
    return this.deleteIds.slice();
  }

  @action setDataStatusCode(code: PageIssueTypeStoreStatusCode) {
    this.dataStatusCode = code;
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
}
// export { PageIssueTypeStore };
export default PageIssueTypeStore;
