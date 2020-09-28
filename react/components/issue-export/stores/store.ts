import { IChosenFieldField, IChosenFieldFieldEvents } from '@/components/chose-field/types';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { useLocalStore } from 'mobx-react-lite';
import { observable, action, computed } from 'mobx';

interface EventsProps extends IChosenFieldFieldEvents {
  loadRecordAxios?: (store: IssueExportStore) => Promise<any>
  exportBefore?: (data: any) => void | boolean,
  exportAxios?: (searchData: any, fieldSort: string) => Promise<any>,
}
interface IDownLoadInfo {
  id: string | null,
  fileUrl: string | null,
  creationDate: string | null,
  lastUpdateDate: string | null,
}

interface Props {
  dataSetSystemFields?: FieldProps[],
  defaultCheckedExportFields?: string[],
  defaultInitFieldAction?: (data: IChosenFieldField) => IChosenFieldField | false | undefined | void,
  transformSystemFilter?: (data: any) => any,
  transformExportFieldCodes?: (data: Array<string>) => Array<string>,
  events?: EventsProps,
}
class IssueExportStore {
  dataSetSystemFields: FieldProps[] = [];

  transformSystemFilter: (data: any) => object;

  transformExportFieldCodes: (data: any) => object;

  events: EventsProps = {};

  defaultCheckedExportFields: string[] = [];

  defaultInitFieldAction: any;

  defaultExportBefore = (data: any) => data;

  setDefaultCheckedExportFields(data: any) {
    this.defaultCheckedExportFields = data;
  }

  constructor(props?: Props) {
    this.events = props?.events || {};
    this.dataSetSystemFields = props?.dataSetSystemFields || [];
    this.transformSystemFilter = props?.transformSystemFilter || ((data) => data);
    this.transformExportFieldCodes = props?.transformExportFieldCodes || ((data) => data);
    this.defaultCheckedExportFields = props?.defaultCheckedExportFields || [];
    this.defaultInitFieldAction = props?.defaultInitFieldAction || ((data: IChosenFieldField) => data);
  }

  @observable downloadInfo = {} as IDownLoadInfo;

  @observable currentChosenFields = observable.map<string, IChosenFieldField>();

  @action
  setDownloadInfo(data: IDownLoadInfo) {
    this.downloadInfo = data;
  }

  @computed get getDownloadInfo() {
    return this.downloadInfo;
  }

  @action setDefaultCurrentChosenFields(dataArr: IChosenFieldField[]) {
    console.log('this.currentChosenFields', [...this.currentChosenFields.values()]);
    dataArr.forEach((v) => {
      if (this.currentChosenFields.has(v.code)) {
        console.log('v......0', v);
        this.currentChosenFields.set(v.code, { ...this.currentChosenFields.get(v.code)!, value: v.code });
      } else {
        this.currentChosenFields.set(v.code, v);
      }
    });
    this.currentChosenFields = observable.map(dataArr.map((v) => [v.code, v]));
  }

  @computed get getCurrentChosenFieldsArr() {
    return [...this.currentChosenFields.values()];
  }

  @action
  initField(field: IChosenFieldField): IChosenFieldField | false {
    const { initField = this.defaultInitFieldAction } = this.events;
    return initField(field);
  }

  @action
  initChosenField(field: IChosenFieldField): IChosenFieldField | false {
    const { initChosenField = this.defaultInitFieldAction } = this.events;
    return initChosenField(field);
  }

  @action
  exportBefore<T>(data: T): T {
    const { exportBefore = this.defaultExportBefore } = this.events;
    return exportBefore(data);
  }

  @action
  exportAxios(searchData: any, fieldSort: string): Promise<any> {
    const { exportAxios = (v1: any, v2: any) => new Promise(() => null) } = this.events;
    return exportAxios(searchData, fieldSort);
  }

  @action
  loadRecordAxios(store: IssueExportStore) {
    const { loadRecordAxios = (v1: any) => new Promise(() => null) } = this.events;
    return loadRecordAxios(store);
  }
}
export default IssueExportStore;
// type IssueExportStore = ReturnType<typeof useIssueExportStore>;
