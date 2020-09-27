import { IChosenFieldField, IChosenFieldFieldActions } from '@/components/chose-field/types';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { useLocalStore } from 'mobx-react-lite';
import { observable, action, computed } from 'mobx';

interface ActionProps extends IChosenFieldFieldActions {
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
  actions?: ActionProps,
}
class IssueExportStore {
  dataSetSystemFields: FieldProps[] = [];

  transformSystemFilter: (data: any) => object;

  transformExportFieldCodes: (data: any) => object;

  actions: ActionProps = {};

  defaultCheckedExportFields: string[] = [];

  defaultInitFieldAction: any;

  defaultExportBefore = (data: any) => data;

  setDefaultCheckedExportFields(data: any) {
    this.defaultCheckedExportFields = data;
  }

  constructor(props?: Props) {
    this.actions = props?.actions || {};
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

  @computed get getCurrentChosenFieldsArr() {
    return [...this.currentChosenFields.values()];
  }

  @action
  initField(field: IChosenFieldField): IChosenFieldField | false {
    const { initField = this.defaultInitFieldAction } = this.actions;
    return initField(field);
  }

  @action
  initChosenField(field: IChosenFieldField): IChosenFieldField | false {
    const { initChosenField = this.defaultInitFieldAction } = this.actions;
    return initChosenField(field);
  }

  @action
  exportBefore<T>(data: T): T {
    const { exportBefore = this.defaultExportBefore } = this.actions;
    return exportBefore(data);
  }

  @action
  exportAxios(searchData: any, fieldSort: string): Promise<any> {
    const { exportAxios = (v1: any, v2: any) => new Promise(() => null) } = this.actions;
    return exportAxios(searchData, fieldSort);
  }

  @action
  loadRecordAxios(store: IssueExportStore) {
    const { loadRecordAxios = (v1: any) => new Promise(() => null) } = this.actions;
    return loadRecordAxios(store);
  }
}
export default IssueExportStore;
// type IssueExportStore = ReturnType<typeof useIssueExportStore>;
