import { IChosenFieldField, IChosenFieldFieldEvents } from '@/components/chose-field/types';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { findIndex } from 'lodash';
import { useLocalStore } from 'mobx-react-lite';
import { observable, action, computed } from 'mobx';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import { DataSet } from 'choerodon-ui/pro/lib';

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
  defaultInitFieldAction?: (data: IChosenFieldField, store: IssueExportStore) => IChosenFieldField | false | undefined | void,
  transformSystemFilter?: (data: any) => any,
  transformExportFieldCodes?: (data: Array<string>) => Array<string>,
  events?: EventsProps,
  renderField?: (field: IChosenFieldField, otherComponentProps: Partial<SelectProps> | Partial<DatePickerProps>, { dataSet }: { dataSet: DataSet }) => React.ReactElement | false | null,
  extraFields?: IChosenFieldField[],
}
class IssueExportStore {
  dataSetSystemFields: FieldProps[] = [];

  transformSystemFilter: (data: any) => object;

  transformExportFieldCodes: (data: any) => object;

  events: EventsProps = {};

  defaultCheckedExportFields: string[] = [];

  defaultInitFieldAction: any;

  defaultExportBefore = (data: any) => data;

  renderField: any;

  @observable extraFields: IChosenFieldField[];

  setDefaultCheckedExportFields(data: any) {
    this.defaultCheckedExportFields = data;
  }

  constructor(props?: Props) {
    this.events = props?.events || {};
    this.dataSetSystemFields = props?.dataSetSystemFields || [];
    this.transformSystemFilter = props?.transformSystemFilter || ((data) => data);
    this.transformExportFieldCodes = props?.transformExportFieldCodes || ((data) => data);
    this.defaultCheckedExportFields = props?.defaultCheckedExportFields || [];
    this.defaultInitFieldAction = props?.defaultInitFieldAction || ((data: IChosenFieldField, store: IssueExportStore) => data);
    this.extraFields = props?.extraFields || [];
    this.renderField = props?.renderField;
  }

  @observable downloadInfo = {} as IDownLoadInfo;

  @observable exportBtnHidden: boolean = false;

  @observable currentChosenFields = observable.map<string, IChosenFieldField>();

  @action
  setDownloadInfo(data: IDownLoadInfo) {
    this.downloadInfo = data;
  }

  @action addExtraField(data: IChosenFieldField) {
    const fieldIndex = findIndex(this.extraFields, { code: data.code });
    fieldIndex && this.extraFields.splice(fieldIndex, 1, data);
  }

  @computed get getExtraFields() {
    return this.extraFields.slice();
  }

  @action
  setExportBtnHidden(data: boolean) {
    this.exportBtnHidden = data;
  }

  @computed get getDownloadInfo() {
    return this.downloadInfo;
  }

  @action setDefaultCurrentChosenFields(dataArr: IChosenFieldField[]) {
    dataArr.forEach((v) => {
      if (this.currentChosenFields.has(v.code)) {
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
    const that = this;
    const { initField = this.defaultInitFieldAction } = this.events;
    return initField(field, that);
  }

  @action
  initChosenField(field: IChosenFieldField): IChosenFieldField | false {
    const that = this;
    const { initChosenField = this.defaultInitFieldAction } = this.events;
    return initChosenField(field, that);
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
