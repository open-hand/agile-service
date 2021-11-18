import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { findIndex } from 'lodash';
import { observable, action, computed } from 'mobx';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import { DataSet } from 'choerodon-ui/pro';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import { Observer } from 'mobx-react-lite';
import { CheckBoxProps } from 'choerodon-ui/pro/lib/check-box/CheckBox';
import { OptionProps } from 'choerodon-ui/lib/select';
import { ITableColumnCheckBoxesDataProps, ITableColumnCheckBoxesOptionData } from '@/components/table-column-check-boxes';
import { IChosenFieldField, IChosenFieldFieldEvents } from '@/components/chose-field/types';

interface EventsProps extends IChosenFieldFieldEvents {
  loadRecordAxios?: (store: IssueExportStore) => Promise<any> /** 查询导出记录 */
  exportBefore?: (data: any) => any, /** 导出前对数据转换 */
  exportAxios?: (searchData: any, fieldSort: string | undefined) => Promise<any>, /** 导出 */
}
interface IDownLoadInfo {
  id: string | null,
  fileUrl: string | null,
  creationDate: string | null,
  lastUpdateDate: string | null,
}

interface IssueExportStoreProps {
  dataSetSystemFields?: FieldProps[], /** dataSet 数据管理 系统字段配置 */
  defaultCheckedExportFields?: string[], /** 默认选中导出字段 */
  defaultInitOptions?: (data: { options: Array<ITableColumnCheckBoxesOptionData>, checkedOptions: string[], dataSet: DataSet }) => Array<{ label: string, value: string, checkBoxProps?: CheckBoxProps }> | void /** 初始化选择字段的字段选项时调用 当返回空时则不进行选项覆盖 */
  defaultInitFieldAction?: (data: IChosenFieldField, store: IssueExportStore) => IChosenFieldField | false | undefined | void, /** 初始化字段时调用，当返回值不为IChosenFieldField 则跳过此字段 */
  defaultInitFieldFinishAction?: (data: { customFields: IChosenFieldField[], systemFields: IChosenFieldField[], currentChosenField: Map<string, IChosenFieldField> }, store: IssueExportStore) => void, /** 初始化字段完成时调用 */
  transformSystemFilter?: (data: any) => any, /** 提交数据前 对系统筛选字段数据转换 */
  transformExportFieldCodes?: (data: Array<string>, otherData: ITableColumnCheckBoxesDataProps) => Array<string>, /** 提交数据 对系统导出字段数据转换 */
  reverseTransformExportFieldCodes?: (data: Array<string>) => Array<string>
  /** webSocket message 的key  */
  wsMessageKey?: string
  events?: EventsProps,
  renderField?: (field: IChosenFieldField, otherComponentProps: Partial<SelectProps> | Partial<DatePickerProps>, { dataSet }: { dataSet: DataSet }) => React.ReactElement | false | null, /** 系统筛选字段项渲染 */
  extraFields?: IChosenFieldField[], /** 额外的筛选字段项  不在下拉菜单中 */
  exportButtonConfig?: { /** 导出按钮配置 */
    component?: React.ReactElement | ((exportEvent: (() => void)) => React.ReactElement),
    buttonProps?: Partial<ButtonProps>,
    buttonChildren?: any,
  }
  checkboxOptionsExtraConfig?: Map<string, { checkBoxProps?: CheckBoxProps, optionConfig: OptionProps, defaultChecked?: boolean }>
}
export { IssueExportStoreProps };
class IssueExportStore {
  dataSetSystemFields: FieldProps[] = [];

  transformSystemFilter: (data: any) => object;

  transformExportFieldCodes: (data: any, d2: any) => string[];

  reverseTransformExportFieldCodes: (data: string[]) => string[]

  events: EventsProps = {};

  defaultCheckedExportFields: string[] = [];

  defaultInitFieldAction: any;

  defaultInitFieldFinishAction: any;

  defaultExportBefore = (data: any) => data;

  defaultInitOptions = (data: any) => { };

  renderField: any;

  exportButtonConfig: IssueExportStoreProps['exportButtonConfig'];

  checkboxOptionsExtraConfig: IssueExportStoreProps['checkboxOptionsExtraConfig'];

  wsMessageKey?:string;

  @observable innerState = observable.map<string, any>();

  @observable extraFields: IChosenFieldField[];

  setDefaultCheckedExportFields(data: any) {
    this.defaultCheckedExportFields = data;
  }

  constructor(props?: IssueExportStoreProps) {
    this.events = props?.events || {};
    this.dataSetSystemFields = props?.dataSetSystemFields || [];
    this.transformSystemFilter = props?.transformSystemFilter || ((data) => data);
    this.transformExportFieldCodes = props?.transformExportFieldCodes || ((data) => data);
    this.reverseTransformExportFieldCodes = props?.reverseTransformExportFieldCodes || ((data) => data);
    this.defaultCheckedExportFields = props?.defaultCheckedExportFields || [];
    this.defaultInitFieldAction = props?.defaultInitFieldAction || ((data: IChosenFieldField, store: IssueExportStore) => data);
    this.defaultInitFieldFinishAction = props?.defaultInitFieldFinishAction || ((data: any) => data);
    this.defaultInitOptions = props?.defaultInitOptions || ((data: any) => { });
    this.extraFields = props?.extraFields || [];
    this.renderField = props?.renderField;
    this.exportButtonConfig = props?.exportButtonConfig || {};
    this.checkboxOptionsExtraConfig = props?.checkboxOptionsExtraConfig || new Map();
    this.wsMessageKey = props?.wsMessageKey;
  }

  @observable downloadInfo = {} as IDownLoadInfo;

  @observable exportBtnHidden: boolean = false;

  @observable currentChosenFields = observable.map<string, IChosenFieldField>();

  @action setState(key: string, value: any) {
    this.innerState.set(key, value);
  }

  getState(key: string) {
    return this.innerState.get(key);
  }

  @action
  setDownloadInfo(data: IDownLoadInfo) {
    this.downloadInfo = data;
  }

  @action addExtraField(data: IChosenFieldField) {
    const fieldIndex = findIndex(this.extraFields, { code: data.code });
    fieldIndex !== -1 ? this.extraFields.splice(fieldIndex, 1, data) : this.extraFields.push(data);
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
  initFieldFinish(customFields: IChosenFieldField[], systemFields: IChosenFieldField[], currentChosenField: Map<string, IChosenFieldField>): IChosenFieldField | false {
    const that = this;
    const { initFieldFinish = this.defaultInitFieldFinishAction } = this.events;
    return initFieldFinish({ customFields, systemFields, currentChosenField }, that);
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
  exportAxios(searchData: any, fieldSort: string | undefined): Promise<any> {
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
