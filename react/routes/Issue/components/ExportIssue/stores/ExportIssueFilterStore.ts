/* eslint-disable camelcase */
import {
  observable, action, runInAction, computed, ObservableMap, toJS,
} from 'mobx';
import { find } from 'lodash';
import { IExportSearch, ICustomFieldData } from '@/api';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { IExportIssueField, IExportIssueChosenField } from '../types';

function transformSystemFilter(data: any): Omit<IExportSearch, 'exportFieldCodes'> {
  const {
    issueTypeId,
    assigneeId,
    statusId,
    priorityId,
    issueIds,
    quickFilterIds,
    createDate = [],
    updateDate = [],
    contents,
    component,
    epic,
    feature,
    label,
    reporterIds,
    sprint,
    summary,
    version,
  } = data;
  return {
    advancedSearchArgs: {
      issueTypeId,
      reporterIds,
      statusId,
      priorityId,
    },
    otherArgs: {
      assigneeId,
      issueIds,
      component,
      epic,
      feature,
      label,
      sprint,
      summary,
      version,
    },
    searchArgs: {
      createStartDate: createDate[0],
      createEndDate: createDate[1],
      updateStartDate: updateDate[0],
      updateEndDate: updateDate[1],
    },
    quickFilterIds,
    contents,
  };
}
class ExportIssueFilterStore {
  constructor(props: { fields: Array<IExportIssueField>, chosenFields: Array<IExportIssueChosenField> }) {
    this.fields = observable.map([['custom', []], ['system', []]]);
    props.fields.forEach((field) => {
      if (field.id) {
        // 冲刺特殊处理
        const newField = field.code === 'sprint' ? { ...field, immutableCheck: true } : field;
        this.fields.get('custom')?.push(newField);
      } else if (!field.noDisplay) {
        this.fields.get('system')?.push(field);
      }
    });
    props.chosenFields.forEach((field) => {
      if (typeof (field.value) !== 'undefined') {
        console.log('chosenFields', field);
        this.chosenFields.set(field.code, { ...field, value: toJS(field.value) });
      }
    });
  }

  @observable fields: ObservableMap<'system' | 'custom', Array<IExportIssueField>>;;

  @observable chosenFields: ObservableMap<string, IExportIssueChosenField> = observable.map();

  @observable currentOptionStatus: 'ALL' | 'PART' | 'NONE' = 'NONE';

  @observable searchVal?: string;

  @action setSearchVal(data: string) {
    this.searchVal = data;
  }

  @computed get getSearchVal() {
    return this.searchVal;
  }

  getChosenByCode(code: string) {
    return this.chosenFields.get(code);
  }

  @computed get getCurrentOptionStatus() {
    return this.currentOptionStatus;
  }

  @computed get getChosenFieldsArr() {
    return [...this.chosenFields.values()];
  }

  getCustomFieldFilters = (record: Record) => {
    const customField: ICustomFieldData = {
      option: [],
      date: [],
      date_hms: [],
      number: [],
      string: [],
      text: [],
    };
    const systemFilter = {} as any;
    for (const chosen of this.chosenFields) {
      const [code, chosenField] = chosen as [string, IExportIssueChosenField];
      const { fieldType, id } = chosenField;
      const value = toJS(record.get(code));
      console.log('value', value);
      if (value === undefined || value === null || value === '') {
        // eslint-disable-next-line no-continue
        continue;
      }
      // 系统字段
      if (!id) {
        systemFilter[code] = value;
        // eslint-disable-next-line no-continue
        continue;
      }
      switch (fieldType) {
        case 'single':
        case 'multiple':
        case 'radio':
        case 'checkbox':
        case 'member': {
          const v = Array.isArray(value) ? value : [value];
          if (v.length > 0) {
            customField.option.push({
              fieldId: id,
              value: v,
            });
          }
          break;
        }
        case 'input': {
          if (value && value.length > 0) {
            customField.string.push({
              fieldId: id,
              value,
            });
          }
          break;
        }
        case 'text': {
          if (value && value.length > 0) {
            customField.text.push({
              fieldId: id,
              value,
            });
          }
          break;
        }
        case 'number': {
          customField.number.push({
            fieldId: id,
            value,
          });
          break;
        }
        case 'time':
        case 'datetime':
        case 'date': {
          if (value && value.length > 0) {
            if (fieldType === 'time') {
              customField.date_hms.push({
                fieldId: id,
                startDate: value[0],
                endDate: value[1],
              });
            } else {
              customField.date.push({
                fieldId: id,
                startDate: value[0],
                endDate: value[1],
              });
            }
          }
          break;
        }
        default: break;
      }
    }
    if (record.get('sprint')) {
      systemFilter.sprint = record.get('sprint');
    }

    const filter = transformSystemFilter(systemFilter);
    filter.otherArgs.customField = customField;
    return filter;
  }

  getExportFieldCodes = (record: Record) => {
    const fieldTransform = {
      issueNum: 'issueNum',
      issueId: 'summary',
      //  "description":
      issueTypeId: 'typeName',
      //  "projectName":
      assigneeId: 'assigneeName',
      // "assigneeRealName":
      reporterId: 'reporterName',
      //  "reporterRealName":
      //   "resolution":
      statusId: 'statusName',
      issueSprintVOS: 'sprintName',
      // "creationDate":
      lastUpdateDate: 'lastUpdateDate',
      priorityId: 'priorityName',
      //  "subTask":
      //  "remainingTime":
      version: 'versionName',
      epic: 'epicName',
      label: 'labelName',
      storyPoints: 'storyPoints',
      component: 'componentName',
    };
    return {
      // @ts-ignore
      exportFieldCodes: record.get('exportFieldCodes').map((code: string) => fieldTransform[code] || code),
    };
  };

  filterFieldBySearchVal = (field: IExportIssueField | IExportIssueChosenField) => field.name.indexOf(this.searchVal!) > -1

  handleFilterChange = (code: string, value: any) => {
    this.setFieldFilter(code, value);
  }

  @action setFieldFilter = (code: string, value: any) => {
    const field = this.chosenFields.get(code);
    // 说明这时候没有被选择，那么要自动选上
    if (!field) {
      const unSelectField = find([...this.getAllFields], { code });
      if (unSelectField) {
        this.chosenFields.set(code, observable({ ...unSelectField, value }));
      }
    } else {
      field.value = value;
    }
  }

  @computed get getFields() {
    if (this.searchVal && this.searchVal !== '') {
      return [this.fields.get('system')!.filter(this.filterFieldBySearchVal), this.fields.get('custom')!.filter(this.filterFieldBySearchVal)];
    }
    return [this.fields.get('system')!, this.fields.get('custom')!];
  }

  @computed get getAllFields() {
    return [...this.fields.get('system')!, ...this.fields.get('custom')!];
  }

  @action('增添选择字段') addChosenFields(key: string, data: IExportIssueChosenField | IExportIssueField) {
    this.chosenFields.set(key, { ...data, value: undefined });
  }

  @action('删除选择字段') delChosenFields(key: string) {
    this.chosenFields.delete(key);
  }

  @action('增添全部字段') addAllChosenFields() {
    //   this.chosenFields.set(key, data);
    const [systemFiled, customFiled] = this.getFields;
    [...systemFiled, ...customFiled].forEach((field) => {
      if (!this.chosenFields.has(field.code)) {
        this.chosenFields.set(field.code, { ...field, value: undefined });
      }
    });
    this.currentOptionStatus = 'ALL';
  }

  @action('取消选择全部字段') cancelAllChosenFields() {
    //   this.chosenFields.set(key, data);
    this.chosenFields.clear();
    this.currentOptionStatus = 'NONE';
  }
}
export default ExportIssueFilterStore;
