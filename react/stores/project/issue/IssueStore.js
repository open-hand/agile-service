import {
  observable, action, computed, toJS,
} from 'mobx';
import {
  stores, axios, Choerodon,
} from '@choerodon/boot';
import { getCustomFields } from '@/api/NewIssueApi';
import {
  debounce, reverse, map, find, isEmpty,
} from 'lodash';

const systemFields = [{
  code: 'issueIds',
  name: 'issueId',
  defaultShow: true,
  noDisplay: true, // 不需要展示，仅作为一个筛选项
}, {
  code: 'quickFilterIds',
  name: '快速筛选',
  defaultShow: true,
  noDisplay: true,
}, {
  code: 'contents',
  name: '概要',
  defaultShow: true,
  noDisplay: true,
}, {
  code: 'issueTypeId',
  name: '问题类型',
  defaultShow: false,
}, {
  code: 'statusId',
  name: '状态',
  defaultShow: true,
}, {
  code: 'assigneeId',
  name: '经办人',
  defaultShow: true,
}, {
  code: 'reporterIds',
  name: '报告人',
  defaultShow: false,
}, {
  code: 'sprint',
  name: '冲刺',
  defaultShow: true,
}, {
  code: 'component',
  name: '模块',
  defaultShow: false,
}, {
  code: 'version',
  name: '版本',
  defaultShow: false,
}, {
  code: 'createDate',
  name: '创建时间',
  defaultShow: false,
}];
const { AppState } = stores;
function transformSystemFilter(data) {
  const {
    issueTypeId,
    assigneeId,
    statusId,
    issueIds,
    quickFilterIds,
    createDate = [],
    contents,
    component,
    epic,
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
    },
    otherArgs: {
      assigneeId,
      issueIds,
      component,
      epic,
      label,
      sprint,
      summary,
      version,
    },
    searchArgs: {
      createStartDate: createDate[0],
      createEndDate: createDate[1],
    },
    quickFilterIds,
    contents,
  };
}

class IssueStore {
  // 当前加载状态
  @observable loading = true;

  // 创建问题窗口是否展开
  @observable createFlag = false;

  // 问题详情是否展开
  @observable expand = false;

  // 当前选中 Issue 详细信息
  @observable selectedIssue = {};

  // 筛选列表是否显示
  @observable filterListVisible = false;

  @observable systemFields = systemFields;

  @computed get getFilterListVisible() {
    return this.filterListVisible;
  }

  @action setFilterListVisible(data) {
    this.filterListVisible = data;
  }

  @observable updateFilterName = '';

  @computed get getUpdateFilterName() {
    return this.updateFilterName;
  }

  @action setUpdateFilterName(data) {
    this.updateFilterName = data;
  }

  // 控制保存模态框是否显示
  @observable saveFilterVisible = false;

  @computed get getSaveFilterVisible() {
    return this.saveFilterVisible;
  }

  @action setSaveFilterVisible(data) {
    this.saveFilterVisible = data;
  }

  // 控制导出模态框是否显示
  @observable exportModalVisible = false;

  @computed get getExportModalVisible() {
    return this.exportModalVisible;
  }

  @action setExportModalVisible(visible) {
    this.exportModalVisible = visible;
  }

  // 我的筛选列表
  @observable myFilters = [];

  @computed get getMyFilters() {
    return toJS(this.myFilters);
  }

  @action setMyFilters(data) {
    this.myFilters = data;
  }

  @observable projectInfo = {};

  @computed get getProjectInfo() {
    return toJS(this.projectInfo);
  }

  @action setProjectInfo(data) {
    this.projectInfo = data;
  }

  @observable selectedMyFilterInfo = {};

  @computed get getSelectedMyFilterInfo() {
    return this.selectedMyFilterInfo;
  }

  @observable editFilterInfo = [];

  @computed get getEditFilterInfo() {
    return toJS(this.editFilterInfo);
  }

  @action setEditFilterInfo(data) {
    this.editFilterInfo = data;
  }

  @action setLoading(data) {
    this.loading = data;
  }

  @computed get getLoading() {
    return toJS(this.loading);
  }

  @action createQuestion(data) {
    this.createFlag = data;
  }

  @computed get getCreateQuestion() {
    return this.createFlag;
  }

  @action setClickedRow(data) {
    this.selectedIssue = data.selectedIssue;
    this.expand = data.expand;
  }

  @computed get getSelectedIssue() {
    return toJS(this.selectedIssue);
  }

  @action setSelectedIssue(data) {
    this.selectedIssue = data;
  }

  @computed get getExpand() {
    return toJS(this.expand);
  }


  axiosGetMyFilterList = () => {
    const { userInfo: { id } } = AppState;
    this.setLoading(true);
    return axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/personal_filter/query_all/${id}`).then((myFilters) => {
      this.setLoading(false);
      const reverseMyFilters = reverse(myFilters);
      this.setMyFilters(reverseMyFilters);
      this.setEditFilterInfo(map(map(reverseMyFilters, item => ({
        filterId: item.filterId,
      })), (item, index) => ({
        ...item,
        isEditing: false,
        isEditingIndex: index,
      })));
    }).catch(() => {
      this.setLoading(false);
      Choerodon.prompt('获取我的筛选列表失败');
    });
  };

  @observable fields = []

  async loadCustomFields() {
    const fields = await getCustomFields();
    this.setFields(fields);
  }

  @action setFields(fields) {
    this.fields = fields;
  }

  @observable chosenFields = new Map(systemFields.filter(f => f.defaultShow).map(f => ([f.code, observable({ ...f, value: undefined })])));

  @action handleChosenFieldChange = (select, field) => {
    const { code } = field;
    if (select) {
      this.chosenFields.set(code, observable({ ...field, value: undefined }));
    } else {
      const { value } = this.chosenFields.get(code);
      this.chosenFields.delete(code);
      if (!isEmpty(value)) {
        this.query();
      }
    }
  }

  getFilterValueByCode(code) {
    return this.chosenFields.get(code) ? toJS(this.chosenFields.get(code).value) : undefined;
  }

  query = debounce(() => {
    this.dataSet.query();
  }, 300);

  handleFilterChange = (code, value) => {
    this.setFieldFilter(code, value);
    this.query();
  }

  @action setFieldFilter = (code, value) => {
    const field = this.chosenFields.get(code);
    // 说明这时候没有被选择，那么要自动选上
    if (!field) {
      const unSelectField = find([...this.fields, ...systemFields], { code });
      if (unSelectField) {
        this.chosenFields.set(code, observable({ ...unSelectField, value }));
      }
    } else {
      field.value = value;
    }
  }

  @action setChosenFields(chosenFields) {
    this.chosenFields = chosenFields;
  }

  @action chooseAll() {
    [...systemFields, ...this.fields].forEach((field) => {
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
    this.chosenFields = new Map(systemFields.filter(f => f.defaultShow).map(f => ([f.code, this.chosenFields.get(f.code)])));
    // 取消全选之前如果有筛选就查一次
    if (hasValue) {
      this.query();
    }
  }

  @action
  clearAllFilter() {
    for (const [, field] of this.chosenFields) {
      if (field.value) {
        field.value = undefined;
      }
    }
  }

  getCustomFieldFilters = () => {
    const customField = {
      option: [],
      date: [],
      date_hms: [],
      number: [],
      string: [],
      text: [],
    };
    const systemFilter = {};
    for (const [code, field] of this.chosenFields) {
      const { fieldType, id } = field;
      const value = toJS(field.value);
      if (value === undefined || value === null || value === '') {
        // eslint-disable-next-line no-continue
        continue;
      }
      // 系统字段
      if (!id) {
        systemFilter[code] = value;
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
    const filter = transformSystemFilter(systemFilter);
    filter.otherArgs.customField = customField;
    // console.log(filter);
    return filter;
  }

  getFieldCodeById(id) {
    const field = find(this.fields, { id: Number(id) });
    return field ? field.code : undefined;
  }
}

export default new IssueStore();
