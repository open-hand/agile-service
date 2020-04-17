import {
  observable, action, computed, toJS, set,
} from 'mobx';
import {
  stores, axios, Choerodon,
} from '@choerodon/boot';
import moment from 'moment';
import { getCustomFields } from '@/api/NewIssueApi';
import _ from 'lodash';

const { AppState } = stores;

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
      const reverseMyFilters = _.reverse(myFilters);
      this.setMyFilters(reverseMyFilters);
      this.setEditFilterInfo(_.map(_.map(reverseMyFilters, item => ({
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

  @observable chosenFields = new Map();

  @action handleChosenFieldChange = (select, field) => {
    const { code } = field;
    if (select) {
      this.chosenFields.set(code, observable({ ...field, value: undefined }));
    } else {
      this.chosenFields.delete(code);
    }
  }

  @action setFieldFilter = (code, value) => {
    const field = this.chosenFields.get(code);
    field.value = value;
  }

  @action setChosenFields(chosenFields) {
    this.chosenFields = chosenFields;
  }

  @action chooseAll() {
    this.fields.forEach((field) => {
      this.chosenFields.set(field.code, observable({ ...field, value: undefined }));
    });
  }

  @action unChooseAll() {
    this.chosenFields.clear();
  }

  getCustomFieldFilters() {
    const customField = {
      option: [],
      // date: [
      //   {
      //     fieldId: 294,
      //     startDate: '2020-04-16 10:48:28',
      //     endDate: '2020-04-18 13:48:28',
      //   },
      // ],
      // number: [
      //   {
      //     fieldId: 296,
      //     value: 3,
      //   },
      // ],
      // string: [
      //   {
      //     fieldId: 297,
      //     value: '1',
      //   },
      // ],
      // text: [
      //   {
      //     fieldId: 298,
      //     value: '123',
      //   },
      // ],
    };
    for (const [, field] of this.chosenFields) {
      const { fieldType, id } = field;
      const value = toJS(field.value);
      if (!value) {
        break;
      }
      switch (fieldType) {
        case 'single':
        case 'multiple':
        case 'radio':
          // eslint-disable-next-line no-case-declarations
          const v = Array.isArray(value) ? value : [value];
          if (v.length > 0) {
            customField.option.push({
              fieldId: id,
              value: v,
            });
          }
          break;      
        default: break;
      }
    }
    return customField;
  }
}

export default new IssueStore();
