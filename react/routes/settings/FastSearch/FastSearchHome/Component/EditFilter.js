import React, { Component } from 'react';
import {
  Modal, Form, Input, Select, Icon, Button, DatePicker, TimePicker,
} from 'choerodon-ui';
import { Content, stores, axios } from '@choerodon/boot';
import moment from 'moment';
import _ from 'lodash';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';
import {
  sprintApi, epicApi, featureApi, userApi, versionApi, fieldApi, issueLabelApi, priorityApi, statusApi, 
} from '@/api';
import { NumericInput } from '../../../../../components/CommonComponent';

const { Sidebar } = Modal;
const { TextArea } = Input;
const { Option } = Select;
const { AppState } = stores;
const FormItem = Form.Item;

let sign = -1;

const customFieldType = {
  radio: 'option',
  checkbox: 'option',
  time: 'date_hms',
  datetime: 'date',
  number: 'number',
  input: 'string',
  text: 'text',
  single: 'option',
  multiple: 'option',
  member: 'option',
  date: 'date',
};
class AddComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      origin: {},
      arr: [],
      o: [],
      originUsers: [],
      originStatus: [],
      originPriorities: [],
      originEpics: [],
      originSprints: [],
      originLabels: [],
      originComponents: [],
      originVersions: [],
      originTypes: [],
      loading: false,
      filters: [
        {
          prop: undefined,
          rule: undefined,
          value: undefined,
        },
      ],
      quickFilterFiled: [],
      deleteItem: [],
      deleteFlag: false,
      originFilterName: '',
    };
  }

  componentDidMount() {
    const { filterId } = this.props;
    this.loadQuickFilterFiled();
    this.loadQuickFilter();
    this.loadFilter(filterId);
  }

  getFilterGroup = (filter) => {
    /* eslint-disable */
    // [=, !=, in, notIn]
    const equal_notEqual_in_notin = new Set(['priority', 'issue_type', 'status']);
    // [=, !=, in, notIn, is, isNot]
    const equal_notEqual_in_notIn_is_isNot = new Set(['assignee', 'reporter', 'created_user', 'last_updated_user', 'epic', 'sprint', 'label', 'component', 'influence_version', 'fix_version', 'feature', 'member']);
    // [>, >=, <, <=]
    const greater_greaterAndEqual_lessThan_lessThanAndEqual = new Set(['last_update_date', 'creation_date', 'date', 'dateTime', 'time']);
    // [>, >=, <, <=, is, isNot]
    const greater_greaterAndEqual_lessThan_lessThanAndEqual_is_isNot_equal = new Set(['story_point', 'remain_time', 'number']);
    // [=, !=, is, isNot]
    const equal_notEqual_is_isNot = new Set(['single', 'radio']);
    // [in, notIn, is, isNot]
    const in_notIn_is_isNot = new Set(['multiple', 'checkbox']);
    //[=, !=, like, notLike]
    const equal_notEqual_like_notLike = new Set(['input', 'text']);
    /* eslint-enable */

    if (equal_notEqual_in_notin.has(filter)) {
      return 'is (=,!=,in,notin)';
    } else if (greater_greaterAndEqual_lessThan_lessThanAndEqual.has(filter)) {
      return 'is (>,>=,<,<=)';
    } else if (equal_notEqual_in_notIn_is_isNot.has(filter)) {
      return 'is (=,!=,in,notin,is,isNot)';
    } else if (greater_greaterAndEqual_lessThan_lessThanAndEqual_is_isNot_equal.has(filter)) {
      return 'is (>,>=,<,<=,is,isNot)';
    } else if (equal_notEqual_is_isNot.has(filter)) {
      return 'is (=, !=, is, isNot)';
    } else if (in_notIn_is_isNot.has(filter)) {
      return 'is (in, notIn, is, isNot)';
    } else if (equal_notEqual_like_notLike.has(filter)) {
      return 'is (=, !=, like, notLike)';
    }
    return null;
  };

  getOperation = (filter) => {
    const { quickFilterFiled } = this.state;
    const field = quickFilterFiled.find(item => item.fieldCode === filter) || {};
    const operationGroupBase = [
      [
        {
          value: '=',
          text: '等于',
        },
        {
          value: '!=',
          text: '不等于',
        },
        {
          value: 'in',
          text: '包含',
        },
        {
          value: 'notIn',
          text: '不包含',
        },
      ],
      [
        {
          value: '>',
          text: '大于',
        },
        {
          value: '>=',
          text: '大于或等于',
        },
        {
          value: '<',
          text: '小于',
        },
        {
          value: '<=',
          text: '小于或等于',
        },
      ],
    ];
    const operationGroupAdv = [
      [
        ...operationGroupBase[0],
        {
          value: 'is',
          text: '是',
        },
        {
          value: 'isNot',
          text: '不是',
        },
      ],
      [
        ...operationGroupBase[1],
        {
          value: 'is',
          text: '是',
        },
        {
          value: 'isNot',
          text: '不是',
        },
        {
          value: '=',
          text: '等于',
        },
      ],
      [
        {
          value: '=',
          text: '等于',
        },
        {
          value: '!=',
          text: '不等于',
        },
        {
          value: 'is',
          text: '是',
        },
        {
          value: 'isNot',
          text: '不是',
        },
      ],
      [
        {
          value: 'in',
          text: '包含',
        },
        {
          value: 'notIn',
          text: '不包含',
        },
        {
          value: 'is',
          text: '是',
        },
        {
          value: 'isNot',
          text: '不是',
        },
      ],
      [
        {
          value: '=',
          text: '等于',
        },
        {
          value: '!=',
          text: '不等于',
        },
        {
          value: 'like',
          text: '包含',
        },
        {
          value: 'notLike',
          text: '不包含',
        },
      ],
    ];

    switch (this.getFilterGroup(field.id ? field.type : filter)) {
      case 'is (=,!=,in,notin)':
        return operationGroupBase[0];
      case 'is (>,>=,<,<=)':
        return operationGroupBase[1];
      case 'is (=,!=,in,notin,is,isNot)':
        return operationGroupAdv[0];
      case 'is (>,>=,<,<=,is,isNot)':
        return operationGroupAdv[1];
      case 'is (=, !=, is, isNot)': 
        return operationGroupAdv[2];
      case 'is (in, notIn, is, isNot)': 
        return operationGroupAdv[3];
      case 'is (=, !=, like, notLike)': 
        return operationGroupAdv[4];
      default:
        return [];
    }
  };

  /**
   *校验快速搜索名称是否重复
   *
   * @memberof AddComponent
   */
  checkSearchNameRepeat = (rule, value, callback) => {
    const { originFilterName } = this.state;
    if (value && value.trim() && value.trim() !== originFilterName) {
      axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/check_name?quickFilterName=${value}`)
        .then((res) => {
          if (res) {
            callback('快速搜索名称重复');
          } else {
            callback();
          }
        });
    } else {
      callback();
    }
  };

  loadFilter = (id) => {
    const { filterId } = this.props;
    axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/${id || filterId}`)
      .then((res) => {
        if (res && res.description) {
          const description = res.description.split('+').slice(0, -3).join('+') || '';
          const obj = JSON.parse(res.description.split('+').slice(-1));
          this.setState({
            arr: this.transformInit(obj.arr || []),
            o: obj.o || [],
            origin: {
              ...res,
              description,
            },
            originFilterName: res.name,
          });
        }
      });
  };

  loadQuickFilterFiled = () => {
    const getPreDefinedField = () => axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/fields`);
    const getCustomField = () => fieldApi.getCustomFields();
    Promise.all([getPreDefinedField(), getCustomField()]).then(([preDefinedField, customField]) => {
      this.setState({
        quickFilterFiled: [...preDefinedField, ...IsInProgramStore.isInProgram ? [{ fieldCode: 'feature', type: 'long', name: '特性' }] : [], ...customField].map(field => ({ ...field, fieldCode: field.code || field.fieldCode, type: field.fieldType || field.type })) || [],
      });
    });
  };

  tempOption = (filter, addEmpty) => {
    const { state } = this;
    const projectId = AppState.currentMenuType.id;
    const orgId = AppState.currentMenuType.organizationId;
    const { quickFilterFiled } = state;
    const customFields = quickFilterFiled.filter(item => item.id);
    const customMemberField = quickFilterFiled.find(item => item.type === 'member') || {};
    const OPTION_FILTER = {
      assignee: {
        url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
        prop: 'list',
        id: 'id',
        name: 'realName',
        state: 'originUsers',
      },
      priority: {
        url: `/agile/v1/projects/${projectId}/priority/list_by_org`,
        prop: '',
        id: 'id',
        name: 'name',
        state: 'originPriorities',
      },
      status: {
        url: `/agile/v1/projects/${projectId}/schemes/query_status_by_project_id?apply_type=agile`,
        prop: '',
        id: 'id',
        name: 'name',
        state: 'originStatus',
      },
      reporter: {
        url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
        prop: 'list',
        id: 'id',
        name: 'realName',
        state: 'originUsers',
      },
      created_user: {
        url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
        prop: 'list',
        id: 'id',
        name: 'realName',
        state: 'originUsers',
      },
      last_updated_user: {
        url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
        prop: 'list',
        id: 'id',
        name: 'realName',
        state: 'originUsers',
      },
      epic: {
        url: `/agile/v1/projects/${projectId}/issues/epics/select_data`,
        prop: '',
        id: 'issueId',
        name: 'epicName',
        state: 'originEpics',
      },
      sprint: {
        // post
        url: `/agile/v1/projects/${projectId}/sprint/names`,
        prop: '',
        id: 'sprintId',
        name: 'sprintName',
        state: 'originSprints',
      },
      label: {
        url: `/agile/v1/projects/${projectId}/issue_labels`,
        prop: '',
        id: 'labelId',
        name: 'labelName',
        state: 'originLabels',
      },
      component: {
        url: `/agile/v1/projects/${projectId}/component`,
        prop: '',
        id: 'componentId',
        name: 'name',
        state: 'originComponents',
      },
      influence_version: {
        // post
        url: `/agile/v1/projects/${projectId}/product_version/names`,
        prop: '',
        id: 'versionId',
        name: 'name',
        state: 'originVersions',
      },
      fix_version: {
        // post
        url: `/agile/v1/projects/${projectId}/product_version/names`,
        prop: '',
        id: 'versionId',
        name: 'name',
        state: 'originVersions',
      },
      issue_type: {
        url: '',
        prop: '',
        id: 'typeCode',
        name: 'name',
        state: 'originTypes',
      },
      feature: {
        url: `/agile/v1/projects/${projectId}/issues/feature/all?organizationId=${orgId}&page=0&size=0&param=`,
        prop: '',
        id: 'issueId',
        name: 'summary',
        state: 'originFeatures',
      },
    };

    customFields.forEach((item) => {
      OPTION_FILTER[item.code] = {
        url: '',
        prop: '',
        id: 'id',
        name: 'value',
        state: `origin${item.code}`,
      };
    });

    OPTION_FILTER[customMemberField.code] = {
      url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
      prop: 'list',
      id: 'id',
      name: 'realName',
      state: 'originUsers',
    };
    const arr = (state[OPTION_FILTER[filter].state] || []).map(v => (
      <Option key={v[OPTION_FILTER[filter].id]} value={v[OPTION_FILTER[filter].id]}>
        {v[OPTION_FILTER[filter].name]}
      </Option>
    ));
   
    if (addEmpty) {
      arr.unshift(
        <Option key="null" value="null">
          无
        </Option>,
      );
    }
    return arr;
  };

  transformOperation = (value) => {
    const OPERATION = {
      '=': '=',
      '!=': '!=',
      in: 'in',
      'not in': 'notIn',
      is: 'is',
      'is not': 'isNot',
      '<': '<',
      '<=': '<=',
      '>': '>',
      '>=': '>=',
      like: 'like',
      'not like': 'notLike',
    };
    return OPERATION[value];
  };

  transformOperation2 = (value) => {
    const OPERATION = {
      '=': '=',
      '!=': '!=',
      in: 'in',
      notIn: 'not in',
      is: 'is',
      isNot: 'is not',
      '<': '<',
      '<=': '<=',
      '>': '>',
      '>=': '>=',
      like: 'like',
      notLike: 'not like',
    };
    return OPERATION[value];
  };

  getValue = (value, filter) => {
    const type = Object.prototype.toString.call(value);
    if (filter === 'priority') {
      if (type === '[object Array]') {
        const v = _.map(value, 'key');
        const vv = v.map(e => `${e}`);
        return `(${vv.join(',')})`;
      } else {
        const v = value.key;
        return `${v}`;
      }
    } else if (filter === 'issue_type') {
      if (type === '[object Array]') {
        const v = _.map(value, 'key');
        const vv = v.map(e => `'${e}'`);
        return `(${vv.join(',')})`;
      } else {
        const v = value.key;
        return `'${v}'`;
      }
    } else if (type === '[object Array]') {
      const v = _.map(value, 'key');
      return `(${v.join(',')})`;
    } else if (type === '[object Object]') {
      if (value.key) {
        const v = value.key;
        if (Object.prototype.toString.call(v) === '[object Number]') {
          return v;
        } else if (Object.prototype.toString.call(v) === '[object String]') {
          return v;
        }
      } else {
        return value.format('YYYY-MM-DD HH:mm:ss');
      }
    } else {
      return value;
    }
    return '';
  };

  getLabel = (value) => {
    if (Object.prototype.toString.call(value) === '[object Array]') {
      const v = _.map(value, 'label');
      return `[${v.join(',')}]`;
    } else if (Object.prototype.toString.call(value) === '[object Object]') {
      if (value.key) {
        const v = value.label;
        if (Object.prototype.toString.call(v) === '[object Number]') {
          return v;
        } else if (Object.prototype.toString.call(v) === '[object String]') {
          return v;
        }
      } else {
        return value.format('YYYY-MM-DD HH:mm:ss');
      }
    } else {
      return value;
    }
    return '';
  };

  transformInit(arr) {
    return arr.map((a, i) => ({
      fieldCode: a.fieldCode,
      operation: a.operation,
      value: this.transformInitialValue(i, a.fieldCode, a.operation, a.value),
    }));
  }

  transformInitialValue(index, filter, operation, value) {
    const { state } = this;
    const projectId = AppState.currentMenuType.id;
    const orgId = AppState.currentMenuType.organizationId;
    const { quickFilterFiled } = state;
    const customFields = quickFilterFiled.filter(item => item.id);
    const customMemberField = quickFilterFiled.find(item => item.type === 'member') || {};
    const OPTION_FILTER = {
      assignee: {
        url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
        prop: 'list',
        id: 'id',
        name: 'realName',
        state: 'originUsers',
      },
      priority: {
        url: `/agile/v1/projects/${projectId}/priority/list_by_org`,
        prop: '',
        id: 'id',
        name: 'name',
        state: 'originPriorities',
      },
      status: {
        url: `/agile/v1/projects/${projectId}/schemes/query_status_by_project_id?apply_type=agile`,
        prop: '',
        id: 'id',
        name: 'name',
        state: 'originStatus',
      },
      reporter: {
        url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
        prop: 'list',
        id: 'id',
        name: 'realName',
        state: 'originUsers',
      },
      created_user: {
        url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
        prop: 'list',
        id: 'id',
        name: 'realName',
        state: 'originUsers',
      },
      last_updated_user: {
        url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
        prop: 'list',
        id: 'id',
        name: 'realName',
        state: 'originUsers',
      },
      epic: {
        url: `/agile/v1/projects/${projectId}/issues/epics/select_data`,
        prop: '',
        id: 'issueId',
        name: 'epicName',
        state: 'originEpics',
      },
      sprint: {
        // post
        url: `/agile/v1/projects/${projectId}/sprint/names`,
        prop: '',
        id: 'sprintId',
        name: 'sprintName',
        state: 'originSprints',
      },
      label: {
        url: `/agile/v1/projects/${projectId}/issue_labels`,
        prop: '',
        id: 'labelId',
        name: 'labelName',
        state: 'originLabels',
      },
      component: {
        url: `/agile/v1/projects/${projectId}/component`,
        prop: '',
        id: 'componentId',
        name: 'name',
        state: 'originComponents',
      },
      influence_version: {
        // post
        url: `/agile/v1/projects/${projectId}/product_version/names`,
        prop: '',
        id: 'versionId',
        name: 'name',
        state: 'originVersions',
      },
      fix_version: {
        // post
        url: `/agile/v1/projects/${projectId}/product_version/names`,
        prop: '',
        id: 'versionId',
        name: 'name',
        state: 'originVersions',
      },
      issue_type: {
        url: '',
        prop: '',
        id: 'typeCode',
        name: 'name',
        state: 'originTypes',
      },
      feature: {
        url: `/agile/v1/projects/${projectId}/issues/feature/all?organizationId=${orgId}&page=0&size=0&param=`,
        prop: '',
        id: 'issueId',
        name: 'summary',
        state: 'originFeatures',
      },
    };

    customFields.forEach((item) => {
      OPTION_FILTER[item.code] = {
        url: '',
        prop: '',
        id: 'id',
        name: 'value',
        state: `origin${item.code}`,
      };
    });

    OPTION_FILTER[customMemberField.code] = {
      url: `/iam/choerodon/v1/projects/${AppState.currentMenuType.id}/users?page=1&size=0`,
      prop: 'list',
      id: 'id',
      name: 'realName',
      state: 'originUsers',
    };

    const field = quickFilterFiled.find(item => item.fieldCode === filter) || {};

    if (sign === index) {
      if (operation === 'in' || operation === 'notIn') {
        sign = -1;
        return [];
      } else {
        sign = -1;
        return undefined;
      }
    }
    if (filter === 'creation_date' || filter === 'last_update_date' || (field.id && field.type === 'datetime')) {
      // return moment
      return moment(value, 'YYYY-MM-DD HH:mm:ss');
    }
    if (field.id && field.type === 'date') {
      // return moment
      return moment(value, 'YYYY-MM-DD');
    }
    if (field.id && field.type === 'time') {
      return moment(value);
    }
    if (operation === 'is' || operation === 'isNot' || operation === 'is not') {
      return ({
        key: "'null'",
        label: '空',
      });
    }
    if (filter === 'story_point' || filter === 'remain_time' || (field.id && (field.type === 'number' || field.type === 'input' || field.type === 'text'))) {
      return value;
    }
    if (filter === 'priority') {
      if (operation === 'in' || operation === 'notIn' || operation === 'not in') {
        const arr = value.slice(1, -1).split(',');
        return arr.map((v) => {
          const priority = _.find(state[OPTION_FILTER[filter].state], { id: v * 1 });
          return {
            key: v * 1,
            label: priority ? priority.name : v,
          };
        });
      } else {
        const k = value;
        const priority = _.find(state[OPTION_FILTER[filter].state], { [OPTION_FILTER[filter].id]: k * 1 });
        return ({
          key: k,
          label: priority ? priority.name : k,
        });
      }
    } else if (filter === 'issue_type') {
      if (operation === 'in' || operation === 'notIn' || operation === 'not in') {
        const arr = value.slice(1, -1).split(',');
        return arr.map(v => ({
          key: v.slice(1, -1),
          label: _.find(state[OPTION_FILTER[filter].state],
            { [OPTION_FILTER[filter].id]: v.slice(1, -1) }).name,
        }));
      } else {
        const k = value.slice(1, -1);
        return ({
          key: k,
          label: (_.find(state[OPTION_FILTER[filter].state],
            { [OPTION_FILTER[filter].id]: k }) || {}).name,
        });
      }
    } else if (operation === 'in' || operation === 'notIn' || operation === 'not in') {
      const arr = value.slice(1, -1).split(',');
      return arr.map(v => ({
        key: v * 1,
        label: _.find(state[OPTION_FILTER[filter].state],
          { [OPTION_FILTER[filter].id]: v * 1 })
          ? _.find(state[OPTION_FILTER[filter].state],
            { [OPTION_FILTER[filter].id]: v * 1 })[OPTION_FILTER[filter].name]
          : undefined,
      }));
    } else if (operation === 'like' || operation === 'not like') {
      return value;
    } else {
      const k = value * 1;
      return ({
        key: k,
        label: _.find(state[OPTION_FILTER[filter].state],
          { [OPTION_FILTER[filter].id]: k })
          ? _.find(state[OPTION_FILTER[filter].state],
            { [OPTION_FILTER[filter].id]: k })[OPTION_FILTER[filter].name]
          : undefined,
      });
    }
  }

  handleOk(e) {
    e.preventDefault();
    const { form, onOk, filterId } = this.props;
    const {
      deleteItem, quickFilterFiled, origin, arr, deleteFlag,
    } = this.state;
    form.validateFieldsAndScroll((err, values, modify) => {
      if (!err && (modify || deleteFlag)) {
        const arrCopy = [];
        const expressQueryArr = [];
        const o = [];
        const f = arr.slice();
        f.forEach((v, i) => {
          if (deleteItem.indexOf(i) !== -1) {
            return;
          }
          const field = quickFilterFiled.find(item => item.fieldCode === v.fieldCode) || {};
          const a = {
            fieldCode: values[`filter-${i}-prop`],
            operation: this.transformOperation2(values[`filter-${i}-rule`]),
            value: this.getValue(values[`filter-${i}-value`], values[`filter-${i}-prop`]),
            predefined: !field.id,
            customFieldType: field.id ? customFieldType[field.type] : undefined,
          };
          if (i) {
            o.push(values[`filter-${i}-ao`]);
            expressQueryArr.push(values[`filter-${i}-ao`].toUpperCase());
          }
          arrCopy.push(a);
          expressQueryArr.push(_.find(quickFilterFiled,
            { fieldCode: a.fieldCode }).name);
          expressQueryArr.push(a.operation);
          expressQueryArr.push(this.getLabel(values[`filter-${i}-value`]));
        });
        const d = new Date();
        const json = JSON.stringify({
          arr: arrCopy,
          o,
        });
        const obj = {
          childIncluded: true,
          objectVersionNumber: origin.objectVersionNumber,
          expressQuery: expressQueryArr.join(' '),
          name: values.name.trim(),
          description: `${values.description || ''}+++${json}`,
          projectId: AppState.currentMenuType.id,
          quickFilterValueVOList: arrCopy,
          relationOperations: o,
        };
        this.setState({
          loading: true,
        });
        axios.put(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/${filterId}`, obj)
          .then((res) => {
            this.setState({
              loading: false,
            });
            onOk();
          });
      }
    });
  }

  loadQuickFilter() {
    const projectId = AppState.currentMenuType.id;
    userApi.getAllInProject().then(res => this.setState({ originUsers: res.list }));
    priorityApi.loadByProject().then(res => this.setState({ originPriorities: res }));
    statusApi.loadByProject().then(res => this.setState({ originStatus: res }));
    epicApi.loadEpicsForSelect().then(res => this.setState({ originEpics: res }));
    sprintApi.loadSprints().then(res => this.setState({ originSprints: res }));
    issueLabelApi.loads().then(res => this.setState({ originLabels: res }));
    axios.get(`/agile/v1/projects/${projectId}/component`).then(res => this.setState({ originComponents: res }));
    versionApi.loadNamesByStatus().then(res => this.setState({ originVersions: res }));
    axios.get(`/agile/v1/projects/${projectId}/schemes/query_issue_types?apply_type=agile`).then(res => this.setState({ originTypes: res }));
    featureApi.queryAllInSubProject([], undefined, 1, 0).then(res => this.setState({ originFeatures: res.content }));
    fieldApi.getCustomFields().then((res) => {
      const customFieldState = {};
      res.forEach((item) => {
        customFieldState[`origin${item.code}`] = item.fieldOptions || [];
      });
      this.setState(customFieldState);
    });
  }

  renderOperation(filter, index) {
    const { form } = this.props;
    if (!filter) {
      return (
        <Select label="关系" />
      );
    } else {
      return (
        <Select
          label="关系"
          onChange={(v) => {
            sign = index;
            const str = `filter-${index}-value`;
            let value;
            if (v === 'in' || v === 'notIn' || v === 'not in' || v === 'like' || v === 'notLike' || v === 'not like') {
              value = [];
            } else {
              value = undefined;
            }
            form.setFieldsValue({
              [str]: value,
            });
          }}
        >
          {
            this.getOperation(filter).map(v => (
              <Option key={v.value} value={v.value}>{v.text}</Option>
            ))
          }
        </Select>
      );
    }
  }

  renderValue(filter, opera) {
    const { quickFilterFiled } = this.state;
    const field = quickFilterFiled.find(item => item.fieldCode === filter) || {};
    let operation;
    if (opera === 'not in') {
      operation = 'notIn';
    } else if (opera === 'is not') {
      operation = 'isNot';
    } else {
      operation = opera;
    }
    if (!filter || !operation) {
      return (
        <Select label="值" />
      );
    } else if (['assignee', 'priority', 'status', 'reporter', 'created_user', 'last_update_user', 'epic', 'sprint', 'label', 'component', 'influence_version', 'fix_version', 'issue_type', 'feature'].indexOf(filter) > -1 || (field.id && (field.type === 'member' || field.type === 'single' || field.type === 'multiple' || field.type === 'radio' || field.type === 'checkbox'))) {
      // select
      if (['=', '!='].indexOf(operation) > -1) {
        // return normal value
        return (
          <Select
            label="值"
            labelInValue
            filter
            optionFilterProp="children"
            dropdownClassName="hidden-text hidden-label"
            filterOption={(input, option) => option.props.children.toLowerCase()
              .indexOf(input.toLowerCase()) >= 0}
          >
            {this.tempOption(filter, false)}
          </Select>
        );
      } else if (['is', 'isNot'].indexOf(operation) > -1) {
        // return value add empty
        return (
          <Select
            label="值"
            labelInValue
            filter
            optionFilterProp="children"
            dropdownClassName="hidden-text hidden-label"
            filterOption={(input, option) => option.props.children.toLowerCase()
              .indexOf(input.toLowerCase()) >= 0}
          >
            <Option key="'null'" value="'null'">
              空
            </Option>
          </Select>
        );
      } else {
        // return multiple value
        return (
          <Select
            label="值"
            labelInValue
            mode="multiple"
            filter
            optionFilterProp="children"
            dropdownClassName="hidden-text hidden-label"
            filterOption={(input, option) => option.props.children.toLowerCase()
              .indexOf(input.toLowerCase()) >= 0}
          >
            {this.tempOption(filter, false)}
          </Select>
        );
      }
    } else if (['creation_date', 'last_update_date'].indexOf(filter) > -1 || (field.id && field.type === 'datetime')) {
      // time
      // return data picker
      return (
        <DatePicker
          style={{ width: '100%' }}
          label="值"
          format="YYYY-MM-DD HH:mm:ss"
          showTime
        />
      );
    } else if (field.id && field.type === 'date') {
      return (
        <DatePicker
          style={{ width: '100%' }}
          label="值"
          format="YYYY-MM-DD"
        />
      );
    } else if (field.id && field.type === 'time') {
      return (
        <TimePicker
          style={{ width: '100%' }}
          label="值"
        />
      );
    } else if (field.id && field.type === 'input') {
      return (
        <Input
          style={{ width: '100%' }}
          label="值"
        />
      );
    } else if (field.id && field.type === 'text') {
      return (
        <TextArea
          style={{ width: '100%' }}
          label="值"
        />
      );
    } else {
      // story points && remainning time
      // return number input
      return !(operation === 'is' || operation === 'isNot') ? (
        <NumericInput
          label="值"
          style={{ lineHeight: '22px', marginBottom: 0, width: '100%' }}
        />
      ) : (
        <Select
          label="值"
          labelInValue
          filter
          optionFilterProp="children"
          dropdownClassName="hidden-text hidden-label"
          filterOption={(input, option) => option.props.children.toLowerCase()
            .indexOf(input.toLowerCase()) >= 0}
        >
          <Option key="'null'" value="'null'">
            空
          </Option>
        </Select>
      );
    }
  }

  render() {
    const { form, onCancel } = this.props;
    const {
      loading, origin, deleteItem, o, arr, quickFilterFiled,
    } = this.state;
    const { getFieldDecorator } = form;
    return (
      <Sidebar
        className="c7n-filter"
        title="修改快速筛选"
        okText="修改"
        cancelText="取消"
        visible={origin.filterId}
        confirmLoading={loading}
        onOk={this.handleOk.bind(this)}
        onCancel={onCancel}
        width={740}
      >
        <Form layout="vertical">
          <FormItem style={{ width: 520 }}>
            {getFieldDecorator('name', {
              rules: [{
                required: true,
                message: '名称必填',
                whitespace: true,
              }, {
                validator: this.checkSearchNameRepeat,
              }],
              initialValue: origin.name,
            })(
              <Input
                label="名称"
                maxLength={10}
              />,
            )}
          </FormItem>
          {
            arr.map((filter, index) => (
              <div key={index.toString()}>
                {
                  deleteItem.indexOf(index) === -1 && (
                    <div>
                      {
                        index !== 0 && (
                          <FormItem style={{
                            width: 80, display: 'inline-block', marginRight: 10,
                          }}
                          >
                            {getFieldDecorator(`filter-${index}-ao`, {
                              rules: [{
                                required: true,
                                message: '关系为必选字段',
                              }],
                              initialValue: o[index - 1],
                            })(
                              <Select label="关系">
                                <Option key="and" value="and">且</Option>
                                <Option key="or" value="or">或</Option>
                              </Select>,
                            )}
                          </FormItem>
                        )
                      }
                      <FormItem style={{
                        width: index === 0 ? 210 : 120, display: 'inline-block', marginRight: 10,
                      }}
                      >
                        {getFieldDecorator(`filter-${index}-prop`, {
                          rules: [{
                            required: true,
                            message: '属性不可为空',
                          }],
                          initialValue: arr[index].fieldCode,
                        })(
                          <Select
                            label="属性"
                            onChange={() => {
                              form.setFieldsValue({
                                [`filter-${index}-rule`]: undefined,
                                [`filter-${index}-value`]: undefined,
                              });
                            }}
                          >
                            {
                              quickFilterFiled.map(v => (
                                <Option key={v.fieldCode} value={v.fieldCode}>{v.name}</Option>
                              ))
                            }
                          </Select>,
                        )}
                      </FormItem>
                      <FormItem style={{
                        width: 100, display: 'inline-block', marginRight: 10,
                      }}
                      >
                        {getFieldDecorator(`filter-${index}-rule`, {
                          rules: [{
                            required: true,
                            message: '关系不可为空',
                          }],
                          initialValue: this.transformOperation(arr[index].operation),
                        })(
                          this.renderOperation(form.getFieldValue(`filter-${index}-prop`), index),
                        )}
                      </FormItem>
                      <FormItem style={{ width: 190, display: 'inline-block' }}>
                        {getFieldDecorator(`filter-${index}-value`, {
                          rules: [{
                            required: true,
                            message: '值不可为空',
                          }],
                          initialValue: arr[index].value,
                        })(
                          this.renderValue(form.getFieldValue(`filter-${index}-prop`), form.getFieldValue(`filter-${index}-rule`)),
                        )}
                      </FormItem>
                      <Button
                        style={{ margin: 10 }}
                        disabled={(arr.length - deleteItem.length) < 2}
                        shape="circle"
                        onClick={() => {
                          const arrCopy = deleteItem.slice();
                          arrCopy.push(index);
                          this.setState({
                            deleteItem: arrCopy,
                            deleteFlag: true,
                          });
                        }}
                      >
                        <Icon type="delete" />
                      </Button>
                    </div>
                  )
                }
              </div>
            ))
          }
          <Button
            style={{ margin: '-10px 0 10px' }}
            type="primary"
            funcType="flat"
            onClick={() => {
              const arrCopy = arr.slice();
              arrCopy.push({
                prop: undefined,
                rule: undefined,
                value: undefined,
              });
              this.setState({
                arr: arrCopy,
              });
            }}
          >
            <Icon type="add icon" />
            添加筛选
          </Button>
          <FormItem style={{ width: 520 }}>
            {getFieldDecorator('description', {
              initialValue: origin.description,
            })(
              <Input label="描述" autosize maxLength={30} />,
            )}
          </FormItem>
        </Form>
      </Sidebar>
    );
  }
}

export default Form.create()(AddComponent);
