import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { Choerodon } from '@choerodon/boot';
import { Select } from 'choerodon-ui';
import { uniqBy } from 'lodash';
import EventEmitter from 'wolfy87-eventemitter';
import {
  sprintApi, quickFilterApi, userApi, personalFilterApi,
} from '@/api';
import './QuickSearch.less';
import { getSelectStyle } from '@/components/issue-search/custom-fields/utils';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import BacklogStore from '../../stores/project/backlog/BacklogStore';
import ScrumBoardStore from '../../stores/project/scrumBoard/ScrumBoardStore';

const { Option, OptGroup } = Select;
const QuickSearchEvent = new EventEmitter();
@inject('AppState')
@observer
class QuickSearch extends Component {
  constructor(props) {
    super(props);
    const scrumboardInitValue = localPageCacheStore.getItem('scrumboard');
    this.defaultQuickFilterValue = [];
    this.defaultSelectUsersValue = [];
    if (scrumboardInitValue) {
      const {
        onlyMeChecked, onlyStoryChecked, moreChecked, personalFilters, assigneeFilter, selectSprint,
      } = scrumboardInitValue;
      onlyMeChecked && this.defaultQuickFilterValue.push({
        key: -1,
      });
      selectSprint && ScrumBoardStore.setSelectSprint(selectSprint);
      onlyStoryChecked && this.defaultQuickFilterValue.push({ key: -2 });
      moreChecked && this.defaultQuickFilterValue.push(...moreChecked.map((item) => ({ key: item })));
      personalFilters && this.defaultQuickFilterValue.push(...personalFilters.map((item) => ({ key: `personal%${item}` })));
      assigneeFilter && this.defaultSelectUsersValue.push(...assigneeFilter.map((item) => ({ ...item, id: item.key, realName: item.label })));
    }
    this.state = {
      userDataArray: [],
      quickSearchArray: [],
      selectQuickSearch: this.defaultQuickFilterValue,
      selectUsers: this.defaultSelectUsersValue,
      personalFilter: [],
    };
  }

  /**
 * DidMount =>
 * 1. 请求快速搜索数据
 * 2. 请求项目经办人信息
 */
  componentDidMount() {
    QuickSearchEvent.addListener('clearQuickSearchSelect', this.clearQuickSearch);
    QuickSearchEvent.addListener('setSelectQuickSearch', this.setSelectQuickSearch);
    QuickSearchEvent.addListener('unSelectStory', this.unSelectStory);
    const axiosGetFilter = quickFilterApi.loadAll();
    const axiosGetUser = userApi.getAllInProject(undefined, undefined, undefined, 40);
    const axiosGetSprintNotClosed = sprintApi.loadSprints(['sprint_planning', 'started']);
    const axiosGetPersonalFilter = personalFilterApi.loadAll();
    Promise.all([axiosGetFilter,
      axiosGetUser,
      axiosGetSprintNotClosed,
      axiosGetPersonalFilter]).then((res = []) => {
      const resFilterData = res[0].map((item) => ({
        label: item.name,
        value: item.filterId,
      }));
        // 非停用角色
      let resUserData = res[1].list.filter((item) => item.enabled).map((item) => ({
        id: item.id,
        realName: item.realName,
      }));
      resUserData.push(...this.defaultSelectUsersValue);
      resUserData = uniqBy(resUserData, 'id');
      const resSprintData = res[2];
      const personalFilter = res[3];

      this.setState({
        userDataArray: resUserData,
        quickSearchArray: resFilterData,
        personalFilter,
      });
      ScrumBoardStore.setSprintNotClosedArray(resSprintData);
    }).catch((error) => {
      Choerodon.prompt(error);
    });
  }

  componentWillUnmount() {
    QuickSearchEvent.removeListener('clearQuickSearchSelect', this.clearQuickSearch);
    QuickSearchEvent.removeListener('setSelectQuickSearch', this.setSelectQuickSearch);
    QuickSearchEvent.removeListener('unSelectStory', this.unSelectStory);
    this.setState({
      userDataArray: [],
      quickSearchArray: [],
      selectQuickSearch: [],
    });
    ScrumBoardStore.setSprintNotClosedArray([]);
  }

  /**
   *
   * @param value（Array） => 选中的快速搜索 ID 组成的数组
   * @props onQuickSearchChange
   */
  handleQuickSearchChange = (value, option) => {
    const { onQuickSearchChange } = this.props;
    const flattenValue = value.map((item) => item.key);
    const otherSearchId = flattenValue.filter((item) => !(item === -1 || item === -2 || item === -3) && String(item).split('%').length === 1);
    const personalFilters = flattenValue.filter((item) => String(item).split('%')[0] === 'personal').map((item) => item.split('%')[1]);
    this.setState({
      selectQuickSearch: value,
    });
    // -1 仅我的问题
    // -2 仅故事
    onQuickSearchChange(
      flattenValue.includes(-1),
      flattenValue.includes(-2),
      flattenValue.includes(-3),
      otherSearchId,
      personalFilters,
    );
  };

  /**
   *
   * @param value（Array）=> 选中的经办人 ID 组成的数组
   */
  handleAssigneeChange = (value) => {
    const { onAssigneeChange } = this.props;
    const flattenValue = value.map((item) => item.key);
    this.setState({
      selectUsers: value,
    });
    localPageCacheStore.mergeSetItem('scrumboard', {
      assigneeFilter: value,
    });
    onAssigneeChange(flattenValue);
  };

  handleSprintChange = (value) => {
    const { onSprintChange } = this.props;
    localPageCacheStore.mergeSetItem('scrumboard', {
      selectSprint: value,
    });
    ScrumBoardStore.setSelectSprint(value);
    onSprintChange(value && value.key);
  }

  deBounce = (delay) => {
    let timeout;
    return (fn, that) => {
      if (timeout) {
        clearTimeout(timeout);
        timeout = null;
      }
      timeout = setTimeout(fn, delay, that);// (自定义函数，延迟时间，自定义函数参数1，参数2)
    };
  };

  clearQuickSearch = () => {
    this.setState({
      selectQuickSearch: [],
      selectUsers: [],
    });
    ScrumBoardStore.setSelectSprint(undefined);
  }

  setSelectQuickSearch = (selectQuickSearch) => {
    this.setState({
      selectQuickSearch,
    });
    this.handleQuickSearchChange(selectQuickSearch);
  }

  unSelectStory = () => {
    const { selectQuickSearch } = this.state;
    const newSelect = selectQuickSearch.filter((search) => search.key !== -2);
    this.setState({
      selectQuickSearch: newSelect,
    });
    this.handleQuickSearchChange(newSelect);
  }

  render() {
    // 防抖函数
    const debounceCallback = this.deBounce(500);
    const {
      style, AppState, onAssigneeChange, quickSearchAllowClear, hideQuickSearch, onSprintChange,
    } = this.props;
    const { showRealQuickSearch } = BacklogStore;
    const {
      userDataArray,
      quickSearchArray,
      selectQuickSearch,
      selectUsers,
      personalFilter,
    } = this.state;

    const { sprintNotClosedArray, selectSprint } = ScrumBoardStore;
    // showRealQuickSearch 用于在待办事项中销毁组件
    // 具体查看 Backlog/BacklogComponent/SprintComponent/SprintItem.js 中 clearFilter 方法
    return (
      <div className="c7n-agile-quickSearch" style={style}>
        {/* <p>搜索:</p> */}
        {showRealQuickSearch ? (
          <>
            {hideQuickSearch ? null : (
              <Select
                key="quickSearchSelect"
                showCheckAll={false}
                className="SelectTheme primary"
                mode="multiple"
                labelInValue
                placeholder="快速搜索"
                style={{ ...getSelectStyle({ name: '快速搜索' }, selectQuickSearch), marginRight: 15 }}
                dropdownMatchSelectWidth={false}
                maxTagCount={0}
                maxTagPlaceholder={(ommittedValues) => `${ommittedValues.map((item) => item.label).join(', ')}`}
                onChange={this.handleQuickSearchChange}
                getPopupContainer={(triggerNode) => triggerNode.parentNode}
                allowClear
                value={selectQuickSearch}
              >
                <OptGroup key="quickSearch" label="常用选项">
                  <Option key={-1} value={-1}>仅我的问题</Option>
                  <Option key={-2} value={-2}>仅故事</Option>
                  <Option key={-3} value={-3}>我的关注</Option>
                </OptGroup>
                <OptGroup key="personal" label="我的筛选">
                  {
                    personalFilter.map((item) => (
                      <Option
                        value={`personal%${item.filterId}`}
                      >
                        {item.name}
                      </Option>
                    ))
                  }
                </OptGroup>
                <OptGroup key="more" label="更多">
                  {
                    quickSearchArray.map((item) => (
                      <Option
                        key={item.value}
                        value={item.value}
                        title={item.label}
                      >
                        {item.label}

                      </Option>
                    ))
                  }
                </OptGroup>
              </Select>
            )}
            {
              onAssigneeChange && (
                <Select
                  key="assigneeSelect"
                  showCheckAll={false}
                  mode="multiple"
                  className="SelectTheme primary"
                  style={{ ...getSelectStyle({ name: '经办人' }, selectUsers), marginRight: 15 }}
                  placeholder="经办人"
                  dropdownMatchSelectWidth={false}
                  labelInValue
                  maxTagCount={0}
                  maxTagPlaceholder={(ommittedValues) => `${ommittedValues.map((item) => item.label).join(', ')}`}
                  filter
                  optionFilterProp="children"
                  value={selectUsers}
                  onFilterChange={(value) => {
                    if (value) {
                      debounceCallback(() => {
                        userApi.getAllInProject(value, 1, undefined, 40).then((res) => {
                          // Set 用于查询是否有 id 重复的，没有重复才往里加
                          const temp = new Set(userDataArray.map((item) => item.id));
                          res.list.filter((item) => item.enabled).forEach((item) => {
                            if (!temp.has(item.id)) {
                              userDataArray.push({
                                id: item.id,
                                realName: item.realName,
                              });
                            }
                          });
                          this.setState({
                            userDataArray,
                          });
                        });
                      }, this);
                    }
                  }}
                  onChange={this.handleAssigneeChange}
                  getPopupContainer={(triggerNode) => triggerNode.parentNode}
                  allowClear
                >
                  {
                    userDataArray.length && userDataArray.map((item) => (
                      <Option
                        key={item.id}
                        value={item.id}
                        title={item.realName}
                      >
                        {item.realName}

                      </Option>
                    ))
                  }
                </Select>

              )
            }
            {
              onSprintChange && sprintNotClosedArray && (
                <Select
                  key="sprintSelect"
                  className="SelectTheme primary c7n-agile-sprintSearchSelect"
                  style={{ ...getSelectStyle({ name: '冲刺' }, selectSprint), marginRight: 15 }}
                  placeholder="冲刺"
                  allowClear
                  dropdownMatchSelectWidth={false}
                  labelInValue
                  optionFilterProp="children"
                  value={selectSprint}
                  onChange={this.handleSprintChange}
                  getPopupContainer={(triggerNode) => triggerNode.parentNode}
                >
                  {
                    (sprintNotClosedArray || []).map((item) => (
                      <Option key={item.sprintId} value={item.sprintId} title={item.sprintName}>
                        {item.sprintName}
                        {
                          item.statusCode === 'started' && (
                            <div className="c7n-agile-sprintSearchSelect-option-active">活跃</div>
                          )
                        }
                      </Option>
                    ))
                  }
                </Select>
              )
            }
          </>
        ) : (
          <>
            {hideQuickSearch ? null : <Select className="SelectTheme primary" placeholder="快速搜索" />}
            <Select className="SelectTheme primary" placeholder="经办人" />
          </>
        )}
      </div>
    );
  }
}

export { QuickSearchEvent };
export default QuickSearch;
