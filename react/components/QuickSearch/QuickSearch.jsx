import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { Choerodon } from '@choerodon/boot';
import { Select } from 'choerodon-ui';
import EventEmitter from 'wolfy87-eventemitter';
import { sprintApi, quickFilterApi, userApi } from '@/api';
import './QuickSearch.less';
import BacklogStore from '../../stores/project/backlog/BacklogStore';
import ScrumBoardStore from '../../stores/project/scrumBoard/ScrumBoardStore';

const { Option, OptGroup } = Select;
const QuickSearchEvent = new EventEmitter();
@inject('AppState')
@observer
class QuickSearch extends Component {
  constructor(props) {
    super(props);
    this.state = {
      userDataArray: [],
      quickSearchArray: [],
      selectQuickSearch: [],
      selectUsers: [],
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
    Promise.all([axiosGetFilter, axiosGetUser, axiosGetSprintNotClosed]).then((res = []) => {
      const resFilterData = res[0].map(item => ({
        label: item.name,
        value: item.filterId,
      }));
      // 非停用角色
      const resUserData = res[1].list.filter(item => item.enabled).map(item => ({
        id: item.id,
        realName: item.realName,
      }));

      const resSprintData = res[2];

      this.setState({
        userDataArray: resUserData,
        quickSearchArray: resFilterData,
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
  handleQuickSearchChange = (value, key) => {
    const { onQuickSearchChange } = this.props;
    const flattenValue = value.map(item => item.key);
    const otherSearchId = flattenValue.filter(item => item >= 0);
    this.setState({
      selectQuickSearch: value,
    });
    // -1 仅我的问题
    // -2 仅故事
    onQuickSearchChange(flattenValue.includes(-1), flattenValue.includes(-2), otherSearchId);
  };

  /**
   *
   * @param value（Array）=> 选中的经办人 ID 组成的数组
   */
  handleAssigneeChange = (value) => {
    const { onAssigneeChange } = this.props;
    const flattenValue = value.map(item => item.key);
    this.setState({
      selectUsers: value,
    });
    onAssigneeChange(flattenValue);
  };

  handleSprintChange = (value) => {
    const { onSprintChange } = this.props;
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
    const newSelect = selectQuickSearch.filter(search => search.key !== -2);
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
    } = this.state;
    
    const { sprintNotClosedArray, selectSprint } = ScrumBoardStore;

    // showRealQuickSearch 用于在待办事项中销毁组件
    // 具体查看 Backlog/BacklogComponent/SprintComponent/SprintItem.js 中 clearFilter 方法
    return (
      <div className="c7n-agile-quickSearch" style={style}>
        {/* <p>搜索:</p> */}
        {showRealQuickSearch ? (
          <React.Fragment>
            {hideQuickSearch ? null : (
              <Select
                key="quickSearchSelect"
                showCheckAll={false}
                className="SelectTheme primary"
                mode="multiple"
                labelInValue
                placeholder="快速搜索"
                style={{ width: 100 }}
                dropdownMatchSelectWidth={false}
                maxTagCount={0}
                maxTagPlaceholder={ommittedValues => `${ommittedValues.map(item => item.label).join(', ')}`}
                onChange={this.handleQuickSearchChange}
                getPopupContainer={triggerNode => triggerNode.parentNode}
                allowClear={!!quickSearchAllowClear}
                value={selectQuickSearch}
              >
                <OptGroup key="quickSearch" label="常用选项">
                  <Option key={-1} value={-1}>仅我的问题</Option>
                  <Option key={-2} value={-2}>仅故事</Option>
                </OptGroup>
                <OptGroup key="more" label="更多">
                  {
                    quickSearchArray.map(item => (
                      <Option key={item.value} value={item.value} title={item.label}>{item.label}</Option>
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
                  style={{ width: 100 }}
                  placeholder="经办人"
                  dropdownMatchSelectWidth={false}
                  labelInValue
                  maxTagCount={0}
                  maxTagPlaceholder={ommittedValues => `${ommittedValues.map(item => item.label).join(', ')}`}
                  filter
                  optionFilterProp="children"
                  value={selectUsers}
                  onFilterChange={(value) => {
                    if (value) {
                      debounceCallback(() => {
                        userApi.getAllInProject(value, 1, undefined, 40).then((res) => {
                          // Set 用于查询是否有 id 重复的，没有重复才往里加
                          const temp = new Set(userDataArray.map(item => item.id));
                          res.list.filter(item => item.enabled).forEach((item) => {
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
                  getPopupContainer={triggerNode => triggerNode.parentNode}
                >
                  {
                    userDataArray.length && userDataArray.map(item => (
                      <Option key={item.id} value={item.id} title={item.realName}>{item.realName}</Option>
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
                // style={{ width: 120 }}
                placeholder="冲刺"
                allowClear
                dropdownMatchSelectWidth={false}
                labelInValue
                optionFilterProp="children"
                value={selectSprint}
                onChange={this.handleSprintChange}
                getPopupContainer={triggerNode => triggerNode.parentNode}
              >
                {
                  (sprintNotClosedArray || []).map(item => (
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
          </React.Fragment>
        ) : (
          <React.Fragment>
            {hideQuickSearch ? null : <Select className="SelectTheme primary" placeholder="快速搜索" />}
            <Select className="SelectTheme primary" placeholder="经办人" />
          </React.Fragment>
        )}
      </div>
    );
  }
}

export { QuickSearchEvent };
export default QuickSearch;
