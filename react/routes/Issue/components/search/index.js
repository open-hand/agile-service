import React, {
  useContext, useEffect, useState, Fragment,
} from 'react';
import { axios } from '@choerodon/boot';
import queryString from 'querystring';
import { withRouter } from 'react-router-dom';
import {
  Input, Select, DatePicker, Icon, Tooltip,
} from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import {
  find, unionBy, pick, isEqual, remove, map,
} from 'lodash';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { getRequest } from '@/common/utils';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/common/utils';
import Store from '../../stores';
import IssueStore from '../../../../stores/project/sprint/IssueStore';
import './index.less';


const { Option, OptGroup } = Select;
const { RangePicker } = DatePicker;
const filterKeys = ['issueTypeId', 'assigneeId', 'reporterIds', 'statusId', 'sprint', 'component', 'version', 'createStartDate', 'createEndDate', 'contents'];
/**
 * 对象扁平化 {a:{b:'v'}}  = >  {b:'v'}
 *
 * @param {*} object
 */
function flattenObject(object) {
  const result = {};
  for (const [key, value] of Object.entries(object)) {
    if (Object.prototype.toString.call(value) === '[object Object]') {
      Object.assign(result, flattenObject(value));
    } else {
      result[key] = value;
    }
  }
  return result;
}
let users = [];
let issueTypes = [];
let issueStatus = [];
let sprints = [];
let versions = [];
let components = [];
export default withRouter(observer(({
  urlFilter, onClear, history, location: { search },
}) => {
  const {
    dataSet, prefixCls, projectId, userId,
  } = useContext(Store);

  const filters = IssueStore.getMyFilters;
  const editFilterInfo = IssueStore.getEditFilterInfo;
  const [quickFilters, setQuickFilters] = useState([]);
  const [selectedQuickFilters, setSelectedQuickFilters] = useState([]);
  const [showMore, setShowMore] = useState(false);
  const loadFilters = async () => {
    IssueStore.axiosGetMyFilterList();
    const QuickFilters = await axios.post(`/agile/v1/projects/${projectId}/quick_filter/query_all`, {
      contents: [],
      filterName: '',
    });
    setQuickFilters(QuickFilters);
  };

  useEffect(() => {
    loadFilters();
  }, []);
  const reset = () => {
    const {
      paramChoose, paramCurrentVersion, paramCurrentSprint, paramId,
      paramType, paramIssueId, paramName, paramOpenIssueId, ...otherArgs
    } = getRequest(search);
    setSelectedQuickFilters([]);
    if (paramIssueId || paramChoose || paramType) {
      history.replace(`/agile/work-list/issue?${queryString.stringify(otherArgs)}`);
    }
    onClear();
    dataSet.queryDataSet.current.clear();
    // dataSet.query();
  };
  const handleSelect = (v) => {
    const { key: k } = v;
    const [type, id] = k.split('-');
    if (type === 'quick') {
      const newSelectedQuickFilters = [...selectedQuickFilters, v];
      setSelectedQuickFilters([...selectedQuickFilters, v]);
      const quickFilterIds = newSelectedQuickFilters.map(filter => filter.key.split('-')[1]);
      dataSet.queryDataSet.current.set('quickFilterIds', quickFilterIds);
    } else if (type === 'my') {
      const targetMyFilter = find(filters, { filterId: Number(id) });
      const filterObject = flattenObject(JSON.parse(targetMyFilter.filterJson));
      // 自动展开
      if (filterObject.component || filterObject.version || filterObject.createEndDate || filterObject.createStartDate) {
        setShowMore(true);
      }
      // 先清除筛选
      dataSet.queryDataSet.current.clear();
      dataSet.queryDataSet.current.set(filterObject);
    }
  };
  const handleDeselect = (v) => {
    // clear
    if (!v) {
      setSelectedQuickFilters([]);
      reset();
      return;
    }
    const { key } = v;
    const [type, id] = key.split('-');
    if (type === 'quick') {
      remove(selectedQuickFilters, { key });
      setSelectedQuickFilters([...selectedQuickFilters]);
    } else if (type === 'my') {
      dataSet.queryDataSet.current.clear();
    }
    const quickFilterIds = selectedQuickFilters.map(filter => filter.key.split('-')[1]);
    dataSet.queryDataSet.current.set('quickFilterIds', quickFilterIds);
  };
  const isFilterSame = (obj, obj2) => {
    // 过滤掉 [] null '' 那些不起作用的属性
    const keys1 = Object.keys(obj).filter(k => obj[k] && obj[k].length > 0);
    const keys2 = Object.keys(obj2).filter(k => obj2[k] && obj2[k].length > 0);

    return isEqual(pick(obj, keys1), pick(obj2, keys2));
  };
  const findSameFilter = () => {
    const currentFilterDTO = dataSet.queryDataSet.current ? pick(dataSet.queryDataSet.current.toData(), filterKeys) : {};
    // 找到与当前筛选相同条件的我的筛选
    const targetMyFilter = find(filters, filter => isFilterSame(pick(flattenObject(JSON.parse(filter.filterJson)), filterKeys), currentFilterDTO));
    return targetMyFilter;
  };
  const isHasFilter = () => {
    const currentFilterDTO = dataSet.queryDataSet.current ? pick(dataSet.queryDataSet.current.toData(), filterKeys) : {};
    return !isFilterSame({}, currentFilterDTO);
  };
  const getMyFilterSelectValue = () => {
    const targetMyFilter = findSameFilter();
    return targetMyFilter ? selectedQuickFilters.concat({ key: `my-${targetMyFilter.filterId}`, label: targetMyFilter.name }) : selectedQuickFilters;
  };
  const handleFilterChange = field => (value) => {
    dataSet.queryDataSet.current.set(field, value);
  };
  const handleDateTimeChange = (values) => {
    const [createStartDate, createEndDate] = values;
    dataSet.queryDataSet.current.set('createStartDate', createStartDate ? moment(createStartDate).startOf('day').format('YYYY-MM-DD HH:mm:ss') : '');
    dataSet.queryDataSet.current.set('createEndDate', createEndDate ? moment(createEndDate).endOf('day').format('YYYY-MM-DD HH:mm:ss') : '');
  };
  const getValue = key => (dataSet.queryDataSet.current ? dataSet.queryDataSet.current.toData()[key] : undefined);
  const getSelectedDate = () => {
    const createStartDate = getValue('createStartDate');
    const createEndDate = getValue('createEndDate');
    if (!createEndDate || !createStartDate) {
      return [];
    }
    return [moment(createStartDate), moment(createEndDate)];
  };
  const handleInputChange = (e) => {
    if (e.target.value) {
      dataSet.queryDataSet.current.set('contents', [e.target.value]);
    } else {
      dataSet.queryDataSet.current.set('contents', []);
    }
    dataSet.queryDataSet.current.set('issueIds', []);
  };
  const handlePressEnter = () => {
    dataSet.query();
  };
  const handleClickSaveFilter = () => {
    IssueStore.setSaveFilterVisible(true);
    IssueStore.setFilterListVisible(false);
    IssueStore.setEditFilterInfo(map(editFilterInfo, item => Object.assign(item, { isEditing: false })));
  };
  const shouldShowMore = showMore;
  const renderSearch = () => (
    <Fragment>
      <div className={`${prefixCls}-search-left`}>
        <div className={`${prefixCls}-search-left-row`}>
          <Input
            onChange={handleInputChange}
            value={getValue('contents') ? getValue('contents')[0] : undefined}
            onPressEnter={handlePressEnter}
            className="hidden-label"
            prefix={<Icon type="search" style={{ color: 'rgba(0, 0, 0, 0.45)', marginLeft: 2 }} />}
            style={{ width: 180, marginRight: 5 }}
            placeholder="请输入搜索内容"
            label={false}
          />
          <Select
            mode="multiple"
            showCheckAll={false}
            allowClear
            className="SelectTheme"
            dropdownMatchSelectWidth={false}
            placeholder="快速筛选"
            maxTagCount={0}
            labelInValue
            maxTagPlaceholder={ommittedValues => `${ommittedValues.map(item => item.label).join(', ')}`}
            style={{ width: 120, margin: '0 5px' }}
            onSelect={handleSelect}
            onDeselect={handleDeselect}
            onClear={handleDeselect}
            value={getMyFilterSelectValue()}
            getPopupContainer={triggerNode => triggerNode.parentNode}
          >
            <OptGroup label="快速筛选">
              {quickFilters.map(filter => (
                <Option value={`quick-${filter.filterId}`}>{filter.name}</Option>
              ))}
            </OptGroup>
            <OptGroup label="我的筛选">
              {
                filters.map(filter => (
                  <Option value={`my-${filter.filterId}`}>{filter.name}</Option>
                ))
              }
            </OptGroup>
          </Select>
          <SelectFocusLoad
            {...configTheme({
              list: issueTypes,
              textField: 'name',
              valueFiled: 'id',
            })}
            type="issue_type"
            loadWhenMount
            style={{ width: 120, margin: '0 5px' }}
            mode="multiple"
            showCheckAll={false}
            allowClear
            dropdownMatchSelectWidth={false}
            placeholder="问题类型"
            saveList={(v) => { issueTypes = unionBy(issueTypes, v, 'id'); }}
            filter={false}
            onChange={handleFilterChange('issueTypeId')}
            value={getValue('issueTypeId')}
            getPopupContainer={triggerNode => triggerNode.parentNode}
          />

          <SelectFocusLoad
            {...configTheme({
              list: issueStatus,
              textField: 'name',
              valueFiled: 'id',
            })}
            type="issue_status"
            showCheckAll={false}
            loadWhenMount
            style={{ width: 82, margin: '0 5px' }}
            mode="multiple"
            allowClear
            dropdownMatchSelectWidth={false}
            placeholder="状态"
            saveList={(v) => { issueStatus = unionBy(issueStatus, v, 'id'); }}
            filter={false}
            onChange={handleFilterChange('statusId')}
            value={getValue('statusId')}
            getPopupContainer={triggerNode => triggerNode.parentNode}
          />
          <SelectFocusLoad
            {...configTheme({
              list: users.concat({ id: 0, realName: '未分配' }),
              textField: 'realName',
              valueFiled: 'id',
              parseNumber: true,
            })}
            type="user"
            loadWhenMount
            key="assigneeSelect"
            style={{ width: 96, margin: '0 5px' }}
            mode="multiple"
            showCheckAll={false}
            allowClear
            dropdownMatchSelectWidth={false}
            placeholder="经办人"
            saveList={(v) => { users = unionBy(users, v, 'id'); }}
            filter
            onChange={handleFilterChange('assigneeId')}
            value={getValue('assigneeId')}
            getPopupContainer={triggerNode => triggerNode.parentNode}
            render={user => <Option value={user.id}>{user.realName || user.loginName}</Option>}
          >
            <Option value="0">未分配</Option>
          </SelectFocusLoad>
          <SelectFocusLoad
            {...configTheme({
              list: users,
              textField: 'realName',
              valueFiled: 'id',
              parseNumber: true,
            })}
            type="user"
            loadWhenMount
            key="reporterSelect"
            style={{ width: 96, margin: '0 5px' }}
            mode="multiple"
            showCheckAll={false}
            allowClear
            dropdownMatchSelectWidth={false}
            placeholder="报告人"
            saveList={(v) => { users = unionBy(users, v, 'id'); }}
            filter
            onChange={handleFilterChange('reporterIds')}
            value={getValue('reporterIds')}
            getPopupContainer={triggerNode => triggerNode.parentNode}
            render={user => <Option value={user.id}>{user.realName || user.loginName}</Option>}
          />
          <Button funcType="flat" color="blue" style={{ marginLeft: 'auto' }} onClick={() => { setShowMore(!shouldShowMore); }}>{shouldShowMore ? '收起' : '更多'}</Button>
        </div>
        <div className={`${prefixCls}-search-left-row ${shouldShowMore ? `${prefixCls}-search-left-row-more` : `${prefixCls}-search-left-row-hidden`}`}>
          <SelectFocusLoad
            {...configTheme({
              list: sprints,
              textField: 'sprintName',
              valueFiled: 'sprintId',
              parseNumber: true,
            })}
            type="sprint"
            loadWhenMount
            key="reporterSelect"
            style={{ width: 120, margin: '0 5px' }}
            mode="multiple"
            showCheckAll={false}
            allowClear
            dropdownMatchSelectWidth={false}
            placeholder="冲刺"
            saveList={(v) => { sprints = unionBy(sprints, v, 'sprintId'); }}
            filter
            onChange={handleFilterChange('sprint')}
            value={getValue('sprint')}
            getPopupContainer={triggerNode => triggerNode.parentNode}
          />
          <SelectFocusLoad
            {...configTheme({
              list: components,
              textField: 'name',
              valueFiled: 'componentId',
              parseNumber: true,
            })}
            type="component"
            loadWhenMount            
            style={{ width: 120, margin: '0 5px' }}
            mode="multiple"
            showCheckAll={false}
            allowClear
            dropdownMatchSelectWidth={false}
            placeholder="模块"
            saveList={(v) => { components = unionBy(components, v, 'componentId'); }}
            filter
            onChange={handleFilterChange('component')}
            value={getValue('component')}
            getPopupContainer={triggerNode => triggerNode.parentNode}
            render={c => (
              <Option        
                value={c.componentId}
              >
                {c.name}
              </Option>
            )}
          />
          <SelectFocusLoad
            {...configTheme({
              list: versions,
              textField: 'name',
              valueFiled: 'versionId',
              parseNumber: true,
            })}
            type="version"
            loadWhenMount            
            style={{ width: 82, margin: '0 5px' }}
            mode="multiple"
            showCheckAll={false}
            allowClear
            dropdownMatchSelectWidth={false}
            placeholder="版本"
            saveList={(v) => { versions = unionBy(versions, v, 'versionId'); }}
            filter
            onChange={handleFilterChange('version')}
            value={getValue('version')}
            getPopupContainer={triggerNode => triggerNode.parentNode}
          />
          {
            getSelectedDate().length > 0 ? (
              <Tooltip title={`创建问题时间范围为${getSelectedDate()[0].format('YYYY-MM-DD')} ~  ${getSelectedDate()[1].format('YYYY-MM-DD')}`}>
                <div>
                  <RangePicker
                    format="YYYY-MM-DD"
                    value={getSelectedDate()}
                    className="RangePickerTheme"
                    style={{ margin: '0 5px', width: 205 }}
                    onChange={handleDateTimeChange}
                    allowClear
                    placeholder={['创建时间', '']}
                  />
                </div>
              </Tooltip>
            ) : (
              <RangePicker
                format="YYYY-MM-DD"
                value={getSelectedDate()}
                className="RangePickerTheme"
                style={{ margin: '0 5px', width: 205 }}
                onChange={handleDateTimeChange}
                allowClear
                placeholder={['创建时间', '']}
              />
            )
          }
        </div>

      </div>
      <div className={`${prefixCls}-search-right`}>
        {isHasFilter() && <Button onClick={reset} funcType="flat" color="blue">重置</Button>}
        {!findSameFilter() && isHasFilter() && <Button onClick={handleClickSaveFilter} funcType="raised" color="blue">保存筛选</Button>}
      </div>
    </Fragment>
  );
  const renderUrlFilter = () => (
    <Fragment>
      <div className={`${prefixCls}-search-left`}>
        <div className={`${prefixCls}-search-urlFilter`}>
          <Icon type="search" />
          <div className={`${prefixCls}-search-urlFilter-item`}>{urlFilter}</div>
        </div>
      </div>
      <div className={`${prefixCls}-search-right`}>
        <Button onClick={reset} funcType="flat" color="blue">重置</Button>
      </div>
    </Fragment>
  );
  return (
    <div className={`${prefixCls}-search`}>
      {urlFilter ? renderUrlFilter() : renderSearch()}
    </div>
  );
}));
