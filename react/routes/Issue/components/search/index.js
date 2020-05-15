/* eslint-disable camelcase */
import React, {
  useContext, useEffect, useState, Fragment,
} from 'react';
import { axios } from '@choerodon/boot';
import queryString from 'querystring';
import { withRouter } from 'react-router-dom';
import {
  Select, Icon,
} from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import {
  find, pick, isEqual, remove, map, isEmpty,
} from 'lodash';
import { observer } from 'mobx-react-lite';
import IssueStore from '@/stores/project/issue/IssueStore';
import SummaryField from './custom-fields/field/SummaryField';
import Store from '../../stores';
import CustomFields from './custom-fields';
import { getSelectStyle } from './custom-fields/utils';
import './index.less';


const { Option, OptGroup } = Select;
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
  const {
    date = [],
    date_hms = [],
    number = [],
    option = [],
    string = [],
    text = [],
  } = result;
  [...date, ...date_hms].forEach((d) => {
    result[d.fieldId] = { isCustom: true, value: [d.startDate, d.endDate] };
  });
  [...number, ...option, ...string, ...text].forEach((d) => {
    result[d.fieldId] = { isCustom: true, value: d.value };
  });

  delete result.date;
  delete result.date_hms;
  delete result.number;
  delete result.option;
  delete result.string;
  delete result.text;
  return result;
}

export default withRouter(observer(({
  urlFilter, onClear, history, location: { search },
}) => {
  const {
    prefixCls, projectId, userId,
  } = useContext(Store);

  const filters = IssueStore.getMyFilters;
  const editFilterInfo = IssueStore.getEditFilterInfo;
  const [quickFilters, setQuickFilters] = useState([]);
  const [selectedQuickFilters, setSelectedQuickFilters] = useState([]);
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
    IssueStore.loadCustomFields();
  }, []);
  const reset = () => {
    const {
      paramChoose, paramCurrentVersion, paramCurrentSprint, paramId,
      paramType, paramIssueId, paramName, paramOpenIssueId, ...otherArgs
    } = queryString.parse(search);
    setSelectedQuickFilters([]);
    if (paramIssueId || paramChoose || paramType) {
      history.replace(`/agile/work-list/issue?${queryString.stringify(otherArgs)}`);
    }
    onClear();
    IssueStore.clearAllFilter();
    IssueStore.query();
  };
  const handleSelect = (v) => {
    const { key: k } = v;
    const [type, id] = k.split('-');
    if (type === 'quick') {
      const newSelectedQuickFilters = [...selectedQuickFilters, v];
      setSelectedQuickFilters([...selectedQuickFilters, v]);
      const quickFilterIds = newSelectedQuickFilters.map(filter => filter.key.split('-')[1]);
      IssueStore.handleFilterChange('quickFilterIds', quickFilterIds);
    } else if (type === 'my') {
      const targetMyFilter = find(filters, { filterId: Number(id) });
      const filterObject = flattenObject(JSON.parse(targetMyFilter.filterJson));
      // 先清除筛选
      IssueStore.clearAllFilter();
      for (const [key, value] of Object.entries(filterObject)) {
        // 自定义字段保存的时候只保存了id，这里要找到code
        if (value.isCustom) {
          const code = IssueStore.getFieldCodeById(key);          
          if (code) {
            IssueStore.handleFilterChange(code, value.value);
          }
        } else if (key === 'createEndDate' || key === 'createStartDate') {
          IssueStore.handleFilterChange('createDate', [filterObject.createStartDate, filterObject.createEndDate]);
        } else if (key === 'updateEndDate' || key === 'updateStartDate') {
          IssueStore.handleFilterChange('updateDate', [filterObject.updateStartDate, filterObject.updateEndDate]);
        } else {
          IssueStore.handleFilterChange(key, value);
        }
      }
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
      IssueStore.clearAllFilter();
      IssueStore.query();
    }
    const quickFilterIds = selectedQuickFilters.map(filter => filter.key.split('-')[1]);
    IssueStore.handleFilterChange('quickFilterIds', quickFilterIds);
  };
  const isFilterSame = (obj, obj2) => {
    // 过滤掉 [] null '' 那些不起作用的属性
    const keys1 = Object.keys(obj).filter(k => !isEmpty(obj[k]));
    const keys2 = Object.keys(obj2).filter(k => !isEmpty(obj2[k]));
    return isEqual(pick(obj, keys1), pick(obj2, keys2));
  };
  const findSameFilter = () => {
    const currentFilterDTO = IssueStore.getCustomFieldFilters() ? flattenObject(IssueStore.getCustomFieldFilters()) : {};
    // console.log(currentFilterDTO);
    // 找到与当前筛选相同条件的我的筛选
    const targetMyFilter = find(filters, filter => isFilterSame(flattenObject(JSON.parse(filter.filterJson)), currentFilterDTO));    
    return targetMyFilter;
  };
  const isHasFilter = () => {
    const currentFilterDTO = IssueStore.getCustomFieldFilters() ? flattenObject(IssueStore.getCustomFieldFilters()) : {};
    return !isFilterSame({}, currentFilterDTO);
  };
  const getMyFilterSelectValue = () => {
    const targetMyFilter = findSameFilter();
    return targetMyFilter ? selectedQuickFilters.concat({ key: `my-${targetMyFilter.filterId}`, label: targetMyFilter.name }) : selectedQuickFilters;
  };
  const handleClickSaveFilter = () => {
    IssueStore.setSaveFilterVisible(true);
    IssueStore.setFilterListVisible(false);
    IssueStore.setEditFilterInfo(map(editFilterInfo, item => Object.assign(item, { isEditing: false })));
  };
  const handleInputChange = (value) => {
    if (value) {
      IssueStore.handleFilterChange('contents', [value]);
    } else {
      IssueStore.handleFilterChange('contents', []);
    }
    IssueStore.handleFilterChange('issueIds', []);
  };
  const renderSearch = () => (
    <Fragment>
      <div style={{ marginTop: 4 }}>
        <SummaryField
          onChange={handleInputChange}
          value={IssueStore.getFilterValueByCode('contents') ? IssueStore.getFilterValueByCode('contents')[0] : undefined}
        />
      </div>      
      <div className={`${prefixCls}-search-left`}>                
        <CustomFields>
          <div style={{ margin: '4px 5px' }}>
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
              style={{ ...getSelectStyle({ name: '快速筛选' }, getMyFilterSelectValue()), height: 34 }}
              onSelect={handleSelect}
              onDeselect={handleDeselect}
              onClear={handleDeselect}
              value={getMyFilterSelectValue()}
              getPopupContainer={triggerNode => triggerNode.parentNode}
            >
              <OptGroup key="quick" label="快速筛选">
                {quickFilters.map(filter => (
                  <Option value={`quick-${filter.filterId}`}>{filter.name}</Option>
                ))}
              </OptGroup>
              <OptGroup key="my" label="我的筛选">
                {
              filters.map(filter => (
                <Option value={`my-${filter.filterId}`}>{filter.name}</Option>
              ))
            }
              </OptGroup>
            </Select>
          </div>
        </CustomFields>
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
